// Copyright 2022 IOTA Stiftung
// SPDX-License-Identifier: Apache-2.0

use std::cell::RefCell;

use crate::{
    account::{
        handle::AccountHandle,
        operations::transfer::{high_level::minimum_storage_deposit::minimum_storage_deposit_foundry, TransferResult},
        TransferOptions,
    },
    Error,
};

use iota_client::bee_message::{
    address::AliasAddress,
    output::{
        unlock_condition::{ImmutableAliasAddressUnlockCondition, UnlockCondition},
        AliasOutputBuilder, BasicOutputBuilder, FoundryId, FoundryOutputBuilder, NativeToken, NftOutputBuilder, Output,
        SimpleTokenScheme, TokenId, TokenScheme, OUTPUT_COUNT_MAX,
    },
};
use primitive_types::U256;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
/// Address, token ID and amount for `burn_native_token()`
#[serde(rename_all = "camelCase")]
pub struct BurnNativeTokenOptions {
    /// Native token
    pub native_token: (TokenId, U256),
}

impl AccountHandle {
    /// Function to burn native tokens with foundry
    pub async fn burn_native_token(
        &self,
        burn_native_token_options: BurnNativeTokenOptions,
        options: Option<TransferOptions>,
    ) -> crate::Result<TransferResult> {
        log::debug!("[TRANSFER] burn_native_token");
        let byte_cost_config = self.client.get_byte_cost_config().await?;

        let token_id = burn_native_token_options.native_token.0;
        let burn_token_amount = burn_native_token_options.native_token.1;

        let foundry_id = token_id.foundry_id();
        let alias_id = *foundry_id.alias_address().alias_id();

        let account = self.read().await;

        let mut existing_alias_output_data = None;
        let mut existing_foundry_output = None;
        account
            .unspent_outputs()
            .values()
            .into_iter()
            .filter(|output_data| match &output_data.output {
                Output::Alias(output) => output.alias_id().or_from_output_id(output_data.output_id) == alias_id,
                Output::Foundry(output) => output.id() == foundry_id,
                _ => false,
            })
            .for_each(|output_data| match &output_data.output {
                Output::Alias(_) => existing_alias_output_data = Some(output_data),
                Output::Foundry(output) => existing_foundry_output = Some(output),
                _ => unreachable!("We checked if it's an alias or foundry output before"),
            });

        let existing_alias_output_data = existing_alias_output_data
            .ok_or_else(|| Error::BurningFailed("Required alias output for token id not found".to_string()))?
            .clone();

        let existing_foundry_output = existing_foundry_output
            .ok_or_else(|| Error::BurningFailed("Required foundry output for token id not found".to_string()))?
            .clone();

        drop(account);

        if let Output::Alias(alias_output) = &existing_alias_output_data.output {
            // Amount can't be burned, only native tokens
            let amount = existing_alias_output_data.amount + existing_foundry_output.amount();
            // Create the new alias output with the same feature blocks, just updated state_index
            let mut new_alias_output_builder = AliasOutputBuilder::new(amount, alias_id)?
                .with_state_index(alias_output.state_index() + 1)
                .with_foundry_counter(alias_output.foundry_counter());

            for unlock_condition in alias_output.unlock_conditions().clone().into_iter() {
                new_alias_output_builder = new_alias_output_builder.add_unlock_condition(unlock_condition);
            }

            for feature_block in alias_output.feature_blocks().iter() {
                new_alias_output_builder = new_alias_output_builder.add_feature_block(feature_block.clone());
            }
            for immutable_feature_block in alias_output.immutable_feature_blocks().iter() {
                new_alias_output_builder =
                    new_alias_output_builder.add_immutable_feature_block(immutable_feature_block.clone());
            }

            let TokenScheme::Simple(foundry_simple_ts) = existing_foundry_output.token_scheme();
            let outputs = vec![
                Output::Alias(new_alias_output_builder.finish()?),
                Output::Foundry(
                    FoundryOutputBuilder::new(
                        minimum_storage_deposit_foundry(&byte_cost_config)?,
                        foundry_id.serial_number(),
                        token_id.token_tag(),
                        TokenScheme::Simple(SimpleTokenScheme::new(
                            *foundry_simple_ts.minted_tokens(),
                            foundry_simple_ts.melted_tokens() + burn_token_amount,
                            *foundry_simple_ts.maximum_supply(),
                        )?),
                    )?
                    .add_native_token(NativeToken::new(
                        token_id,
                        foundry_simple_ts.circulating_supply() - burn_token_amount,
                    )?)
                    .add_unlock_condition(UnlockCondition::ImmutableAliasAddress(
                        ImmutableAliasAddressUnlockCondition::new(AliasAddress::from(alias_id)),
                    ))
                    .finish()?,
                ),
            ];
            self.send(outputs, options).await
        } else {
            unreachable!("We checked if it's an alias output before")
        }
    }

    /// Function to burn native tokens without foundry
    pub async fn burn_native_token_without_foundry(
        &self,
        burn_native_token_options: BurnNativeTokenOptions,
        options: Option<TransferOptions>,
    ) -> crate::Result<TransferResult> {
        log::debug!("[TRANSFER] burn_native_token_without_foundry");
        let token_id = burn_native_token_options.native_token.0;
        let burn_token_amount = burn_native_token_options.native_token.1;

        let account = self.read().await;
        let mut inputs_and_outputs = Vec::new();
        for (output_id, output_data) in account.unspent_outputs().iter() {
            if let Some(native_tokens) = output_data.output.native_tokens() {
                let amount = RefCell::new(U256::from(0));
                let not_to_be_burnt_native_tokens = native_tokens
                    .iter()
                    .filter(|native_token| {
                        if *native_token.token_id() == token_id {
                            amount.replace_with(|current| *current + *native_token.amount());
                            return false;
                        }
                        true
                    })
                    .cloned();

                // If the output has a native token that we wish to burn,
                // clone the output but without native tokens that are to be burnt
                if !amount.borrow().is_zero() {
                    let output = AccountHandle::rebuild_output_with_native_tokens(
                        &output_data.output,
                        not_to_be_burnt_native_tokens,
                    )?;
                    inputs_and_outputs.push((output_id, *amount.borrow(), output));
                }
            }
        }

        // Sort descending order
        inputs_and_outputs.sort_by(|a, b| b.1.cmp(&a.1));

        // Select unspent outputs with token id that sum up to the required amount
        let mut outputs = Vec::new();
        let mut custom_inputs = Vec::new();
        let mut native_token_amount_accumulator = U256::from(0);
        for input_and_output in inputs_and_outputs.into_iter().take(OUTPUT_COUNT_MAX as usize) {
            if native_token_amount_accumulator >= burn_token_amount {
                break;
            }
            outputs.push(input_and_output.2);
            custom_inputs.push(*input_and_output.0);
            native_token_amount_accumulator += input_and_output.1;
        }

        drop(account);

        let options = match options {
            Some(mut options) => {
                options.custom_inputs.replace(custom_inputs);
                Some(options)
            }
            None => Some(TransferOptions {
                custom_inputs: Some(custom_inputs),
                ..Default::default()
            }),
        };

        // TODO: Avoid or work on `try_select_inputs` to make it burn native tokens rather than have them put in remainder
        self.send(outputs, options).await
    }

    /// Function to destroy foundry
    pub async fn destroy_foundry(
        &self,
        foundry_id: FoundryId,
        options: Option<TransferOptions>,
    ) -> crate::Result<TransferResult> {
        log::debug!("[TRANSFER] destroy_foundry");

        let alias_id = *foundry_id.alias_address().alias_id();
        let account = self.read().await;

        let mut existing_alias_output_data = None;
        let mut existing_foundry_output = None;
        account
            .unspent_outputs()
            .values()
            .into_iter()
            .filter(|output_data| match &output_data.output {
                Output::Alias(output) => output.alias_id().or_from_output_id(output_data.output_id) == alias_id,
                Output::Foundry(output) => output.id() == foundry_id,
                _ => false,
            })
            .for_each(|output_data| match &output_data.output {
                Output::Alias(_) => existing_alias_output_data = Some(output_data),
                Output::Foundry(_) => existing_foundry_output = Some(output_data),
                _ => unreachable!("We checked if it's an alias or foundry output before"),
            });

        let existing_alias_output_data = existing_alias_output_data
            .ok_or_else(|| Error::BurningFailed("Required alias output for foundry not found".to_string()))?
            .clone();

        let existing_foundry_output_data = existing_foundry_output
            .ok_or_else(|| Error::BurningFailed("Required foundry output not found".to_string()))?
            .clone();

        drop(account);

        let custom_inputs = vec![
            existing_alias_output_data.output_id,
            existing_foundry_output_data.output_id,
        ];
        let options = match options {
            Some(mut options) => {
                options.custom_inputs.replace(custom_inputs);
                Some(options)
            }
            None => Some(TransferOptions {
                custom_inputs: Some(custom_inputs),
                ..Default::default()
            }),
        };

        let outputs = match existing_alias_output_data.output {
            Output::Alias(alias_output) => {
                // Amount can't be burned, only native tokens
                let amount = existing_alias_output_data.amount + existing_foundry_output_data.amount;
                // Create the new alias output with the same feature blocks, just updated state_index
                let mut new_alias_output_builder = AliasOutputBuilder::new(amount, alias_id)?
                    .with_state_index(alias_output.state_index() + 1)
                    .with_foundry_counter(alias_output.foundry_counter());

                for unlock_condition in alias_output.unlock_conditions().clone().into_iter() {
                    new_alias_output_builder = new_alias_output_builder.add_unlock_condition(unlock_condition);
                }

                for feature_block in alias_output.feature_blocks().iter() {
                    new_alias_output_builder = new_alias_output_builder.add_feature_block(feature_block.clone());
                }
                for immutable_feature_block in alias_output.immutable_feature_blocks().iter() {
                    new_alias_output_builder =
                        new_alias_output_builder.add_immutable_feature_block(immutable_feature_block.clone());
                }

                vec![Output::Alias(new_alias_output_builder.finish()?)]
            }
            _ => unreachable!("We checked if it's an alias output before"),
        };

        self.send(outputs, options).await
    }

    // This should preferably be done in the bee crate to make sure that
    // potentially new output fields are not left out while cloning
    pub(crate) fn rebuild_output_with_native_tokens(
        output: &Output,
        native_tokens: impl IntoIterator<Item = NativeToken>,
    ) -> crate::Result<Output> {
        let output = match output {
            Output::Basic(basic_output) => Output::Basic(
                BasicOutputBuilder::new(basic_output.amount())?
                    .with_native_tokens(native_tokens)
                    .with_unlock_conditions(basic_output.unlock_conditions().iter().cloned())
                    .with_feature_blocks(basic_output.feature_blocks().iter().cloned())
                    .finish()?,
            ),
            Output::Alias(alias_output) => Output::Alias(
                AliasOutputBuilder::new(alias_output.amount(), *alias_output.alias_id())?
                    .with_native_tokens(native_tokens)
                    .with_state_index(alias_output.state_index())
                    .with_state_metadata(alias_output.state_metadata().to_vec())
                    .with_foundry_counter(alias_output.foundry_counter())
                    .with_unlock_conditions(alias_output.unlock_conditions().iter().cloned())
                    .with_feature_blocks(alias_output.feature_blocks().iter().cloned())
                    .with_immutable_feature_blocks(alias_output.immutable_feature_blocks().iter().cloned())
                    .finish()?,
            ),
            Output::Foundry(foundry_output) => Output::Foundry(
                FoundryOutputBuilder::new(
                    foundry_output.amount(),
                    foundry_output.serial_number(),
                    *foundry_output.token_tag(),
                    foundry_output.token_scheme().clone(),
                )?
                .with_native_tokens(native_tokens)
                .with_unlock_conditions(foundry_output.unlock_conditions().iter().cloned())
                .with_feature_blocks(foundry_output.feature_blocks().iter().cloned())
                .with_immutable_feature_blocks(foundry_output.immutable_feature_blocks().iter().cloned())
                .finish()?,
            ),
            Output::Nft(nft_output) => Output::Nft(
                NftOutputBuilder::new(nft_output.amount(), *nft_output.nft_id())?
                    .with_native_tokens(native_tokens)
                    .with_unlock_conditions(nft_output.unlock_conditions().iter().cloned())
                    .with_feature_blocks(nft_output.feature_blocks().iter().cloned())
                    .with_immutable_feature_blocks(nft_output.immutable_feature_blocks().iter().cloned())
                    .finish()?,
            ),
            Output::Treasury(_) => {
                return Err(crate::Error::InvalidOutputKind(
                    "Treasury output cannot hold native tokens".to_string(),
                ))
            }
        };

        Ok(output)
    }
}
