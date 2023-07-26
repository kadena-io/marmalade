# Quote Manager

The Quote Manager is a module that saves the quotes in marmalade's sales, and are called from `marmalade.policy-manager`

This module includes the following key components:

## Quote Manager Schema

**QUOTE SCHEMA**: The `quote` schema contains the quote information of a sale, and is used to save the quote information in the `quotes` table. It consists of `token-id`, `spec`, `seller-guard`, and `quote-guards`

**QUOTE-MSG SCHEMA**: The `quote-msg` schema is the information required to provide in `env-data` field at `offer` step. It consists of `spec`, `seller-guard`, `quote-guards`

**QUOTE-SPEC SCHEMA**: The `quote-spec` schema contains fungible information about the sale, and is part of `quote` schema. It consists of `fungible`, `seller-account`, `price`, `amount`.

## Quote Manager Functions

Main functions of quote manager includes

- `add-quote`: Takes in `sale-id`, `token-id`, and `quote-msg`.
  Adds the quote into the quotes table. This function is required to be called from the `policy-manager`.

- `optional-add-quote`: Takes in `sale-id`, and `token-id`.
  Reads the key for `QUOTE-MSG-KEY` in data field of the transaction, and optionally calls `add-quote` with the `QUOTE_MSG` if it exists. If `QUOTE-MSG-KEY` does not exists, skip.

- `update-quote-price`: Takes in `sale-id` and `price`.
  Updates the `quote`'s price with the provided `price` argument. This function is guarded by one of the `quote-guards`, and is required to be called from the `policy-manager`. In the policy-manager, the function is called within `enforce-buy`.

- `optional-update-quote-price`: Takes in `sale-id`
  Reads for `UPDATE-QUOTE-PRICE-MSG-KEY` and optionally calls `update-quote-price` if `UPDATE_QUOTE_PRICE_MSG_KEY` exists. If the key does not exist, skip

- `update-quote-guards`: Takes in `sale-id` and `quote-guards`.
  Updates the `quote-guards` field in the `quotes` table. This function is built to be directly called from the `quote-manager`, and is guarded by the `seller-guard`.
