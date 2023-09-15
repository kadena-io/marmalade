# Quote Manager

The Quote Manager is a module that saves the quotes in marmalade's sales, and are called from `marmalade.policy-manager`

This module includes the following key components:

## Quote Manager Schema

**QUOTE SCHEMA**: The `quote` schema contains the quote information of a sale, and is used to save the quote information in the `quotes` table. It consists of `token-id`, `spec`, `seller-guard`, and `quote-guards`

**QUOTE-MSG SCHEMA**: The `quote-msg` schema is the information required to provide in `env-data` field at `offer` step. It consists of `spec`, `seller-guard`, `quote-guards`

**QUOTE-SPEC SCHEMA**: The `quote-spec` schema contains fungible information about the sale, and is part of `quote` schema. It consists of `fungible`, `seller-account`, `price`, `amount`.

**Capabilities**
  - `GOVERNANCE`
  - `UPDATE-QUOTE-GUARD`
  - `UPDATE-QUOTE-PRICE`
  - `QUOTE` @event
  - `QUOTE-GUARDS` @event

## Quote Manager Functions

Main functions of quote manager includes

- `add-quote`: Takes in `sale-id`, `token-id`, and `quote-msg`. Adds the quote into the quotes table. This function is required to be called from the `policy-manager`.

- `update-quote-price`: Takes in `sale-id` and `price`, `buyer`. Updates the `quote`'s price with the provided `price` argument. This function is guarded by one of the `quote-guards`, and is required to be called from the `policy-manager`. In the policy-manager, the function is called within `reserve-sale-at-price`.
  - Required Capability
    - Capbility: `(UPDATE_QUOTE_PRICE sale-id price buyer)`
    - Signer: One of the `quote-guards`. Installed inside `policy-manager.RESERVE_SALE_AT_PRICE`.

- `remove-quote-guard`: Removes a quote guard from the `quote-guards` list registered with the quotes in the quotes table.
  - Required Capability
    - Capbility: `(UPDATE_QUOTE_GUARD sale-id)`
    - Signer: seller guard registered in the quote

- `add-quote-guard`: Adds a quote guard to the `quote-guards`
  - Required Capability
    - Capbility: `(UPDATE_QUOTE_GUARD sale-id)`
    - Signer: seller guard registered in the quote

- `get-quote-info`: Returns the up to date quote information of the sale.
