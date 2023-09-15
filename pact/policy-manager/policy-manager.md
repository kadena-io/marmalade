# Policy Manager

The Policy Manager is a module that maps that maps the function calls of individual policies of the token. With the upgrade of marmalade V2, token creators now can select multiple policies to program the tokens. These policies can include rules for creation, mints, transfers, burns, sales, and more, providing a flexible and customizable way to manage the behavior of tokens on the platform. The token policies is built using the `kip.token-policy-v2` interface, which defines the standard interface that token policies must implement.

## Policy Manager and Quotes

Another main feature that policy manager allows is having one standard for collecting and distributing the fungibles. Previously in marmalade v1, `fixed-quote-policy` was an example of handling the fungible transfers at sales, and in marmalade v2, the feature is optionally supported for every tokens from policy-manager.

### Offer
Seller can choose to make a quoted offer by adding `quote` information in transaction data. If not present, the sale will proceed without quotes. Note that individual policies can enforce quoted sale by adding their logic in the code. |

When an offer is made with quotes, the policy manager adds the quotes with `quote-manager.add-quote` function. Quote has additional fields, `quote-guards` and `seller-guard`.
`quote-guards` represent the list of guards that the seller grants the authority to update the quote price. `seller-guard` represents the seller's guard, which can add or remove the guards from the `quote-guards` list.


### Reserve sale at price

In between `offer` and `buy`, we provide a function, `reserve-sale-at-price`. The purpose of the function is to update the quote price and reserve the `buyer` information. This is only necessary when the quote guard is added as one of the quote guards and would like to process the sale with quote price different than initial quote price.

The function collects the fungible from the `quote-account`, and reserves the `buyer` information, which will be enforced in the `enforce-buy`.

#### Quote Guards

Quote guards are a list of guards that have ability to call `reserve-sale-at-price`. The seller adds the quote guards at `offer`, and also have the ability to add or remove quote guards after making the offer, which would be analogous to participating in multiple auctions simultaneously. If the seller wants the quote price to be unchanged, the quote guards field can be left as an empty list.

Note that the `offer` is still valid to be bought directly by the buyer, even if the quote guards are present. The seller can block the direct `buy`'s by setting the quote price at `0.0`, which will only allow `buy`'s through `reserve-sale-at-price` with updated quote price.


### Buy
There are 3 different conditions that the `enforce-buy` runs
  1. Sale was processed without quotes: the function doesn't do more than running policy functions.
  2. Sale was reserved: Escrow account already collected fungibles, and so escrow account distributes the collected fungible from the `reserve-sale-at-price` to the seller and the required policies.
  3. Sale was processed with quotes and not reserved:  `buyer-fungible-account` needs to transfer fungible to the escrow account, and the escrow account distributes to the seller and required policies.

##  Components

This module includes the following key components:

**Ledgers Table**: There are functions that limits their call to be initiated from the ledger. We create a ledgers table, so we can save the module reference to the ledger, and add enforcement that the function cannot be called otherwise.

**Concrete Policies Table**: A table, `concrete-policies` is created to store the concrete policy information for each token policy. A Concrete policy can be viewed as a default policy provided by the marmalade team. Its a standardised set of policies to get token creators started. This table ensures proper handling of policy configurations for the NFTs managed under the policy.

**Ledger Schema**: The `ledger` schema contains the module reference of the ledger that the policy manager is governed by. We strictly limit the module to use the interface, `ledger-v1`.

**Concrete Policies Schema:**: The `concrete-policies` schema contains the module reference. We strictly limit the module to use the interface, `token-policy-v2`

**Capabilities**
 - `GOVERNANCE`
 - `CONCRETE-POLICY` @event
 - `ESCROW`
 - `MAP-ESCROWED-BUY`
 - `OFFER`
 - `BUY`
 - `WITHDRAW`
 - `RESERVE-SALE-AT-PRICE` @event
 - `INIT-CALL`
 - `TRANSFER-CALL`
 - `MINT-CALL`
 - `BURN-CALL`
 - `OFFER-CALL`
 - `WITHDRAW-CALL`
 - `BUY-CALL`
 - `ADD-QUOTE-CALL`
 - `UPDATE-QUOTE-PRICE-CALL`

## Policy Functions

`enforce-**` functions requires the `ledger::**-CALL` capability to be in scope, which enforces that the function is started from the ledger with the scoped parameters. It then retrieves the `policies` list from the `token` input parameter, which contains a list of policies associated with the token.

`enforce-init`: Runs `policies::enforce-init` at `marmalade-v2.ledger.create-token`.
`enforce-mint`: Runs `policies::enforce-mint` at `marmalade-v2.ledger.mint`.
`enforce-burn`: Runs `policies::enforce-burn` at `marmalade-v2.ledger.burn`.
`enforce-offer`: Runs `policies::enforce-offer` at `marmalade-v2.ledger.offer`(step 0 of `marmalade-v2.ledger.sale`). There is an optional parameter `quote`, which is read in `env-data` field of the transaction. If `quote` is present, the offer saves quote with the sale, and creates the escrow accounts. If not, offer proceeds without quotes.
`enforce-withdraw`: Runs `policies::enforce-withdraw` at `marmalade-v2.ledger.withdraw` (step 1 rollback of `marmalade-v2.ledger.sale`). If the offer is already reserved, `enforce-withdraw` fails.
`enforce-buy`: Runs `policies::enforce-buy` at `marmalade-v2.ledger.buy` (step 1 of `marmalade-v2.ledger.sale`).
    There are 3 different conditions that the `enforce-buy` runs
    1. Sale was processed without quotes: the function doesn't do more than running policy functions.
    2. Sale was reserved: Escrow account already collected fungibles, and so escrow account distributes the collected fungible from the `reserve-sale-at-price` to the seller and the required policies.
    3. Sale was processed with quotes and not reserved:  `buyer-fungible-account` needs to transfer fungible to the escrow account, and the escrow account distributes to the seller and required policies.
  - Required Capability (if quoted, non-reserved)
    - Capability: `(fungible::TRANSFER buyer-fungible-account escrow-account sale-price)`
    - Signer: buyer-fungible-account

`reserve-sale-at-price`: Updates the quote price and reserves the sale, and the marmalade buyer account information. Fungible payment is required. After this step, anyone can process `marmalade-v2.ledger.buy` step.
  - Required Capability
    - Capability: `(RESERVE-SALE-AT-PRICE sale-id price buyer buyer-guard)`
    - Signer: One of the `quote-guards`
    - Capability: `(fungible::TRANSFER buyer-fungible-account escrow-account sale-price)`
    - Signer: buyer-fungible-account

`map-escrowed-buy`: Helper function inside the `enforce-buy` to process quoted sales.

`get-escrow-account`: Returns the fungible escrow account created for quoted sales at `enforce-offer`. The escrow account receives the fungible from the buyer and distributes the fungibles to the policies and the seller at `buy` step.

`write-concrete-policy`: Registers concrete policy modref into the concrete-policies table.
  - Required Capability
    - Capability: `(CONCRETE-POLICY policy-field policy)`
    - Signer: (keyset-ref-guard `marmalade-v2.marmalade-admin`)

`get-concrete-policy`: Returns the modref of the concrete policy.

`enforce-sale-pact`: Ensures that the `sale` parameter provided to the function is equal to the ID of the currently executing pact. It does this by calling the `pact-id` function to retrieve the ID of the currently executing pact and comparing it to the provided `sale` parameter. If they are not equal, an exception will be thrown".
