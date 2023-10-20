# Policy Manager

The Policy Manager is a module that maps that maps the function calls of individual policies of the token. With the upgrade of marmalade V2, token creators now can select multiple policies to program the tokens. These policies can include rules for creation, mints, transfers, burns, sales, and more, providing a flexible and customizable way to manage the behavior of tokens on the platform. The token policies is built using the `kip.token-policy-v2` interface, which defines the standard interface that token policies must implement.

## Policy Manager and Quotes

Another main feature that policy manager allows is having one standard for collecting and distributing the fungibles. Previously in marmalade v1, `fixed-quote-policy` was an example of handling the fungible transfers at sales, and in marmalade v2, the feature is optionally supported for every tokens from policy-manager.

### Offer
Seller can choose to make a quoted offer by adding `quote` information in transaction data. If not present, the sale will proceed without quotes. Note that individual policies can enforce quoted sale by adding their logic in the code. |

When an offer is made with quotes, the policy manager adds the quotes in the quotes table.
Quotes are composed of the following:
- `fungible:module{fungible-v2}`: The fungible that the quote is offered with
- `seller-fungible-account:object{fungible-account}`: account and guard that will receive the fungible
- `price:decimal`: price of the offer. If set to `0.0`, price is not finalized and is required to be finalized through sale contract before `buy` can happen
- `amount:decimal`: amount of the token that was offered
- `sale-type:string`: The sale contract that will manage the quote. If set to `""`, there is no sale contract controlling the quote, and can only be bought at initially offered price.

### Buy
There are 3 different conditions that the `enforce-buy` runs
  1. Sale was processed without quotes: the function doesn't do more than running policy functions.
  2. Sale was made without `sale-type`: the function processes the initially offered quote.  
  3. Sale was made with a valid `sale-type`: the function reads optionally `updated_price` key and updates the quote if present. If initial price was set to 0.0, `updated_price` is required.

##  Components

This module includes the following key components:

**Ledgers Table**: There are functions that limits their call to be initiated from the ledger. We create a ledgers table, so we can save the module reference to the ledger, and add enforcement that the function cannot be called otherwise.

**Concrete Policies Table**: A table, `concrete-policies` is created to store the concrete policy information for each token policy. A Concrete policy can be viewed as a default policy provided by the marmalade team. Its a standardised set of policies to get token creators started. This table ensures proper handling of policy configurations for the NFTs managed under the policy.

**Sale Whitelist Table**: A table to store valid whitelisted sale contracts.

**Quotes Table**: A table to store quotes for quoted sales.

**Ledger Schema**: The `ledger` schema contains the module reference of the ledger that the policy manager is governed by. We strictly limit the module to use the interface, `ledger-v1`.

**Concrete Policies Schema:**: The `concrete-policies` schema contains the module reference. We strictly limit the module to use the interface, `token-policy-v2`

**Capabilities**  
  - GOVERNANCE
  - QUOTE @event
  - ESCROW
  - INIT-CALL
  - TRANSFER-CALL
  - MINT-CALL
  - BURN-CALL
  - OFFER-CALL
  - WITHDRAW-CALL
  - BUY-CALL
  - SALE-GUARD-CALL
  - FUNGIBLE-TRANSFER-CALL
  - UPDATE-QUOTE-PRICE @event
  - SALE-WHITELIST @event
  - CONCRETE-POLICY @event
  - OFFER
  - BUY
  - WITHDRAW

## Policy Functions

`enforce-**` functions requires the `ledger::**-CALL` capability to be in scope, which enforces that the function is started from the ledger with the scoped parameters. It then retrieves the `policies` list from the `token` input parameter, which contains a list of policies associated with the token.

`enforce-init`: Runs `policies::enforce-init` at `marmalade-v2.ledger.create-token`.
`enforce-mint`: Runs `policies::enforce-mint` at `marmalade-v2.ledger.mint`.
`enforce-burn`: Runs `policies::enforce-burn` at `marmalade-v2.ledger.burn`.
`enforce-offer`: Runs `policies::enforce-offer` at `marmalade-v2.ledger.offer`(step 0 of `marmalade-v2.ledger.sale`). There is an optional parameter `quote`, which is read in `env-data` field of the transaction. If `quote` is present, the offer saves quote with the sale, and creates the escrow accounts. If not, offer proceeds without quotes.
`enforce-withdraw`: Runs `policies::enforce-withdraw` at `marmalade-v2.ledger.withdraw` (step 1 rollback of `marmalade-v2.ledger.sale`). 
`enforce-buy`: Runs `policies::enforce-buy` at `marmalade-v2.ledger.buy` (step 1 of `marmalade-v2.ledger.sale`).

`get-escrow-account`: Returns the fungible escrow account created for quoted sales at `enforce-offer`. The escrow account receives the fungible from the buyer and distributes the fungibles to the policies and the seller at `buy` step.

`write-concrete-policy`: Registers concrete policy modref into the concrete-policies table.
  - Required Capability
    - Capability: `(CONCRETE-POLICY policy-field policy)`
    - Signer: (keyset-ref-guard `marmalade-v2.marmalade-admin`)

`get-concrete-policy`: Returns the modref of the concrete policy.

`enforce-sale-pact`: Ensures that the `sale` parameter provided to the function is equal to the ID of the currently executing pact. It does this by calling the `pact-id` function to retrieve the ID of the currently executing pact and comparing it to the provided `sale` parameter. If they are not equal, an exception will be thrown".
