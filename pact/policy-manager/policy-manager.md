# Policy Manager

The Policy Manager is a module that maps that maps the function calls of individual policies of the token. With the upgrade of marmalade V2, token creators now can select multiple policies to program the tokens. These policies can include rules for transfers, burns, sales, and more, providing a flexible and customizable way to manage the behavior of tokens on the platform. The token policies is built using the `kip.token-policy-v2` interface, which defines the standard interface that token policies must implement.

This module includes the following key components:


**Ledgers Table**: There are functions that limits their call to be initiated fro the ledger. We create a ledgers table, so we can save the module reference to the ledger, and add enforcement that the function cannot be called otherwise.

**Concrete Policies Table**: A table, `concrete-policies` is created to store the concrete policy information for each token policy. A Concrete policy can be viewed as a default policy provided by the marmalade team. Its a standardised set of policies to get token creators started. This table ensures proper handling of policy configurations for the NFTs managed under the policy.

**Ledger Schema**: The `ledger` schema contains the module reference of the ledger that the policy manager is governed by. We strictly limit the module to use the interface, `ledger-v1`.

**Concrete Policies Schema:**: The `concrete-policies` schema contains the module reference. We strictly limit the module to use the interface, `token-policy-v2`

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
    - Capability: `(RESERVE_SALE_AT_PRICE sale-id price buyer buyer-guard)`
    - Signer: One of the `quote-guards`
    - Capability: `(fungible::TRANSFER buyer-fungible-account escrow-account sale-price)`
    - Signer: buyer-fungible-account

`map-escrowed-buy`: Helper function inside the `enforce-buy` to process quoted sales.

`get-escrow-account`: Returns the fungible escrow account created for quoted sales at `enforce-offer`. The escrow account receives the fungible from the buyer and distributes the fungibles to the policies and the seller at `buy` step.

`write-concrete-policy`: Registers concrete policy modref into the concrete-policies table.
  - Required Capability
    - Capability: `(CONCRETE_POLICY policy-field policy)`
    - Signer: (keyset-ref-guard `marmalade-v2.marmalade-admin`)

`get-concrete-policy`: Returns the modref of the concrete policy.


`enforce-sale-pact`: Ensures that the `sale` parameter provided to the function is equal to the ID of the currently executing pact. It does this by calling the `pact-id` function to retrieve the ID of the currently executing pact and comparing it to the provided `sale` parameter. If they are not equal, an exception will be thrown".
