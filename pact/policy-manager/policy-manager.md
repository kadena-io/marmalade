# Policy Manager

The Policy Manager is a module that enables token creators to define and enforce policies for their tokens. In contrast with marmalade v1 which only supports one policy marmalade v2 supports something we call `stackable-policies` which enables token creators to add multiple policies to their tokens. These policies can include rules for transfers, burns, sales, and more, providing a flexible and customisable way to manage the behaviour of tokens on the platform. The module is built using the `kip.token-policy-v2` interface, which defines the standard interface that token policies must implement.

This module includes the following key components:

**Ledger Info Table**: A table, `concrete-policy-table`, is created to store the concrete policy information for each token policy. This table ensures proper handling of policy configurations for the NFTs managed under the policy.

**Ledger Schema**: The `ledger` schema contains information about the ledger the policy manager manages for. It consists of `ledger`, `ledger-guard`, and `concrete-policy-manager` field.

## Policies List Schema

Includes the following fields:

- `concrete-policies`: A list of concrete policies.
  A Concrete policy can be viewed as a default policy provided by the marmalade team. Its a standardised set of policies to get token creators started.

## Policy Functions

`enforce-buy`: The `enforce-buy` function first calls the `enforce-ledger` function to ensure the ledger is up-to-date. It then retrieves the `policies` object from the `token` input parameter, which contains a list of policies associated with the token.

Its good to understand that the buy function within the policy manager acts a bit different then your regular policy, since it's managing other policies. In this case it checks
If the `QUOTE_POLICY` is present in the policies list, the function retrieves the quote specifications associated with the sale ID and calculates the sale price based on the amount of tokens being purchased and the price per token specified in the quote. The function then creates a new guard for the `QUOTE_ESCROW` capability and an escrow account associated with that guard.

If the `QUOTE_POLICY` is not present in the policies list, the function continues with the token purchase process by transferring funds from the buyer to the seller and transferring the appropriate number of tokens from the seller to the buyer.

Regardless of whether the `QUOTE_POLICY` is present, the function then iterates over the list of policies (excluding the `QUOTE_POLICY`) and enforce-buy on each policy within the list.

`is-used`: Is a simple helper function we created to indicate whether a specific policy is currently in use for the token.

`get-escrow-account`: Returns the escrow account created in `bidding` inside quote policy, which received funds when the bidder initiated the bid.

`enforce-sale-pact`: Ensures that the `sale` parameter provided to the function is equal to the ID of the currently executing pact. It does this by calling the `pact-id` function to retrieve the ID of the currently executing pact and comparing it to the provided `sale` parameter. If they are not equal, an exception will be thrown".

`create-concrete-policy-list`: We use this to generate a list of concrete policies from the available policies stored in a `token-policies` object.

<!-- ## Events

#### `ROTATE_POLICY` Event

The `ROTATE_POLICY` event is emitted when a new policy is set for a token ID in the `concrete-policy-table` within the `policy-manager` module. The primary purpose of this event is to log and provide information about the token and the updated policy.

This event is emitted using the following line of code:
`(emit-event (ROTATE_POLICY token-id policy))` -->
