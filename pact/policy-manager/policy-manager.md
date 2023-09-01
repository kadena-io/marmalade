# Policy Manager

The Policy Manager is a module that maps that maps the function calls of individual policies of the token. With the upgrade of marmalade V2, token creators now can select multiple policies to program the tokens. These policies can include rules for transfers, burns, sales, and more, providing a flexible and customizable way to manage the behavior of tokens on the platform. The token policies is built using the `kip.token-policy-v2` interface, which defines the standard interface that token policies must implement.

This module includes the following key components:


**Ledgers Table**: There are functions that limits their call to be initiated fro the ledger. We create a ledgers table, so we can save the module reference to the ledger, and add enforcement that the function cannot be called otherwise.

**Concrete Policies Table**: A table, `concrete-policies` is created to store the concrete policy information for each token policy. A Concrete policy can be viewed as a default policy provided by the marmalade team. Its a standardised set of policies to get token creators started. This table ensures proper handling of policy configurations for the NFTs managed under the policy.

**Ledger Schema**: The `ledger` schema contains the module reference of the ledger that the policy manager is governed by. We strictly limit the module to use the interface, `ledger-v1`.

**Concrete Policies Schema:**: The `concrete-policies` schema contains the module reference. We strictly limit the module to use the interface, `token-policy-v2`

## Policy Functions

`enforce-buy`: The `enforce-buy` function requires the `ledger::INIT-CALL` capability to be in scope, which enforces that the function is started from the ledger with the scoped paameters. It then retrieves the `policies` object from the `token` input parameter, which contains a list of policies associated with the token.

Its good to understand that the buy function within the policy manager acts a bit different then your regular policy, since it's managing other policies. In this case it checks
If the `QUOTE_POLICY` is present in the policies list, the function retrieves the quote specifications associated with the sale ID and calculates the sale price based on the amount of tokens being purchased and the price per token specified in the quote. The function then creates a new guard for the `QUOTE_ESCROW` capability and an escrow account associated with that guard.

If the `QUOTE_POLICY` is not present in the policies list, the function continues with the token purchase process by transferring funds from the buyer to the seller and transferring the appropriate number of tokens from the seller to the buyer.

Regardless of whether the `QUOTE_POLICY` is present, the function then iterates over the list of policies (excluding the `QUOTE_POLICY`) and enforce-buy on each policy within the list.

`get-escrow-account`: Returns the escrow account created for quoted sales. The escrow account collects the fungible from the buyer and distributes the fungibles to the policies and the senders. |

`enforce-sale-pact`: Ensures that the `sale` parameter provided to the function is equal to the ID of the currently executing pact. It does this by calling the `pact-id` function to retrieve the ID of the currently executing pact and comparing it to the provided `sale` parameter. If they are not equal, an exception will be thrown".

`reserve-sale`: Enables the quote guards to reserve a sale with an updated quote price. Transfers the fungible from the buyer's fungible account and saves the marmalade buyer account information that is enforced at `buy`.

   Required capability for sign: `UPDATE_QUOTE_PRICE`
