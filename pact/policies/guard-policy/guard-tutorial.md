# guard-policy tutorial

guard-policy is a policy that defines several functions that enforce rules on various token-related actions such as minting, burning, transferring, and buying/selling tokens. The policy defines a schema for guards which includes keysets for various actions.

A table:policy-guards contains the mapped token IDs to guard values for that token. Additionally.

The enforce-ledger function is used to enforce the marmalade.ledger.ledger-guard guard, which appears to ensure that only authorized parties can make changes to the ledger.

The enforce-init function initializes the policy-guards table for a given token ID with the appropriate guard values.

The enforce-offer and enforce-buy functions enforce rules related to buying and selling tokens, including checking the sale-id against the currently executing pact.

The enforce-transfer and enforce-crosschain functions enforce rules related to transferring tokens, including checking the sender, receiver, and amount of the transfer against the specified guards.

Finally, the code includes a conditional block that creates the policy-guards table if it does not already exist, or returns a message indicating that the upgrade is complete if the table does exist.
