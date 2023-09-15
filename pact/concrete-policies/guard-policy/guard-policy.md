# Guard Policy

guard-policy is a policy that defines several functions that enforce rules on various token-related actions such as minting, burning, transferring, and buying/selling tokens. The policy defines a schema for guards which includes keysets for various actions.

## Specification, tables, capabilities:


**Schemas**: `guards` schemas that store `mint`, `burn`, `sale`, `transfer` guards

**Tables**: `policy-guards` table that store the mapped token IDs to guard values for that token.

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrade.
 - `GUARDS` @event: emits token's guard information at `enforce-init`
 - `MINT`: enforces the registered `mint-guard` at `enforce-mint`
 - `BURN`: enforces the registered `burn-guard` at `enforce-burn`
 - `SALE`:  enforces the registered `sale-guard` at `enforce-offer`, `enforce-withdraw`, `enforce-buy`
 - `TRANSFER` : enforces the registered `sale-guard` at `enforce-transfer`

## Policy Functions

**enforce-init**: initializes the policy-guards table for a given token ID with the appropriate guard values.

**enforce-mint**: enforces mint guards

**enforce-burn**: enforces burn guards

**enforce-offer**: enforces sale guards and checks the sale-id against the currently executing pact.

**enforce-withdraw**: enforces sale guards, and checks the sale-id against the currently executing pact.

**enforce-buy**: enforces sale guards, and checks the sale-id against the currently executing pact.

**enforce-transfer**: enforce transfer guards, including checking the sender, receiver, and amount of the transfer.

Finally, the code includes a conditional block that creates the policy-guards table if it does not already exist, or returns a message indicating that the upgrade is complete if the table does exist.


## Mint Guards

Mint Guards are security mechanism that most tokens should make use of. Naturally without any policy interruption, `create-token` does not guard its minting to any entity.

Mint guards are a security mechanism used in the `guard-policy-v1` to control and restrict the minting process of non-fungible tokens (NFTs). They are designed to ensure that only authorised entities can mint new tokens, and they help enforce the single mint rule for NFTs with a fixed supply of one.

When the `enforce-mint` function is called, it checks the provided `guard` against the stored mint guard for the given token. If the provided guard matches the stored mint guard, the minting process can proceed. Otherwise, the minting process is halted, ensuring that only authorised entities can mint new tokens.

Otherwise non-f
