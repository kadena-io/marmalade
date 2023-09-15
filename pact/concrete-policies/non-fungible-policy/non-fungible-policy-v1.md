# Non-fungible-policy-v1


The `non-fungible-policy-v1` facilitates the creation and management of non-fungible tokens (NFTs) with a fixed supply of `ONE`. It's implementing the `kip.token-policy-v2` interface. This policy provides a concrete implementation for handling various token-related actions, such as minting, burning, offering, buying, withdrawing, transferring, and cross-chain transfers.


## Specifications and Tables:

**Policy functions**: Several functions that enforce specific rules for token-related actions.

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrade.

## Policy Functions

`enforce-init`: Ensures that the token initiation is performed by a valid ledger guard and stores the mint guard associated with the token for controlling the minting process. It is executed to set up the necessary conditions and store the mint guard associated with the token before any minting actions can take place. This  is done for maintaining the unique and non-fungible nature of the tokens, ensuring that only authorized entities can mint new tokens and that the process adheres to the predefined rules.

`enforce-mint`: By enforcing the minting rules, in this case allowing only a single mint for each token and ensuring a fixed supply of 1, this maintains the non-fungible.


## Enabling

To use the `non-fungible-policy-v1` , enable it by setting it to `true` within the concrete policies list.
