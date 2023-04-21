# Non-fungible-policy-v1
  

The `non-fungible-policy-v1` facilitates the creation and management of non-fungible tokens (NFTs) with a fixed supply of `ONE`. It's implementing the `kip.token-policy-v2` interface. This policy provides a concrete implementation for handling various token-related actions, such as minting, burning, offering, buying, withdrawing, transferring, and cross-chain transfers.


## Overview
  
This policy module includes the following key components:
 
  
**Policy functions**: Several functions that enforce specific rules for token-related actions.
  
**Mint Guards Specification**: The `mint-guard-schema` is a schema designed to store information related to mint guards for non-fungible tokens.
 
**Mint Guards Table**: A table, `mintguards`, is created to store the mint guard information for each non-fungible token (NFT), including the token ID and its associated mint guard. This table ensures proper authorization and control over the minting process, preserving the unique properties of the NFTs managed under the policy.

  

## Policy Functions
 
`enforce-init`: Ensures that the token initiation is performed by a valid ledger guard and stores the mint guard associated with the token for controlling the minting process. It is executed to set up the necessary conditions and store the mint guard associated with the token before any minting actions can take place. This  is done for maintaining the unique and non-fungible nature of the tokens, ensuring that only authorized entities can mint new tokens and that the process adheres to the predefined rules. 


`enforce-mint`: By enforcing the minting rules, in this case allowing only a single mint for each token and ensuring a fixed supply of 1, this maintains the non-fungible. 




## Enabling

To use the `non-fungible-policy-v1` , enable it by setting it to `true` within the concrete policies list.


## Mint Guards

Mint guards are a security mechanism used in the `non-fungible-policy-v1` to control and restrict the minting process of non-fungible tokens (NFTs). They are designed to ensure that only authorised entities can mint new tokens, and they help enforce the single mint rule for NFTs with a fixed supply of one.
  
In the context of the `non-fungible-policy-v1`, mint guards are represented as a `guard` data type and stored in a table named `mintguards`. Each entry in this table consists of a token ID and its associated mint guard. The mint guard is set during the token initialisation process and cannot be changed afterward.

When the `enforce-mint` function is called, it checks the provided `guard` against the stored mint guard for the given token. If the provided guard matches the stored mint guard, the minting process can proceed. Otherwise, the minting process is halted, ensuring that only authorised entities can mint new tokens.