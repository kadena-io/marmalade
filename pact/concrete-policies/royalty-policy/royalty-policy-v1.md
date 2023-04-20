# Royalty-policy-v1

  

This policy, `royalty-policy-v1`, is designed to support royalty payouts during the sale of non-fungible token. It implements the `kip.token-policy-v2` interface and extends the functionality provided by the base policy.

  

## Overview

  

This policy module includes the following key components:

  
  

**Policy functions**: Several functions that enforce specific rules for token-related actions.

  

**Royalty Specification**: The `royalty-schema` is a schema designed to store information related to royalties for non-fungible tokens, such as the creator, creator guard, royalty rate, and associated fungible token.

  

**Royalties Table**: A table, `royalties`, is created to store the royalty information for each non-fungible token (NFT), including the token ID and its associated royalty details. This table ensures proper handling of royalty payouts during token sales and maintains a record of royalty configurations for the NFTs managed under the policy.

  

  

## Policy Functions

  

`enforce-init`: Initialises the token with the provided `token-info` and stores royalty information in the `royalties` table.

`enforce-mint`: This function enforce ledger restrictions for various token operations but do not implement any additional rules specific to this policy. 

`enforce-burn`: Enforces burning rules for NFTs managed under this policy and checks for a valid ledger guard. It enforces a false condition by using `(enforce false "Burn prohibited")`. This effectively prevents any burning operation from being executed under this policy, as it will always result in an error with the message "Burn prohibited".

  

`enforce-offer`: This function enforce ledger restrictions for various token operations but do not implement any additional rules specific to this policy. 


`enforce-buy`: Handles the payment of royalties during the sale, ensuring that the correct sale token is used and the appropriate royalty payout is transferred to the creator.

`enforce-transfer`: Prohibits token transfers by always enforcing a false condition.

`enforce-crosschain`: Enforces cross-chain transfer rules for NFTs under this policy and checks for a valid ledger guard. It enforces a false condition by using `(enforce false "Transfer prohibited")`. This effectively prevents any cross-chain transfer operation from being executed under this policy, as it will always result in an error with the message "Transfer prohibited".

  

`enforce-withdraw`: Allows token withdrawals #WIP

  

## Enabling

  

To use the `royalty-policy-v1` , enable it by setting it to `true` within the concrete policies list.

  

### Payload messages

  

#### ROYALTY_SPEC

  
  

The `enforce-init` function initialises a royalty for a fungible token and enforces several checks to ensure the validity of the royalty. The message payload contains the `royalty-schema` object, which has five required fields: `fungible`, `creator`, `creator-guard`, `royalty-rate`, and `quote-policy`.

  

The `fungible` field specifies the module that contains the fungible token. The `creator` field specifies the creator of the token. The `creator-guard` field specifies a guard that must be satisfied by the creator's details. The `royalty-rate` field specifies the percentage of the sale price that will be charged as a royalty. The `quote-policy` field specifies the policy module that contains the quote policy.

  

Once the `royalty-schema` object has been read from the message payload, the function extracts the relevant fields from it and performs several checks to ensure the validity of the royalty. It then inserts the royalty into the `royalties` database.

  
  

### Events Emitted

  

#### ROYALTY Event

  

##### Event Parameters

  

##### Emission

  

##### Use Cases