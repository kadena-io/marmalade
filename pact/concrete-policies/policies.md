# Policies

Policies following [kip/token-policy-v2.pact] interface.
Enable creators to tightly controll all aspects of a token through enforceable token policies for minting, burning, offering, buying, transfer and sale, and cross-chain transfers

# Introduction
The token-policy-v2 interface is used to define policies for the minting, burning, transfer, and sale of tokens. This interface is built using the Pact smart contract language and provides a set of functions that you can use to enforce policies for your token.

# Getting Started
To use this interface, you first need to import it into your Pact code using the following command:

`(namespace 'my-namespace)`
`(import 'marmalade.token-policy-v2)`

To get started, you will need to define the schema for your token using the token-info data type. The token-info data type should include the id, supply, precision, and manifest fields.

`(defschema token-info
  id:string
  supply:decimal
  precision:integer
  manifest:object{kip.token-manifest.manifest})`

Once you have imported the interface, and have defined the token-info data type, you can define your own policies by creating functions that implement the required functions defined by the interface. Here are the functions you can implement:

## enforce-mint:
This function enforces the policy related to minting of tokens. It takes in the token object, the account to mint tokens to, the guard, and the amount to be minted. You can define your own policies related to minting by adding custom checks in this function.

## enforce-burn:
This function enforces the policy related to burning of tokens. It takes in the token object, the account to burn tokens from, and the amount to be burned. You can define your own policies related to burning by adding custom checks in this function.

## enforce-init:
This function enforces the policy related to initialization of tokens. It takes in the token object and can be used to define custom initialization checks.

## enforce-offer:
This function enforces the policy related to offering tokens for sale. It takes in the token object, the seller offering the tokens, the amount being offered, and the sale-id. You can define your own policies related to offering tokens for sale by adding custom checks in this function.

## enforce-buy:
This function enforces the policy related to buying tokens offered for sale. It takes in the token object, the seller offering the tokens, the buyer purchasing the tokens, the buyer-guard, the amount being bought, and the sale-id. You can define your own policies related to buying tokens by adding custom checks in this function.

## enforce-transfer:
This function enforces the policy related to transferring tokens from one account to another. It takes in the token object, the sender of the tokens, the guard, the receiver of the tokens, and the amount being transferred. You can define your own policies related to transferring tokens by adding custom checks in this function.

To define your own policies, simply create a new function with the name of the policy you want to implement and add your custom checks. Make sure the function returns a boolean value indicating whether or not the policy is enforced.

Overall, the token-policy-v2 interface provides a powerful tool for defining policies that govern the behavior of tokens written in Pact on the kadena blockchain. By creating custom functions to enforce policies related to minting, burning, offering, buying, transferring, and cross-chain transfers of tokens, you can ensure that your marmalade token operates in a secure and reliable manner.