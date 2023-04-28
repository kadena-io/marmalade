
# Fungible-quote-policy-v1

The `fungible-quote-policy-v1` is specifically tailored for managing token sales through a simple quoted pricing mechanism. By implementing the `kip.token-policy-v2` interface, this policy provides a concrete implementation for handling various token-related actions, such as minting, burning, offering, buying, withdrawing, transferring, and cross-chain transfers.

## Specifications and Tables:
  
**Policy functions**: Several functions that enforce specific rules for token-related actions.

**Quote Specifications**: A schema for quote specifications, `quote-spec`, is provided, which includes details about the fungible token, price, recipient, and recipient guard.

**Marketplace Fee Specifications**: A schema for marketplace fee specifications, `marketplace-fee-spec`, is included, with details about the marketplace account and fee.

**Quote Table**: A table, `quotes`, is created to store the quote information, including the quote ID and its associated specifications.

  

## Policy Functions

`enforce-offer`: In order to facilitate the sale of tokens in a secure and controlled manner `enforce-offer` provides a mechanism to validate the essential details of a sale before initiating the sale process. The primary function of enforcing this rules is to make sure that the sale conditions are met, ultimately ensuring a safe and reliable marketplace for both buyers and sellers.

`enforce-buy`: Enforces buying rules and handles the payment of marketplace fees and royalties, if applicable. The function also ensures that the correct sale token is used. In the end this ensures for buyers and sellers to trade their tokens are exchanged between the intended parties and transferred accordingly

  
## Enabling


To use the `fungible-quote-policy-v1` , enable it by setting it to `true` within the concrete policies list.

  

By utilising this policy module, you can build token sale platforms and marketplaces that offer a simple and secure way of managing token sales with quoted pricing mechanisms.

  
## Payload messages

  
#### QUOTE-MSG-KEY

  
The `enforce-offer` function captures a quote specification for the sale of a fungible token from a message. The message payload contains the `quote-spec` based object within the `quote` property of the payload, which has three required fields: `fungible`, `price`, `recipient`, and `recipient-guard`.


The `fungible` field specifies the module that contains the fungible token being sold. The `price` field specifies the price per unit of the token being sold. The `recipient` field specifies the recipient of the token being sold. The `recipient-guard` field specifies a guard that must be satisfied by the recipient's details.

#### MARKETPLACE-FEE-MSG-KEY
  
The `enforce-buy` function enables the purchase of a fungible token and enforces the transfer of funds from the buyer to the seller and marketplace fees, if applicable. The message payload contains the `marketplace-fee-spec` object within the `marketplace-fee` property of the payload, which has two required fields: `marketplace-account` and `fee`.
    
The `marketplace-account` field specifies the account to which the marketplace fees will be transferred. The `fee` field specifies the percentage of the sale price that will be charged as a fee.

Once the `marketplace-fee-spec` object has been read from the message payload, the function extracts the relevant fields from it and performs several checks to ensure the validity of the sale. It then transfers the appropriate funds to the seller, as well as any applicable marketplace fees.


## Events

  
#### QUOTE Event
The `QUOTE` event is designed to be emitted during the execution of the `enforce-offer` function within the `fungible-quote-policy-v1` The primary purpose of this event is to log and provide information about a token sale offer, including details about the sale, token, and quote specifications.


This event is emitted using the following line of code:
```(emit-event (QUOTE sale-id (at 'id token) amount price sale-price spec))```


