
# Fungible-quote-policy-v1

The `fungible-quote-policy-v1` is specifically tailored for managing token sales through a simple quoted pricing mechanism. By implementing the `kip.token-policy-v2` interface, this policy provides a concrete implementation for handling various token-related actions, such as minting, burning, offering, buying, withdrawing, transferring, and cross-chain transfers.

## Specifications and Tables:
  
**Policy functions**: Several functions that enforce specific rules for token-related actions.

**Bidding functions**: Several functions that support the bidding process  

**Quote Specifications**: A schema for quote specifications, `quote-spec`, is provided, which includes details about the fungible token, price, recipient, and recipient guard.

**Marketplace Fee Specifications**: A schema for marketplace fee specifications, `marketplace-fee-spec`, is included, with details about the marketplace account and fee.

**Quote Table**: A table, `quotes`, is created to store the quote information, including the quote ID and its associated specifications.

**Bid Table**: A table, `bids`, is created to store the bid information


## Policy Functions

`enforce-offer`: In order to facilitate the sale of tokens in a secure and controlled manner `enforce-offer` provides a mechanism to validate the essential details of a sale before initiating the sale process. The primary function of enforcing this rules is to make sure that the sale conditions are met, ultimately ensuring a safe and reliable marketplace for both buyers and sellers.

`enforce-buy`: Enforces buying rules and handles the payment of marketplace fees and royalties, if applicable. The function also ensures that the correct sale token is used. In the end this ensures for buyers and sellers to trade their tokens are exchanged between the intended parties and transferred accordingly

## Bid Functions

`place-bid`: The place-bid function is responsible for placing a bid on a sale. It verifies that the bid is being placed on the correct sale token, transfers the specified amount from the buyer to an escrow account, stores the bid details in the bids collection, and emits a bid event.

`withdraw-bid`: The withdraw-bid function is responsible for withdrawing a previously placed bid. It retrieves the bid details from the bids collection based on the provided bid-id. It verifies that the bid is in an open status and belongs to the buyer by checking the buyer's capability. It then transfers the bid amount from the bid's escrow account back to the buyer. Finally, it updates the bid status to "withdrawn" in the bids collection and emits a bid withdrawn event.

`accept-bid`: The accept-bid function is responsible for accepting a bid for a sale. It retrieves the bid details from the bids collection based on the provided bid-id. It verifies that the bid is in an open status. It then retrieves the sale details from the quotes collection based on the provided sale-id. It updates the sale's spec by setting the fungible, price, seller-guard, and amount fields to match the bid. It also sets the buyer as reserved for the sale, so it can only be purchased by the buyer that placed the bid. The function can only be executed by the seller because it requires the capability of the seller's guard. It updates the bid status to "accepted" in the bids collection. Finally, it emits a bid accepted event.

`transfer-bid`: The transfer-bid function facilitates the transfer of the bid amount to the escrow account that holds the funds for a specific sale before `enforce-buy` is executed in all attached policies. 

Finally, the function returns true to indicate a successful transfer of the bid amount to the escrow account.

  
## Enabling


To use the `fungible-quote-policy-v1` , enable it by setting it to `true` within the concrete policies list.

  

By utilising this policy module, you can build token sale platforms and marketplaces that offer a simple and secure way of managing token sales with quoted pricing mechanisms.

  
## Payload messages

  
#### QUOTE-MSG-KEY

  
The `enforce-offer` function captures a quote specification for the sale of a fungible token from a message. The message payload contains the `quote-spec` based object within the `quote` property of the payload, which has four required fields: `fungible`, `price`, `amount`, and `seller-guard`.


The `fungible` field specifies the module that contains the fungible token being sold. The `price` field specifies the price per unit of the token being sold. The `amount` field specifies the number of units that are being sold. The `seller-guard` field specifies the guard of the seller of the token.

#### MARKETPLACE-FEE-MSG-KEY
  
The `enforce-buy` function enables the purchase of a fungible token and enforces the transfer of funds from the buyer to the seller and marketplace fees, if applicable. The message payload contains the `marketplace-fee-spec` object within the `marketplace-fee` property of the payload, which has two required fields: `marketplace-account` and `mk-fee-percentage`.
    
The `marketplace-account` field specifies the account to which the marketplace fees will be transferred. The `mk-fee-percentage` field specifies the percentage of the sale price that will be charged as a fee.

Once the `marketplace-fee-spec` object has been read from the message payload, the function extracts the relevant fields from it and performs several checks to ensure the validity of the sale. It then transfers the appropriate funds to the seller, as well as any applicable marketplace fees.


## Events

  
#### QUOTE Event
The `QUOTE` event is designed to be emitted during the execution of the `enforce-offer` function within the `fungible-quote-policy-v1` The primary purpose of this event is to log and provide information about a token sale offer, including details about the sale, token, and quote specifications.


This event is emitted using the following line of code:
```(emit-event (QUOTE sale-id (at 'id token) amount price sale-price spec))```


#### BID Event
The `BID` event is emitted when a bid is placed in the `place-bid` function. It represents the act of placing a bid on a sale. 

The event contains the following information:
- `bid-id`: The unique identifier of the placed bid.
- `sale-id`: The identifier of the sale for which the bid is placed.
- `token-id`: The identifier of the token associated with the bid.
- `amount`: The amount of tokens that the bid is for (this must match the amount of tokens that are on offer).
- `price`: The price for a single token at which the bid is placed.
- `buyer`: The account of the buyer who placed the bid.

The `BID` event serves as a notification for interested parties that a new bid has been placed, and it provides relevant details about the bid and the sale.

#### BID-WITHDRAWN Event
The `BID-WITHDRAWN` event is emitted when a bid is withdrawn in the `withdraw-bid` function. It represents the action of removing a previously placed bid from consideration.

The event contains the following information:
- `bid-id`: The unique identifier of the withdrawn bid.
- `sale-id`: The identifier of the sale from which the bid is withdrawn.
- `token-id`: The identifier of the token associated with the bid.
- `amount`: The amount of tokens that the bid was for (this must match the amount of tokens that are on offer).
- `price`: The price for a single token at which the bid was placed.
- `buyer`: The identity of the buyer who withdrew the bid.

The `BID-WITHDRAWN` event serves as a notification for interested parties that a bid has been withdrawn. It provides relevant details about the withdrawn bid, including the associated sale and the buyer who withdrew the bid.

#### BID-ACCEPTED Event
The `BID-ACCEPTED` event is emitted when a bid is accepted in the `accept-bid` function. It signifies that a bid has been successfully accepted by the seller. 

The event contains the following information:
- `bid-id`: The unique identifier of the accepted bid.
- `sale-id`: The identifier of the sale for which the bid was accepted.
- `token-id`: The identifier of the token associated with the bid.
- `amount`: The amount of tokens that the bid is for (this must match the amount of tokens that are on offer).
- `price`: The price for a single token at which the bid is placed.
- `buyer`: The account of the buyer who placed the bid.

The `BID-ACCEPTED` event serves as a notification for interested parties that a specific bid has been accepted and provides relevant details about the bid and the sale.