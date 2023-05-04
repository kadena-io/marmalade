
# Collection Policy

The Collection Policy is a module that manages the creation and management of collections of tokens. It's a concrete policy that displays the simplicity of creating collections with Marmalade v2. In Marmalade v1 there was a collection policy that included a whitelist. This functionality is removed in v2, and will become a separate policy.  Its good to know that like all concrete policies the module implements the `kip.token-policy-v2` interface, which defines the standard interface that token policies must implement.


## Specification, tables, capabilities:


**Schemas**: `collection` and `token` schemas that store information related to collections and the tokens within them.

**Tables**: `collections` and `tokens` tables that store the collections and token information, respectively.

**Capabilities**: `GOVERNANCE` and `OPERATOR` capabilities that enforce access control.


## Policy Functions

The functions within this collection policy are very straight forward.

**`create-collection:`** Creates a new collection by defining the collection name, size and guard of the operator of the collection.
- `collection-name`: The name of the collection to be created.
- `collection-size`: The maximum amount of tokens the collection can hold. If `collection-size` is set to 0, the collection can hold an unlimited amount of tokens.
- `operator-guard`: The guard of the operator of the collection to be created.

**`enforce-init:`** Adds a new token to the collection while validating the collection's size. Als sets the guard of the account that is allowed to mint the token.
**`enforce-mint`:** Validates if the account minting the token is allowed to do so.

The other enforcements within the collection policy don't have any additional functionality implemented.

**`create-collection-id:`** This function is used to retrieve a collection identifier based on the name of the collection.
**`get-collection:`** This function is used to retrieve collection information by collection-id
**`get-token:`** This function is used to retrieve token info, related to the collection,  by token-id


## Enabling
To use the `collection-policy-v1` , enable it by setting it to `true` within the concrete policies list.

## Capabilities

This policy module has the following capabilities:

- `GOVERNANCE`: Enforces that only the `marmalade-admin` keyset has the authority to call this capability.
- `COLLECTION`: Used to publish the COLLECTION event.
- `OPERATOR`: Enforces that only the operator of the specified collection has the authority to call this capability.
- `MINT`: Validates that the account minting the token is allowed to do so.
