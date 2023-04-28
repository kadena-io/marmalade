
# Collection Policy

The Collection Policy is a module that manages the creation and management of collections of tokens. It's a concrete policy that displays the simplicity of creating collections with Marmalade. v2. In Marmalade v1 there was a collection policy that included a whitelist. This functionality is removed in v2, and will become a separate policy.  Its good to know that like all concrete policies the module implements the `kip.token-policy-v2` interface, which defines the standard interface that token policies must implement.


  
## Specification, tables, capabilities:


**Schemas**: `collection` and `token` schemas that store information related to collections and the tokens within them.

**Tables**: `collections` and `tokens` tables that store the collections and token information, respectively.

**Capabilities**: `GOVERNANCE`, `INTERNAL`, and `OPERATOR` capabilities that enforce access control.


## Policy Functions

The functions within this collection policy are very straight forward.
  
`init-collection`: Initialises a new collection.
`enforce-init`: Initialises a new token 
`enforce-mint`: Mints a new token.

The other enforcements within the collection policy will only return true and in case of burning and cross-chain transfers false

`get-policy`: This function is used to retrieve the policy associated with a particular token.
`get-collection:` This function is used to retrieve information Collection info by collection -id
`get-token:`: This function is used to retrieve token info by token-id


## Enabling


To use the `collection-policy-v1` , enable it by setting it to `true` within the concrete policies list.

  


## Capabilities

 
This policy module has the following capabilities:
 
`GOVERNANCE`: Enforces that only the `marmalade-admin` keyset has the authority to call this capability.
`INTERNAL`: Allows internal function calls.
`OPERATOR`: Enforces that only the operator of the specified collection has the authority to call this capability.
