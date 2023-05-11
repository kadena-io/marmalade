# migration-policy-v1

The `migration-policy-v1` policy is specifically built to migrate `marmalade.ledger` tokens to `marmalade.ledger-v2`. It facilitates migration by storing v1 token names to v2

## Specification, tables, capabilities:

**Schemas**:

- `migration` schema stores `token-id-v1`, `token-id-v2`
- `token-id-v2` stores `token-id-v2`

**Tables**:

- `migrations` table store migration information, keyed by `token-id-v2`.
- `token-ids` table stores `token-id-v2` information, keyed by `token-id-v1`. `token-ids` table is created to allow querying `token-id-v2` with `token-id-v1`.

**Capabilities**:

- `GOVERNANCE` that enforce access control of the contract.
- `V1_MIGRATE_TOKEN` that emits `token-id-v1`, `token-id-v2` information at `enforce-mint` at initial mint
- `V1_MIGRATE_MINT` that emits `token-id-v1`, `token-id-v2`, `amount` information at `enforce-mint`

## Policy Functions

`enforce-mint`: runs at `marmalade.ledger.mint`. registers `token-id-v2` into the `token-ids` table at initial mint. burns `token-id-v1` from v1's ledger, and updates `amount` in the migrations table.

`get-migration`: Takes in `token-id-v2` and returns migration information.

`get-token-id-v1`: Takes in `token-id-v2` and returns `token-id-v1`

`get-token-id-v2`: Takes in `token-id-v1` and returns `token-id-v2`

## Enabling

To use the `migration-policy-v1` , enable it by adding `marmalade.migration-v1` into `adjustable-policies` field.
Once v1 to v2 migration is complete, `migration-policy-v1` can be removed from `adjustable-policies`, by using `marmalade.ledger.adjust-policy`.

## Migration Steps

#### Allow burn in marmalade v1.

Marmalade v1 tokens are linked with a single policy, that implements `token-policy-v1`. Make sure that your token policy allows `enforce-burn`. If not, contract admins should upgrade the contract and allow `enforce-burn` to enable migration.

#### Create off-chain URI that will store metadata, which was used in `token-manifest`

Marmalade v2 expects off chain uri, specified [here](../../README.md#using-policies). Read about the [onchain to offchain migration](../../migration.md#onchain-manifest-to-offchain-uri)

#### Create token with `marmalade-policy-v1` with marmalade v2.

With the created `uri`, you can now create a new token. Note that you need to add `marmalade-policy-v1` as `adjustable-policies`, and add `token-id-v1` information in the `data` field.

```
(marmalade.ledger-v2.create-token
  (create-token-id {
     "precision": precision
    ,"uri": uri
    ,"policies":
      { 'concrete-policies: CONCRETE_POLICIES
       ,'immutable-policies: IMMUTABLE_POLICIES
       ,'adjustable-policies: [migration-policy-v1]
      }
  })
  precision
  uri
  { 'concrete-policies: CONCRETE_POLICIES
   ,'immutable-policies: IMMUTABLE_POLICIES
   ,'adjustable-policies: [migration-policy-v1]
  })
```

#### Mint the token

At `mint`, information about `token-id-v1` is required. To pass this information, add into data field of the transaction with token-id of marmalade-v1 in an object keyed with `token-id-v1`

```
{"token-id-v1": "old-token-id-from-v1"}
```

Note that `burn` need to be enabled in marmalade v1 policy for the desired token.

#### Remove `migration-policy-v1` from the `adjustable-policies`

The policy does not have any reason to be stored in the `adjustable-policies` field. It only adds extra function to be run at `marmalade` functions. We suggest removing the policy when the migration is complete.
You will still have access to `migration` history, and lookup functions for old and new token-ids with `get-migration`, `get-token-id-v1`, `get-token-id-v2`.
