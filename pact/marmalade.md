# Marmalade Tutorial

## Quick Guide

1. Create Token
2. Mint / Burn Token
3. Transfer
4. Sale (Offer / Buy)

## Marmalade Ledger

The main contract in Marmalade is `marmalade.ledger`. This contract stores the token information, token's accounts, and the policy associated with it. The main functions, `create-token`, `mint`, `burn`, `transfer`, `sale`. Token policies can customize to allow one/both/none of `transfer` and `sale` as a way of transferring. `transfer` allows a direct transfer, and `sale` allows escrowed transfer with timeout.

### Create Token

Token is created in marmalade via running `create-token`. Arguments include:

- `id`: token-id, formatted in `t:{token-detail-hash}`. Should be created using `create-token-id`
- `precision`: Number of decimals allowed for for the token amount. For one-off token, precision must be 0, and should be enforced in the policy's `enforce-init`.
- `uri`: url to external JSON containing metadata
- `policy`: policy contract with custom functions to execute at marmalade functions

`policy::enforce-init` function of the policy contract is executed in `create-token`.

### Mint Token

Token amount is minted to an account at `mint`. Arguments include:

- `id`: token-id
- `account`: account that will receive the minted token
- `guard`: guard of the minted account
- `amount`: amount to be minted

`policy::enforce-mint` function of the policy contract is executed at `mint`.

### Burn Token

Token amount is burnt from an account at `burn`. Arguments include:

- `id`: token-id
- `account`: account where the token will be burnt from
- `amount`: amount to be burnt

`policy::enforce-burn` function of the policy contract is executed at `burn`.

### Transfer

Token amount is transferred from sender to receiver at `transfer`. Arguments include:

- `id`: token-id
- `sender`: sender account
- `receiver`: receiver account
- `amount`: amount to be transferred

`policy::enforce-transfer` function of the policy contract is executed at `transfer`.

### Sale

`sale` allows a two-step offer - buy escrow system using [defpact](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defpact). Arguments include:

- `id`: token-id
- `seller`: seller account
- `amount`: amount to be sold
- `timeout`: timeout of the offer

#### offer

Step 0 of `sale` executes `offer`. `offer` transfers the token from the seller to the escrow account.

`policy::enforce-offer` function of the policy contract is executed at step 0 of `sale`

#### withdraw (cont)

Step 0-rollback executes `withdraw`. `withdraw` transfers token from the escrow back to the seller. `withdraw` can be executed after timeout, by sending in `cont` command with `rollback: true`, `step: 0`. Formatting `cont` commands can be read in [here](https://pact-language.readthedocs.io/en/latest/pact-reference.html?highlight=continuation#yaml-continuation-command-request)

`policy::enforce-withdraw` function of the policy contract is executed at step 0-rollback of `sale`

#### buy (cont)

Step 1 executes `buy`. `buy` transfers token from the escrow to the buyer. `buy` can be executed before `timeout`. The `buyer` and `buyer-guard` information is read from the `env-data` of the command instead of passing in arguments. Just like `withdraw`, `buy` is executed using `cont` command with `rollback:false`, `step: 0`.

`policy::enforce-buy` function of the policy contract is executed at step 1 of `sale`

## Policies

Marmalade Policies allow customized rules for token operations. Read about policies [here](./policies/policies.md)

- [Simple One Off Collection Policy](./policies/one-off-collection-policy/one-off-collection-policy.pact)
- [Fixed Quote Policy](./policies/fixed-quote-policy/fixed-quote-policy.pact)
- [Fixed Quote Royalty Policy](./policies/fixed-quote-royalty-policy/fixed-quote-royalty-policy.pact)
- [Guard Policy](./policies/guard-policy/guard-policy.pact)
