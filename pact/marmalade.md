# Marmalade V2

### Marmalade Ledger

The main contract in Marmalade is `marmalade.ledger`. This contract stores the token information, token's accounts, and the policies associated with it. The main functions, `create-token`, `mint`, `burn`, `transfer`, `sale`. Token policies can customize to allow one/both/none of `transfer` and `sale` as a way of transferring. `transfer` allows a direct transfer, and `sale` allows escrowed transfer with timeout.

### Policy manager

Marmalade V2's new feature is a policy manager. Marmalade tokens now store multiple policies in a `token-policies` format, instead of a single policy. We provide 4 different concrete policies (link)
Policy manager acts as a middleware between policies, and run `policy::enforce-**` functions.

## Using Policies

### Concrete Policies

Marmalade V2 aims to make token creation simple and convenient, yet still offer the rich features using concrete-policies. We provide 4 concrete policies, which will provide the most used functionalities.

- **Collection Policy**: Initiates a collection with pre-defined token lists
- **Fungible Quote Policy**: Provides a sale of NFT with fungibles using escrow account
- **Non-fungible Policy**: Defines the token supply to 1 and precision of 0, so the token becomes non-fungible
- **Royalty-policy** [dependent on `fungible-quote-policy`]: Defines creator account that will receive royalty whenever the token using `fungible-quote-policy` is sold.

Marmalade users can mint tokens with above features by adding `true` or `false` next to the policy fields in `token-policies`. If projects would like to use customized logic in addition to what concrete policies offer, they can add additional policies to `immutable-policies` , or `adjustable-policies` field.

## Marmalade Functions

Token is created in marmalade via running `create-token`. Arguments include:

- `id`: token-id, formatted in `t:{token-detail-hash}`. Should be created using `create-token-id`
- `precision`: Number of decimals allowed for for the token amount. For one-off token, precision must be 0, and should be enforced in the policy's `enforce-init`.
- `uri`: url to external JSON containing metadata
- `policies`: policies contract with custom functions to execute at marmalade functions

`policy-manager.enforce-init` calls `policy:enforce-init` in stored token-policies, and the function is executed in `ledger.create-token`,

### Mint Token

Token amount is minted to an account at `mint`. Arguments include:

- `id`: token-id
- `account`: account that will receive the minted token
- `guard`: guard of the minted account
- `amount`: amount to be minted

`policy-manager.enforce-mint` calls `policy:enforce-mint` in stored token-policies, and the function is executed at `ledger.mint`.

### Burn Token

Token amount is burnt from an account at `burn`. Arguments include:

- `id`: token-id
- `account`: account where the token will be burnt from
- `amount`: amount to be burnt

`policy-manager.enforce-burn` calls `policy:enforce-burn` in stored token-policies, and the function is executed at `ledger.burn`.

### Transfer

Token amount is transferred from sender to receiver at `transfer`. Arguments include:

- `id`: token-id
- `sender`: sender account
- `receiver`: receiver account
- `amount`: amount to be transferred

`policy-manager.enforce-transfer` calls `policy:enforce-transfer` in stored token-policies, and the function is executed at `ledger.transfer`.

### Sale

`sale` allows a two-step offer - buy escrow system using [defpact](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defpact). Arguments include:

- `id`: token-id
- `seller`: seller account
- `amount`: amount to be sold
- `timeout`: timeout of the offer

#### offer

Step 0 of `sale` executes `offer`. `offer` transfers the token from the seller to the escrow account.

`policy-manager.enforce-offer` calls `policy:enforce-offer` in stored token-policies, and the function is executed at step 0 of `sale`

#### withdraw (cont)

Step 0-rollback executes `withdraw`. `withdraw` transfers token from the escrow back to the seller. `withdraw` can be executed after timeout, by sending in `cont` command with `rollback: true`, `step: 0`. Formatting `cont` commands can be read in [here](https://pact-language.readthedocs.io/en/latest/pact-reference.html?highlight=continuation#yaml-continuation-command-request)

`policy-manager.enforce-withdraw` calls `policy:enforce-withdraw` in stored token-policies, and the function is executed at step 0-rollback of `sale`

#### buy (cont)

Step 1 executes `buy`. `buy` transfers token from the escrow to the buyer. `buy` can be executed before `timeout`. The `buyer` and `buyer-guard` information is read from the `env-data` of the command instead of passing in arguments. Just like `withdraw`, `buy` is executed using `cont` command with `rollback:false`, `step: 0`.

`policy-manager.enforce-buy` calls `policy:enforce-buy` in stored token-policies, and the function is executed at step 1 of `sale`

## Policies

Marmalade Policies allow customized rules for token operations. Read about policies [here](./concrete-policies/concret-policies.md)

Concrete Policies

- [Collection Policy](./concrete-policies/collection-policy/collection-policy.pact)
- [Fungible Quote Policy](./concrete-policies/fungible-quote-policy/fungible-quote-policy-v1.pact)
- [Non-Fungible Policy](./concrete-policies/non-fungible-policy/non-fungible-policy.pact)
- [Royalty Policy](./concrete-policies/royalty-policy/royalty-policy-v1.pact)

Regular Policies

- [Whitelist Policy]()
