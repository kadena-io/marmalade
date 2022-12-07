# Collection Policy Tutorial

Collection Policy is a marmalade policy that allows pre-sale of collection tokens. This tutorial will go step-by-step with an example of collection, `muppets-v1`.

Releasing Collection follows the following steps:

1. Initiate Collection (Operator)
2. Reserve Whitelist (Minter)
3. Reveal Tokens in collection (Operator)
4. Create / Mint Token (Minter)
5. TODO - Transfer / Sale - TODO

## 1. Initiate Collection

In order to start a collection, the operator must run `marmalade.simple-1-off-whitelist-collection-policy.init-collection` with required fields. Note, that this step is called directly from the `marmalade.simple-1-off-whitelist-collection-policy.init-collection`.

- `collection-id`: id of collection
- `collection-size`: Total number of tokens in the collection.
- `collection-hash`: Hash of the list of token IDs in the collection.
- `operator-guard`: Guard that is used to reveal the tokens. (todo - discuss, if `reveal-token` should require guard check)
- `operator`: (todo - to receive funds at `mint`)
- `fungible` (todo - the fungible to be transferred as funds)
- `price` (todo - the price of the fungible to be transferred from accounts at `mint`)

The most important field to understand in this step is the `collection-hash`. In order to lock in the tokens without revealing its properties, the list of tokens will be hashed, with each token-id being the hash of the its manifests.
The tokens will be revealed at a later step, and the token manifests will have to match the given collection-hash in order to be created/minted.

When `init-collection` succeeds, an event `(INIT_COLLECTION collection-id collection-size fungible price operator)` will be emitted.

### Initiate `muppets-v1`

1.  Create token-manifest for each token:

All marmalade tokens must use `kip.token-manifest.create-manifest` function, which will return a manifest format like this:

```
{
  uri:object{mf-uri}
  hash:string
  data:[object{mf-datum}]
}
```

For simplicity, each muppet token manifests will contain text of its names.

2. Get `hash` from the manifest. We can take the hash from its manifest. The below code returns `33vh4wJvxEkXW72Bgvd88S6HKcyxLj2WJZEydAP4CCU` as the hash of token manifest

```
(at 'hash (kip.token-manifest.create-manifest (uri "text" "Kermit the Frog") [])))
```

3. Use `marmalade.simple-1-off-whitelist-collection-policy.token-id` to get generate token-id

```
(token-id (at 'hash (kip.token-manifest.create-manifest (uri "text" "Kermit the Frog") []))))
```

The function simply formats the manifest with `t:{manifest-hash}`. The token-id for `Kermit the Frog` muppet token will therefore be `t:33vh4wJvxEkXW72Bgvd88S6HKcyxLj2WJZEydAP4CCU`.

We will do the same for the 7 more tokens, and get the list of token-ids.

```
(let*
  ( (get-manifest (lambda (muppet:string)
      (create-manifest (uri "text" muppet) [])
      ))
    (manifests:list (map get-manifest [ "Kermit the Frog"
                                        "Miss Piggy"
                                        "Fozzie Bear"
                                        "Gonzo"
                                        "Rowlf the Dog"
                                        "Scooter"
                                        "Animal"
                                        "Pepe the King Prawn"
                                        "Rizzo the Rat"]))

    (hashes:list (map (at 'hash ) manifests))
    (tokens:list (map (token-id) hashes)
    ))
   tokens
)
```

returns

```
 [ "t:33vh4wJvxEkXW72Bgvd88S6HKcyxLj2WJZEydAP4CCU"
   "t:UohIzpKOlpp5l7UZ-KNni2xaLu5pO67QlHLmxj65g5U"
   "t:u7u36BxiPTKp5Wuq_mXOLS7r2LFRaLeEKg2FY6ylNX0"
   "t:rj3gbsmUdcXeb0kA39meDeKoMhWOPK6XEOCQ2RKF0q8"
   "t:uOCqa9-MgFF68zyxccZKORGDk2zMRLwpKdXvr8hfE5A"
   "t:dOFBE3GdsL-5BgMCdZfQzT20G81cANzwgwIf22N8_ig"
   "t:LScEFcxVDsvZP38jO1Kp95yNu7hGmGNrzYkwCM5UWyA"
   "t:--kKualbcNt2jKUPLG0Pyp6ByLJerOS_Wtxe914_YHA"
   "t:sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4"]
```

3. Hash the token list.

Finally, we can hash the list and get the `collection-hash`, by using `hash` function.

```
(hash
  ["t:33vh4wJvxEkXW72Bgvd88S6HKcyxLj2WJZEydAP4CCU"
   "t:UohIzpKOlpp5l7UZ-KNni2xaLu5pO67QlHLmxj65g5U"
   "t:u7u36BxiPTKp5Wuq_mXOLS7r2LFRaLeEKg2FY6ylNX0"
   "t:rj3gbsmUdcXeb0kA39meDeKoMhWOPK6XEOCQ2RKF0q8"
   "t:uOCqa9-MgFF68zyxccZKORGDk2zMRLwpKdXvr8hfE5A"
   "t:dOFBE3GdsL-5BgMCdZfQzT20G81cANzwgwIf22N8_ig"
   "t:LScEFcxVDsvZP38jO1Kp95yNu7hGmGNrzYkwCM5UWyA"
   "t:--kKualbcNt2jKUPLG0Pyp6ByLJerOS_Wtxe914_YHA"
   "t:sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4"] )
```

`eLbTngl8lNBPshPMohX0ILM8l7R4RV8eNm9p0Pq1W6E` is our collection hash. Note that all of the steps 1-3 are not to be done on-chain, but should be done off-chain. Only the following step will be expected on-chain.

4. Run `init-collection`

Finally, we have got our required fields to initiate our collection. The following code creates `muppet-v1` collection with 9 tokens that we generated above. This code should now be sent to the chain and make it into a block.

```
(init-collection
  "muppet-v1" 9 "eLbTngl8lNBPshPMohX0ILM8l7R4RV8eNm9p0Pq1W6E"
  "k:{operator-key}" (read-keyset 'operator-guard) coin 0.0)
```

## Reserve Whitelist

Whitelists in this collection policy is on a first-come, first-served basis. This step is to be run by the `minters`.

To reserve the whitelists, minters must generate a principaled account. This can simply be a `k:{public-key}`, but there is a pact function to generate it in code. For example, running the following code.

```
(create-principal (read-keyset 'keyset))
```

with a keyset of

```
{
  "keys": ["27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4"],
  "pred": "keys-all"
}
```

will generate a `k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4`.

With the account name prepared, minters can now run `reserve-whitelist` and get their slots locked in.

Successful whitelist will emit an event, `(RESERVE_WHITELIST collection-id account whitelist-index)`. This info should be saved for the token creations and mint index.

The whitelist of the last token will generate a random index that will shift the whitelist slots. This shift index will be used to randomize the selection of nfts within collection. By randomly setting the shift index at the end of whitelist,
marmalade prevents the operator to reserve certain nfts that may be more valuable than others.

### Whitelist a token in `muppet-v1`

1. Run `reserve-whitelist` with a keyset.

```
(reserve-whitelist "muppet-v1" (create-principal (read-keyset 'keyset)))
```

2. Save the emitted event.

```
(marmalade.simple-1-off-whitelist-collection-policy.RESERVE_WHITELIST "muppets-v1" "k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4" 0)
```

3. Whitelist of the last token generates a shift-index.

Let's assume that the last `reserve-whitelist` made into the block 20987. The shift-index generated will be 8, and the transaction will emit 2 events.

```
(marmalade.simple-1-off-whitelist-collection-policy.RESERVE_WHITELIST "muppets-v1" "k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4" 8)
(marmalade.simple-1-off-whitelist-collection-policy.SHIFT collection-0" 8)
```

## Reveal Tokens

Once `reserve-whitelist` is complete, operator can now reveal the token manifests. This step will reveal which whitelist index is dedicated to each token id.

### Reveal `muppets-v1`

Operator of the `muppets-v1` will prepare the token-ids, and run the function.

```
  (reveal-tokens "muppets-v1"
    [  "t:33vh4wJvxEkXW72Bgvd88S6HKcyxLj2WJZEydAP4CCU"
       "t:UohIzpKOlpp5l7UZ-KNni2xaLu5pO67QlHLmxj65g5U"
       "t:u7u36BxiPTKp5Wuq_mXOLS7r2LFRaLeEKg2FY6ylNX0"
       "t:rj3gbsmUdcXeb0kA39meDeKoMhWOPK6XEOCQ2RKF0q8"
       "t:uOCqa9-MgFF68zyxccZKORGDk2zMRLwpKdXvr8hfE5A"
       "t:dOFBE3GdsL-5BgMCdZfQzT20G81cANzwgwIf22N8_ig"
       "t:LScEFcxVDsvZP38jO1Kp95yNu7hGmGNrzYkwCM5UWyA"
       "t:--kKualbcNt2jKUPLG0Pyp6ByLJerOS_Wtxe914_YHA"
       "t:sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4"])
```

Note that the hash of the list should match the `collection-hash` that was used in the `init-collection` step.
In order for minters to be able to create and mint their tokens, the operator must publish the token manifests to the whitelisters.

## Create Token / Mint Token

`create-token` is unique step required for all marmalade tokens. The purpose of the transaction is to add token information to marmalade ledger. Users must use the published manifest and add it in their transactions to `create-token`.

This step is ideally run together with `mint` transaction. `mint` is the final step that is required to own the token.
Similar to `create-token`, `mint` is a general marmalade operation.

Collection policy enforces that the `create-token` and `mint` is done by the whitelisted account.

### Create "t:sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4" token.

1. Find the `whitelist-info`

   In the previous step, we had reserved whitelist for the account, and saw the emitted event of below.

```
(marmalade.simple-1-off-whitelist-collection-policy.RESERVE_WHITELIST "muppets-v1" "k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4" 0)
```

We need 4 fields for whitelist info: `collection-id`, `account`, `guard`, `index`. What's not in this event is the guard. Guard can simply be the `keyset` info we've provided at `(create-principal (read-keyset 'keyset))`.

Our `whitelist-info` will be added to `env-data` of our transaction, and will look like the following:

```
{
  'whitelist-info : {
     'collection-id: "muppets-v1"
    ,"index": 0
    ,"account": "k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4"
    ,"guard": {"keys": ["27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4"], "pred": "keys-all"}}
}

```

2. Find the token that matches the whitelist index

Our whitelist index was 0. When the whitelist ended, the shift index was updated at 8. This means we will be able to mint the token at index 8. If our whitelist index was 1, we will be able to mint the token at index 0, and so on. The token-id at index 8 is `t:sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4`.

3. Receive the token-manifest from the operator.

The operator will release the token-manifest somewhere for the users to mint the tokens. The manifest that matches token
`t:sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4`, is shown below.

```
{"uri":
  {"scheme": "text","data": "Rizzo the Rat"},
   "hash": "sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4","data": []}
```

Note that above is the result of the code,

```
(create-manifest (uri "text" "Rizzo the Rat") [])
```

4. Run `create-token` with the `whitelist-info` in env-data.

```
(marmalade.ledger.create-token
  "t:sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4"
  0
  {"uri":
    {"scheme": "text","data": "Rizzo the Rat"},
     "hash": "sQ19jh3-w3HOchpBefpKTBGj2_ARjC4xLiV0SVlokf4","data": []}
  marmalade.simple-1-off-whitelist-collection-policy)
```

`create-token` takes in `token-id`, `precision`, `token-manifest`, and `policy`. The `precision` is 0, because this is a one-off collection policy.

The minter need to sign the capability, `(marmalade.ledger.CREATE_TOKEN "t:9mCeDcVIuQET1awDEWbYXF-HlRzhLv5VW3hXiW9m678" "k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4")`

5. Run `mint` with the `whitelist-info` in env-data.

```
(marmalade.ledger.mint
  "t:9mCeDcVIuQET1awDEWbYXF-HlRzhLv5VW3hXiW9m678"
  "k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4"
  (at 'guard (read-msg 'whitelist-info ))
  1.0))
```

`mint` takes in `token-id`, `account`, `guard`, and `amount`. `amount` is always 1.0, because this is an one-off collection policy.

The minter need to sign the capability, `(marmalade.ledger.MINT "t:9mCeDcVIuQET1awDEWbYXF-HlRzhLv5VW3hXiW9m678" "k:27fff7d20390142caf727cd4713d2c810839486fa2350af7e2ce980090185ce4" 1.0)`

The steps 4 and 5 can be run at the same transaction, but the user must sign the both capabilities.

## Conclusion

We have now looked at initiating collection to minting each tokens in the collection. The account designated at the `mint` step now owns the token in the main `marmalade` ledger.

TODO: transfer of the ownership using `transfer` or `sale`
TODO: customize fungible payment at `mint` or `whitelist`