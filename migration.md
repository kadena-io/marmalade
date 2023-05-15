# Marmlade V2 Migration Guide

## Overview of Changes from V1

The migration from V1 to V2 introduces several important changes that enhance the functionality. The following changes have been implemented:

- [Token Policies](#token-policies)
- [Policy Manager](#policy-manager)
- [Onchain Manifest to offchain URI](#onchain-manifest-to-offchain-uri)
- [Migration of minted token from v1](#migration-of-minted-token-from-v1)

### Token Policies

We are upgrading the kip.token-policy-v1 interface to kip.token-policy-v2.

In Marmalade v1, it was necessary to select **one** policy that implements `kip.token-policy-v1` when creating tokens. In Marmalade V2, we now expect a `kip.token-policy-v2.token-policies` object, which is displayed below. Each of the policies field store a list of policies that implement `kip.token-policy-v2`.

```
(defschema token-policies
  concrete-policies:object{concrete-policy}
  immutable-policies:[module{token-policy-v2}]
  adjustable-policies:[module{token-policy-v2}]
)
```

Detailed information about each policies field can be found [here](./README.md#using-policies)

#### Re-writing v1 Contract with concrete-policies

TODO (re-written collection-policy link)

### Policy Manager

In Marmalade v1, we called the `enforce-**` functions directly from the policy. In v2, to handle multiple policies, we call `enforce-**` from the `policy-manager`. For example, `policy-manager.enforce-init` is called at `ledger.create-token`, and `policies::enforce-init` are called from the `policy-manager`

We also introduce `escrow` accounts in the policy manager, which are for tokens that use `fungible-quote-policy`. We let multiple policies to access the fungible quote, and escrow account to be used for paying a portion of the fungible to other policies that request it, i.e.) `royalty-policy`.

### Onchain manifest to offchain URI

In marmalade v1, we expected manifests in a data format specified by `kip.token-manifest.manifest` schema. Although this allowed easy storage of customized data for projects, it was impossible to build a standard around it.

In V2, to provide marmalade tools, and marketplaces that standardizes the way of looking up token data, we are requesting `uri` in tokens. This `uri` will be expected to host a json object that follows the schema defined [here](./README.md#off-chain-schema). We recommend storing data on IPFS for decentralized and available storage, but projects can decide depending on project needs.

Here are some guides to using IPFS:

~~- [Marmalade guide to storing file on IPFS](https://docs.kadena.io/build/guides/marmalade-tutorial#interplanetary-storage-saving)~~outdated
- [Best Practices for Storing NFT Data Using IPFS](https://docs.ipfs.tech/how-to/best-practices-for-nft-data/#best-practices-for-storing-nft-data-using-ipfs)

#### Using Onchain with V2

With the transition to V2, we do not require using onchain-manifests. However, for projects wishing to continue using onchain-manifests similar to V1, they can do so by adding `onchain-manifest-policy-v1` to the `immutable-policies` field. This policy accepts the `kip.token-manifest` schema, which aligns with the schema used in V1.

The manifests stored with `onchain-manifest-policy-v1` will be upgradable, hence it will require a guard to manage the upgrades. If the projects don't desire upgradable manifests, they can simple register a failing guard as the manifest guard.

Below is an example demonstrating the env-data required to add a manifest to the token using `onchain-manifest-policy`

```
"manifest-spec": {
    "manifest": (kip.token-manifest.create-manifest (kip.token-manifest.uri "text" "data") [])
   ,"guard": {"keys": ["manifest"], "pred": "keys-all"}
 }
```

Please be aware that offchain-uri will still be required in order to ensure that the tokens are supported by marmalade standards. Therefore, we recommend using onchain-manifest-policy **only if** onchain-manifests are needed, as offchain-uri adequately fulfills most requirements.

## Migration of minted token from v1

For tokens minted in marmalade v1, we provide a `migration-policy-v1`, which allows to track old token-ids from new token-ids, and the amount burnt and minted. Please follow the instructions in [Migration steps using migration policy](./pact/policies/migration-policy/migration-policy.md#migration-steps)
