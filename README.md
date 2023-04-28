# Kadena Marmalade V2: Mint a Marketplace

## Decentralized Infrastructure for Poly-Fungibles and NFTs

This repo houses smart contracts and frontend code for Marmalade, Kadena's platform for poly-fungibles and NFTs.

### Marmalade Ledger

The main contract in Marmalade is `marmalade.ledger`. This contract stores the token information, token's accounts, and the policies associated with it. The main functions, `create-token`, `mint`, `burn`, `transfer`, `sale`. Token policies can customise to allow one/both/none of `transfer` and `sale` as a way of transferring. `transfer` allows a direct transfer, and `sale` allows escrowed transfer with timeout.

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

Marmalade Policies allow customised rules for token operations.

#### Concrete Policies:

- [Collection Policy](./pact/concrete-policies/collection-policy/collection-policy-v1.pact) ([docs](./pact/concrete-policies/collection-policy/collection-policy-v1.md))
- [Fungible Quote Policy](./pact/concrete-policies/fungible-quote-policy/fungible-quote-policy-v1.pact) ([docs](./pact/concrete-policies/fungible-quote-policy/fungible-quote-policy-v1.md))
- [Non-Fungible Policy](./pact/concrete-policies/non-fungible-policy/non-fungible-policy-v1.pact) ([docs](./pact/concrete-policies/non-fungible-policy/non-fungible-policy-v1.md))
- [Royalty Policy](./pact/concrete-policies/royalty-policy/royalty-policy-v1.pact) ([docs](./concrete-policies/royalty-policy/royalty-policy-v1.md))

Regular Policies

- [Whitelist Policy]() TODO

---

### Marmalade v2 metadata standard

## Overview

With marmalade V2, the **on-chain** metadata schema for non-fungible tokens (NFTs) has been deprecated. This means that the manifest schema, which was previously used to store the metadata for NFTs on-chain, is no longer being used. Instead, a new schema has been introduced, which is off-chain and conforms to the widely accepted standard for NFT metadata.

    (defschema token-schema
        manifest:object{manifest}
    )

Has been replaced with:

    (defschema token-schema
        uri:string
    )

Token schema now has the following structure:

    (defschema token-schema
    	id:string
    	uri:string
    	precision:integer
    	supply:decimal
    	policy:module{kip.token-policy-v2}
    )

- uri: A string representing the URI that points to an external JSON-Schema for the NFT.

## Off-chain Schema

The off-chain schema contains properties that describe the metadata of an NFT stored off-chain in a JSON format. The schema includes the following fields:

| **Field Name**  |  **Data Type**   |                                                                                                                           **Description**                                                                                                                           |
| :-------------- | :--------------: | :-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: |
| name            |      string      |                                                                                                          Identifies the asset to which this NFT represents                                                                                                          |
| description     |      string      |                                                                                                          Describes the asset to which this NFT represents                                                                                                           |
| image           |      string      |                   A URI pointing to a resource with mime type image/\* representing the asset to which this NFT represents. Consider making any images at a width between 320 and 1080 pixels and aspect ratio between 1.91:1 and 4:5 inclusive.                    |
| \*properties    |      object      |                                                                                               Arbitrary properties. Values may be strings, numbers, object or arrays.                                                                                               |
| \*authors       | array of objects |                                                               An array of authors who created or contributed to the asset. Each author is an object with a "name" field specifying the author's name.                                                               |
| \*external_url  |      string      |                                                                                            URL to an external application or website where users can also view the asset                                                                                            |
| \*animation_url |      string      | URL to a multimedia attachment of the asset. The supported file formats are MP4 and MOV for video, MP3, FLAC and WAV for audio, GLB for AR/3D assets, and HTML for HTML pages. You may use the ?ext={file_extension} query to provide information on the file type. |
| \*collection    |      object      |                                                 an object with a "name" field specifying the name of the collection, and a “family” field specifying the larger category or group to which the collection belongs.                                                  |

\* optional

##

### JSON Schema

     {
      "title": "Token Metadata",
      "description": "Schema for non-fungible token (NFT) metadata.",
      "type": "object",
      "required": [
        "name",
        "description",
        "image"
      ],
      "properties": {
        "name": {
          "type": "string",
          "description": "Identifies the asset to which this NFT represents."
        },
        "description": {
          "type": "string",
          "description": "Describes the asset to which this NFT represents."
        },
        "image": {
          "type": "string",
          "format": "uri",
          "description": "A URI pointing to a resource with mime type image/* representing the asset to which this NFT represents. Consider making any images at a width between 320 and 1080 pixels and aspect ratio between 1.91:1 and 4:5 inclusive."
        },
        "properties": {
          "type": "object",
          "description": "Arbitrary properties. Values may be strings, numbers, objects or arrays."
        },
        "external_url": {
          "type": "string",
          "format": "uri",
          "description": "URL to an external application or website where users can also view the asset."
        },
        "animation_url": {
          "type": "string",
          "format": "uri",
          "description": "URL to a multimedia attachment of the asset. The supported file formats are MP4 and MOV for video, MP3, FLAC and WAV for audio, GLB for AR/3D assets, and HTML for HTML pages. You may use the ?ext={file_extension} query to provide information on the file type."
        },
        "authors": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "name": {
                "type": "string",
                "description": "The author's name."
              }
            }
          },
          "description": "An array of authors who created or contributed to the asset."
        },
        "collection": {
          "type": "object",
          "properties": {
            "name": {
              "type": "string",
              "description": "The name of the collection."
            },
            "family": {
              "type": "string",
              "description": "The larger category or group to which the collection belongs."
            }
          },
          "description": "An object specifying the name and family of the collection to which this NFT belongs."
        }
      }
    }

### JSON Basic example

    {
    	"name": "My NFT",
    	"description": "This is my awesome NFT",
    	"image": "https://example.com/my-nft.jpg"
    }

### JSON Additional fields example

    {
      "name": "My NFT",
      "description": "This is my non-fungible token.",
      "image": "https://example.com/image.png",
      "external_url": "https://example.com",
      "animation_url": "https://example.com/animation.mp4",
      "authors": [
        {
          "name": "John Doe"
        },
        {
          "name": "Jane Smith"
        }
      ],
      "collection": {
        "name": "My Collection",
        "family": "Art"
      }
    }

### JSON Properties example

    {
      "name": "Sword of the Thunder God",
      "description": "A legendary sword imbued with the power of the thunder god.",
      "image": "https://example.com/sword-of-thunder-god.jpg",
      "properties": {
        "damage": 50,
        "element": "Thunder",
        "rarity": "Legendary"
      },
      "external_url": "https://example.com/sword-of-thunder-god",
      "authors": [
        {
          "name": "John Smith"
        }
      ],
      "collection": {
        "name": "Legendary Weapons",
        "family": "Fantasy"
      }
    }

##

## Generating t:{hash}

The previous schema used by Marmalade V1 was called 'manifest', and it consisted of a URI, a hash value, and an array of data objects. To create a new manifest, the 'create-manifest' function was used, which took a URI object and an array of data objects as input. The 'create-datum' function was used to create a data object, which consisted of a URI object and a datum object. The 'verify-manifest' function was used to verify that a given manifest was valid, and the 'enforce-verify-manifest' function was used to enforce the validity of a manifest.

    (defschema token-details
    	uri:string
    	precision:integer
    	policy:module{kip.token-policy-v2}
    )

Since **onchain** manifest is deprecated, token-details schema is hashed.

The reason for hashing the token-details is to capture all the data on the ledger for cross-chain data. Previously, the manifest was hashed to create a unique identifier for each NFT, but now the token-details schema is hashed to create a unique identifier for each NFT.

###

## Rationale

### Why the Manifest was Replaced by URI:

Marmalade has undergone a change in its schema with regards to the metadata attached to non-fungible tokens (NFTs). Previously, the metadata for NFTs was stored on-chain within the manifest schema. The manifest schema was called from the ledger by the use of kip.token-manifest. However, the manifest schema has been deprecated and replaced by an off-chain schema.

We decided to replace the manifest schema with a URI-based schema. The new schema stores the metadata off-chain and utilises a URI to reference the metadata. By utilising a URI-based schema, Marmalade tokens can improve scalability, and provide greater flexibility for developers and most of all simplicity of usage in general.

### Why Off-Chain Schema Choice was Made:

The new schema for NFT metadata is a simple JSON schema that describes the properties of the metadata. This schema enables compatibility with various marketplaces and wallets, making Marmalade tokens more interoperable.

Additionally, the schema allows for arbitrary properties to be added to the metadata, providing developers with more flexibility in customising the metadata. The decision to move NFT metadata off-chain and use a widely accepted standard for the metadata schema is a positive step for Marmalade tokens.
