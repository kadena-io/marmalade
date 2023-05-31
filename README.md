# Kadena Marmalade V2: Mint a Marketplace

## Decentralized Infrastructure for Poly-Fungibles and NFTs

This repo houses smart contracts and frontend code for Marmalade, Kadena's platform for poly-fungibles and NFTs.

### Marmalade Ledger

The main contract in Marmalade is `marmalade.ledger`. This contract stores the token information, token's accounts, and the policies associated with it. The main functions, `create-token`, `mint`, `burn`, `transfer`, `sale`. Token policies can customise to allow one/both/none of `transfer` and `sale` as a way of transferring. `transfer` allows a direct transfer, and `sale` allows escrowed transfer with timeout.

### Policy manager

Marmalade V2's new feature is a policy manager. Marmalade tokens now store multiple policies in a `token-policies` format, instead of a single policy.
The policy manager makes a distinction between 3 types of policies, concrete, immutable, and adjustable which are explained in detail below. Policy manager acts as a middleware between policies, and runs the `policy::enforce-**` functions.

## Using Policies

### Token Policies

```
  (defschema concrete-policy
    non-fungible-policy:bool
    quote-policy:bool
    royalty-policy:bool
    collection-policy:bool
  )
```
```
  (defschema token-policies
    concrete-policies:object{concrete-policy}
    immutable-policies:[module{token-policy-v2}]
    adjustable-policies:[module{token-policy-v2}]
  )
```

- `concrete-policies` store boolean values that represent if the token uses the concrete-policy or not. Immutable.
- `immutable-policies` store additional immutable policies that the token chooses to be bound with.
- `adjustable-policies` store policies that can be rotated by the token owner. Fractional tokens cannot rotate.

### Concrete Policies

Marmalade V2 aims to make token creation simple and convenient, yet still offer the rich features using concrete-policies. A concrete policy is a simple basic implementation of some of the most used features in token creation.
We provide 4 concrete policies, which will provide the most used functionalities.

- **Collection Policy**: Initiates a collection with pre-defined token lists
- **Fungible Quote Policy**: Provides a sale of NFT with fungibles using escrow account
- **Non-fungible Policy**: Defines the token supply to 1 and precision of 0, so the token becomes non-fungible
- **Royalty-policy**: [dependent on `fungible-quote-policy`]: Defines creator account that will receive royalty whenever the token using `fungible-quote-policy` is sold.

Marmalade users can mint tokens with above features by adding `true` or `false` next to the policy fields in `token-policies`. If projects would like to use customized logic in addition to what concrete policies offer, they can turn off the concrete policy and add additional policies to the `immutable-policies` , or `adjustable-policies` field.

## Marmalade Functions

### Create Token

A Token is created in marmalade via running `create-token`. Arguments include:

- `id`: token-id, formatted in `t:{token-detail-hash}`. Should be created using `create-token-id`
- `precision`: Number of decimals allowed for for the token amount. For one-off token, precision must be 0, and should be enforced in the policy's `enforce-init`.
- `uri`: url to external JSON containing metadata
- `policies`: policies contract with custom functions to execute at marmalade functions

`policy-manager.enforce-init` calls `policy:enforce-init` in stored token-policies, and the function is executed in `ledger.create-token`.

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

`policy-manager.enforce-offer` calls `policy:enforce-offer` in stored token-policies, and the function is executed at step 0 of `sale`.

#### withdraw (cont)

Step 0-rollback executes `withdraw`. `withdraw` transfers token from the escrow back to the seller. `withdraw` can be executed after timeout, by sending in `cont` command with `rollback: true`, `step: 0`. Formatting `cont` commands can be read in [here](https://pact-language.readthedocs.io/en/latest/pact-reference.html?highlight=continuation#yaml-continuation-command-request)

`policy-manager.enforce-withdraw` calls `policy:enforce-withdraw` in stored token-policies, and the function is executed at step 0-rollback of `sale`.

#### buy (cont)

Step 1 executes `buy`. `buy` transfers token from the escrow to the buyer. `buy` can be executed before `timeout`. The `buyer` and `buyer-guard` information is read from the `env-data` of the command instead of passing in arguments. Just like `withdraw`, `buy` is executed using `cont` command with `rollback:false`, `step: 0`.

`policy-manager.enforce-buy` calls `policy:enforce-buy` in stored token-policies, and the function is executed at step 1 of `sale`.

## Policies

Marmalade Policies allow customised rules for token operations.

#### Concrete Policies:

- [Collection Policy](./pact/concrete-policies/collection-policy/collection-policy-v1.pact) ([docs](./pact/concrete-policies/collection-policy/collection-policy-v1.md))
- [Fungible Quote Policy](./pact/concrete-policies/fungible-quote-policy/fungible-quote-policy-v1.pact) ([docs](./pact/concrete-policies/fungible-quote-policy/fungible-quote-policy-v1.md))
- [Non-Fungible Policy](./pact/concrete-policies/non-fungible-policy/non-fungible-policy-v1.pact) ([docs](./pact/concrete-policies/non-fungible-policy/non-fungible-policy-v1.md))
- [Royalty Policy](./pact/concrete-policies/royalty-policy/royalty-policy-v1.pact) ([docs](./pact/concrete-policies/royalty-policy/royalty-policy-v1.md))

#### Regular Policies:

- [Fixed Issuance Policy]("./pact/policies/fixed-issuance-policy/fixed-issuance-policy.pact) ([docs](./pact/policies/fixed-issuance-policy/fixed-issuance-policy.md))
- [Guard Policy]("./pact/policies/guard-policy/guard-policy.pact) ([docs](./pact/policies/guard-policy/guard-policy.md))
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

### Why the Manifest was Replaced by URI 

We decided to replace the manifest schema with a URI-based schema. The new schema for NFT metadata is a simple JSON schema that describes the properties of the metadata. This schema enables compatibility with various marketplaces and wallets, making Marmalade tokens more interoperable. By utilising a URI-based schema, Marmalade tokens can improve scalability, and provide greater flexibility for developers and most of all simplicity of usage in general.

The decision to move NFT metadata off-chain and use a widely accepted standard for the metadata schema is a positive step for Marmalade tokens.






## IPFS Storage Guide

This guide provides our recommend approach to storing metadata and image assets on IPFS, leveraging hypothetical paths and CIDs. Our manual illustrates two distinctive storage scenarios and outlines the method for accessing stored data.

### Storing Collections: Step-by-Step Guide

1.  **Image Upload to IPFS:**
    
    -  Uploading your image assets folder to IPFS, adopting sequential numbering for streamlined referencing (e.g., "1.jpg, 2.jpg...").
2.  **Metadata Update:**
    
    -  After the upload, capture the CID for the assets folder (e.g., "Bayfol...").
    -  Proceed to update the metadata files, correlating the image property with the path to CID (e.g., "ipfs://Bayfol.../1.jpg").
3.  **Metadata Upload to IPFS:**
    
    -  Upload the metadata files to IPFS, maintaining sequential numbering that corresponds with the asset (e.g., "1.json, 2.json...").
    -  Retrieve the CID for the uploaded metadata folder (e.g., "Baymetx...).
4.  **Finalizing URI:**
    
    -  Merge the metadata folder CID (e.g., "Baymetx...") with the respective filename and extension to construct a comprehensive URI (e.g., "ipfs://Baymetx.../1.json").
    -  Forge the final URI for the token on the ledger to the combined CID (e.g., "ipfs://Baymetx.../1.json")

### Example:

 - **uri:** [ipfs://bafybeig4ihtm2phax2eodfpubwy467szuiieqafkoywp5khzt6cz2hqrna/1.json](ipfs://bafybeig4ihtm2phax2eodfpubwy467szuiieqafkoywp5khzt6cz2hqrna/1.json)
   
 - **gateway:** [[click here]](https://bafybeig4ihtm2phax2eodfpubwy467szuiieqafkoywp5khzt6cz2hqrna.ipfs.dweb.link/1.json)


 - **collection-asset-folder:** ipfs://bafybeie4ktsgx4x3gnpvo2uptngez4cvvqdq75iimpnukvpee2x34yp6jm

   
 - **collection-asset-folder-gateway:** [[click here]](https://bafybeie4ktsgx4x3gnpvo2uptngez4cvvqdq75iimpnukvpee2x34yp6jm.ipfs.dweb.link/)
 
 - **collection-metadata-folder:** ipfs://bafybeig4ihtm2phax2eodfpubwy467szuiieqafkoywp5khzt6cz2hqrna


 - **collection-metadata-folder-gateway:** [[click here]](https://bafybeig4ihtm2phax2eodfpubwy467szuiieqafkoywp5khzt6cz2hqrna.ipfs.dweb.link/)




### Single NFT Storage: Step-by-Step Guide

1.  **Image and Metadata Upload to IPFS:**
    
    -  Upload the image asset to IPFS.
2.  **Metadata Update:**
    
    -  Upon successful upload, retrieve the CID for the asset (e.g., "Bayfabc...").
    -  Revise the metadata files, matching the image property with the path to CID (e.g., "ipfs://Bayfabc.../1.jpg").
3.  **Metadata Upload to IPFS:**
    -  Upload the metadata file to IPFS.
4.  **Finalizing URI:**
    -  Retrieve the path containing the CID for the uploaded metadata file (e.g., "ipfs//Bayfxyz.../metadata.json")

### Example:

 - **uri:** [ipfs://bafyreiainnf575ivbxffep3xqx4d4v2jrpyz4yrggylfp5i7lru7zpfese/metadata.json](ipfs://bafyreiainnf575ivbxffep3xqx4d4v2jrpyz4yrggylfp5i7lru7zpfese/metadata.json)
   
 - **gateway-link:** [[click here]](https://bafyreiainnf575ivbxffep3xqx4d4v2jrpyz4yrggylfp5i7lru7zpfese.ipfs.dweb.link/metadata.json)

### Metadata Structure

Your metadata files should adhere to our JSON schema. The schema provides a structure for your metadata, ensuring that necessary details are present and formatted correctly. This schema can be found within this readme.

In this schema, the `image` property should contain a link to the image on IPFS (as illustrated in the previous examples).

### Token Creation in the Ledger

When creating a token in the ledger, you should use the `create-token` function. The link obtained from IPFS (.json) serves as the URI supplied to create a token within the ledger:


    (defun create-token:bool
        ( id:string
          precision:integer
          uri:string
          policies:object{token-policies}
        )
        ...
    )

Please be reminded that these CIDs are hypothetical and should be tailored to match your specific use case and IPFS setup. A thorough understanding of the IPFS storage mechanism is crucial, and the steps should be adjusted as necessary.

By faithfully following these detailed steps, you can efficiently store metadata and image assets on IPFS, associate them with NFTs, and seamlessly retrieve them in your DApp or application.

### URI retrieval from Ledger

Retrieving the URI for a specific token from the ledger is facilitated through a function called `get-uri`. This function requires a token ID as its argument and returns the associated URI.

    (defun get-uri:string (id:string)
      (at 'uri (read tokens id))
    )

When you call the `get-uri` function and pass in a token ID, it will access the `tokens` map, find the row corresponding to the provided token ID, and return the value stored in the `'uri` field of that row. Essentially, it retrieves the URI that corresponds to the token ID you specified.

Thus, by utilising this `get-uri` function, you can efficiently retrieve the URI associated with any token stored within the ledger by simply providing its token ID.
