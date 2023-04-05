
# Marmalade v2 Metadata Standard
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
		policy:module{kip.token-policy-v1}
	)

- uri: A string representing the URI that points to an external JSON-Schema for the NFT.


## Off-chain Schema
The off-chain schema contains properties that describe the metadata of an NFT stored off-chain in a JSON format. The schema includes the following fields:


|**Field Name**|**Data Type**|**Description**|
| :- | :-: | :-: |
|name|string|Identifies the asset to which this NFT represents|
|description|string|Describes the asset to which this NFT represents|
|image|string|A URI pointing to a resource with mime type image/\* representing the asset to which this NFT represents. Consider making any images at a width between 320 and 1080 pixels and aspect ratio between 1.91:1 and 4:5 inclusive.|
|\*properties|object|Arbitrary properties. Values may be strings, numbers, object or arrays.|
|\*authors|array of objects|An array of authors who created or contributed to the asset. Each author is an object with a "name" field specifying the author's name.|
|\*external\_url|string|URL to an external application or website where users can also view the asset|
|\*animation\_url|string|URL to a multimedia attachment of the asset. The supported file formats are MP4 and MOV for video, MP3, FLAC and WAV for audio, GLB for AR/3D assets, and HTML for HTML pages. You may use the ?ext={file\_extension} query to provide information on the file type.|
|\*collection|object|an object with a "name" field specifying the name of the collection, and a “family” field specifying the larger category or group to which the collection belongs.|

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
    	policy:module{kip.token-policy-v1}
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

Additionally, the schema allows for arbitrary properties to be added to the metadata, providing developers with more flexibility in customising the metadata.  The decision to move NFT metadata off-chain and use a widely accepted standard for the metadata schema is a positive step for Marmalade tokens.



