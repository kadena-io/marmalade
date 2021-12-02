# Kadena Marmalade: Mint a Marketplace

## Decentralized Infrastructure for Poly-Fungibles and NFTs

This repo houses smart contracts and frontend code for Marmalade, Kadena's platform for poly-fungibles and NFTs.

## Key features:

* **Poly-Fungible v2**. This is the new standard that improves on ERC-1155, namely by adding `sale`, the trustless-escrow pact for selling tokens on-chain.
* **Haber Content Manifests.** This allows rich, verifiable data to be stored with the token, after Stuart Haber's work on Content Integrity Services (2006).
* **Pluggable Token Policies.** This is how creators "mint marketplaces" by specifying and tightly controlling all aspects of a token marketplace through enforceable token policies for minting, burning, transfer and sale.
* **Content-addressed identifiers**. This is a WIP for better NFT identifiers, leveraging Haber Content Manifests. WIP includes integrating this concept with DIDs.

## Testnet Roadmap

 [ ] Creation: Manifests and minimal token specification
 [ ] Minting and Burning
 [ ] Sale execution

## Deployment
UI deployment: https://kadena-io.github.io/marmalade/

