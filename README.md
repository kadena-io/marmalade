# Kadena Marmalade V2: Mint a Marketplace

## Decentralized Infrastructure for Poly-Fungibles and NFTs

This repo houses smart contracts and frontend code for Marmalade, Kadena's platform for poly-fungibles and NFTs.

[[MARMALADE DOCS]](https://docs.kadena.io/marmalade)
[[MARMALADE Contract Reference]](https://docs.kadena.io/reference/nft-ref)

## Setup Marmalade and Spirekey
Run the following commands to deploy marmalade and spirekey on devnet:

```bash
git clone git@github.com:kadena-io/marmalade.git
docker volume create l1
docker run --rm -it -p 8080:8080 -e MINING_BATCH_PERIOD=0.5 -e MINING_CONFIRMATION_PERIOD=0.5 -v l1:/data kadena/devnet:latest
pnpm i
```
