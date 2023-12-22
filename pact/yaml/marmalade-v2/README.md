# Marmalade - V2 Deployment

## Data file
The YAML data files includes parameters for deployment:

Example for testnet:
```yaml
network: testnet04
chain: 1
marmalade_namespace: marmalade-v2
kip_namespace: kip
is_upgrade: false
sender: dfb16b13e4032a6878fd98506b22cb0d6e5932c541e656b7ee5d69d72e6eb76e
sender_key: dfb16b13e4032a6878fd98506b22cb0d6e5932c541e656b7ee5d69d72e6eb76e
```

## YAML transactions files

The current deployment YAML files assume that only 1 signer is needed:
  - For gas payment
  - For namespaces unlocking

Others signers can be added if needed

## Transactions (JSON) files

Transaction files can be generated and submitted by:

```sh
kda gen -t XXX.yaml -d data_file.yaml

# Depending on your signature method
kda sign tx.yaml --chainweaver
# or
kda sign tx.yaml -k key.yaml
# or
# ...

# Then submit
kda send tx.json
```


## Deployment Order / Dependencies

The deployment order must be respected to fulfill all modules dependencies.

* In the KIP Namespace

  1. token-policy-v2
  2. poly-fungible-v3

* In the marmalade Namespace

  3. ledger-v1
  4. sale-v1
  5. policy-manager
  6. ledger
  7. Concrete policies
    * collection-policy-v1
    * guard-policy-v1
    * non-fungible-policy-v1
    * royalty-policy-v1
  9. policy-init **(Don't forget this one)*
  10. util-v1

Ideally, verify that each transaction has been fully mined and has succeed between each step.

## Note about the KIP namespace

Most marmalade contracts include fully qualified links to the KIP deployed interfaces.

If the `token-policy-v2` and `poly-fungible-v3` are deployed in another namespace than `kip`, the links have to be manually fixed before deployment, in each Pact module.

## Note about upgrading modules

Don't forget to set `is_upgrade` to `true` in the YAML data file
