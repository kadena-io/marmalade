(namespace (read-msg 'ns))

(module onchain-manifest-policy GOVERNANCE

  @doc "onchain manifest storage for marmalade-v2"
  (implements kip.token-policy-v2)

  (use kip.token-manifest)
  (use kip.token-policy-v2 [token-info])

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (defschema onchain-manifest
    manifest:{manifest}
    guard:guard
  )

  (deftable manifests:{onchain-manifest})

  (defcap UPGRADE (token-id:string)
    (with-read manifests token-id {
      "guard":=manifest-guard
      }
      (enforce-guard manifest-guard)
    )
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun upgrade-manifest
    ( token:object{token-info}
      manifest:{manifest}
    )
    (with-capability (UPGRADE (at 'id token) )
      (enforce-verify-manifest manifest)
      (update manifests (at 'id token) manifest)
    )
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let ( (manifest:{manifest} (read-msg 'manifest )) )
      (enforce-verify-manifest manifest)
      (insert manifests (at 'id token) manifest)
    )
  )


  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    true)

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    true)

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    true)

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    true)


  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    true)


  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    true)

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table manifests)
])
