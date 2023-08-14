(namespace (read-msg 'ns))

(module onchain-manifest-policy-v1 GOVERNANCE

  @doc "onchain manifest storage for marmalade-v2"
  (implements kip.token-policy-v2)

  (use kip.token-manifest)
  (use kip.token-policy-v2 [token-info])

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (defconst MANIFEST-SPEC-MSG-KEY:string "manifest_spec")

  (defschema manifest-spec
    manifest:object{manifest}
    guard:guard
  )

  (deftable manifests:{manifest-spec})

  (defcap UPGRADE (token-id:string)
    @managed
    (with-read manifests token-id {
      "guard":=manifest-guard
      }
      (enforce-guard manifest-guard)
    )
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade-v2.ledger.ledger-guard))
  )

  (defun get-manifest:object{manifest} (token-id:string)
    (with-read manifests token-id {
      "manifest":= manifest
    }
    manifest
  ))

  (defun upgrade-manifest
    ( token-id:string
      manifest:object{manifest}
    )
    (with-capability (UPGRADE token-id )
      (enforce-verify-manifest manifest)
      (update manifests token-id {
        "manifest": manifest
      })
    )
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "Exected at `create-token` step of marmalade.ledger.                  \
    \ Required msg-data keys:                                                  \
    \ * manifest_spec:object{manifest-spec} - registers the manifest object of \
    \ the token and the guard that manages the upgrade of the manifest on chain"
    (enforce-ledger)
    (let ( (manifest:object{manifest-spec} (read-msg MANIFEST-SPEC-MSG-KEY )) )
      (enforce-verify-manifest (at 'manifest manifest))
      (insert manifests (at 'id token) manifest)
    )
    true
  )


  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    true
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    true
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    true
  )


  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    true
  )


  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    true
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table manifests)
])
