
(namespace (read-msg 'ns))

(interface token-policy-v1

  (defschema token-info
    id:string
    supply:decimal
    precision:integer)

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    @doc "Minting policy for TOKEN"
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    @doc "Burning policy for TOKEN"
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )

  (defun enforce-init:bool
    (id:string)
    @doc "Enforce that token ID policy is initialized"
    )
)

(module guard-token-policy GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'hft-admin )))

  (implements token-policy-v1)

  (defschema guards
    mint-guard:guard
    burn-guard:guard
  )

  (deftable policy-guards:{guards})

  (defun init-guards (id:string mint-guard:guard burn-guard:guard)
    (insert policy-guards id
      { 'mint-guard: mint-guard, 'burn-guard: burn-guard })
  )

  (defun get-guards:object{guards} (token:object{token-info})
    (read policy-guards (at 'id token))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-guard (at 'mint-guard (get-guards token)))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-guard (at 'burn-guard (get-guards token)))
  )

  (defun enforce-init:bool
    ( id:string
    )
    (read policy-guards id)
    true
  )
)

(create-table policy-guards)
