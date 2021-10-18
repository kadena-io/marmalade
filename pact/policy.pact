
(namespace (read-msg 'ns))

(interface token-policy-v1

  (defschema token-info
    token:string
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
    (token:string)
    @doc "Enforce that the token is initialized"
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

  (defun init-guards (token:string mint-guard:guard burn-guard:guard)
    (insert policy-guards token
      { 'mint-guard: mint-guard, 'burn-guard: burn-guard })
  )

  (defun get-guards:object{guards} (token:object{token-info})
    (read policy-guards (at 'token token))
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

  (defun check-init:bool
    ( token:string
    )
    (try false (!= (length (read policy-guards token)) 0))
  )

  (defun enforce-init:bool
    ( token:string
    )
    (let ((init (check-init token)))
      (enforce init "Token is not initialized"))
  )

)

(create-table policy-guards)
