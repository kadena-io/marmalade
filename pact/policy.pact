
(namespace (read-msg 'ns))

(interface token-policy-v1

  (defschema token-info
    token:string
    supply:decimal
    precision:integer
    manifest:object{token-manifest.manifest})

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
    ( token:object{token-info} )
    @doc "Enforce that TOKEN policy is initialized"
    )

  (defun enforce-sale:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      amount:decimal
      sale:string )
    @doc "Enforce rules on SALE by SELLER to BUYER AMOUNT of TOKEN."
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      receiver:string
      amount:decimal )
    @doc "Enforce rules on transfer of TOKEN AMOUNT from SENDER to RECEIVER."
  )
)

(module guard-token-policy GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'hft-admin )))

  (implements token-policy-v1)

  (defschema guards
    mint-guard:guard
    burn-guard:guard
    sale-guard:guard
    transfer-guard:guard
  )

  (deftable policy-guards:{guards})

  (defun init-guards
    ( token:string
      mint-guard:guard
      burn-guard:guard
      sale-guard:guard
      transfer-guard:guard
    )
    (insert policy-guards token
      { 'mint-guard: mint-guard, 'burn-guard: burn-guard
      , 'sale-guard: sale-guard, 'transfer-guard: transfer-guard })
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

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (get-guards token)
    true
  )

  (defun enforce-sale:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      amount:decimal
      sale:string )
    (enforce-guard (at 'sale-guard (get-guards token)))
  )


  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      receiver:string
      amount:decimal )
    (enforce-guard (at 'transfer-guard (get-guards token)))
  )
)

(create-table policy-guards)
