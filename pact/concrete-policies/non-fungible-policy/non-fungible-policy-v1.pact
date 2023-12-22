(namespace (read-string 'ns))

(module non-fungible-policy-v1 GOVERNANCE

  @doc "Concrete policy for issuing an nft with a fixed supply of 1 and precision of 0"

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use policy-manager)
  (use kip.token-policy-v2 [token-info])

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) non-fungible-policy-v1))
    (enforce (= 0 (at 'precision token)) "Precision must be 0")
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount non-fungible-policy-v1))
    (enforce (= amount 1.0) "Mint can only be 1")
    (enforce (= (at 'supply token) 0.0) "Only one mint allowed")
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn prohibited")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
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

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
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

)

(enforce-guard ADMIN-KS)
