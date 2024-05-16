(namespace (read-string 'ns))

(module non-updatable-uri-policy-v1 GOVERNANCE

  @doc "Concrete policy for issuing an nft without updatability of URI"

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (implements kip.updatable-uri-policy-v1)

  (use kip.token-policy-v2 [token-info])

  (defun enforce-init:bool
    ( token:object{token-info} )
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal )
    true
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal )
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
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

  (defun enforce-update-uri:bool
    ( token:object{token-info}
      new-uri:string )
    (enforce false "Update URI Prohibited")
  )

)

(enforce-guard ADMIN-KS)
