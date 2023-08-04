(namespace (read-msg 'ns))

(module non-fungible-policy-v1 GOVERNANCE

  @doc "Concrete policy for issuing an nft with a fixed supply of 1 and precision of 0"

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (enforce (= 0 (at 'precision token)) "Precision must be 0")
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
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
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
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
    (enforce-ledger)
  )
)
