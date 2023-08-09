(namespace (read-msg 'ns))

(module fixed-issuance-policy-v1 GOVERNANCE

  @doc "Policy for minting with a fixed issuance"

  (defcap GOVERNANCE ()
    (enforce-guard "marmalade-v2.marmalade-admin"))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])

  (defconst FIXED-ISSUANCE-SPEC:string "fix_issuance_spec")

  (defschema supply-schema
    max-supply:decimal
    min-amount:decimal
    precision:integer
  )

  (deftable supplies:{supply-schema})

  (defun get-supply:object{supply-schema} (token:object{token-info})
    (read supplies (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade-v2.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc ""
    (enforce-ledger)
    (let* (
            (fixed-issuance-spec:object{supply-schema} (read-msg FIXED-ISSUANCE-SPEC))
            )
      (enforce (>= (at 'min-amount fixed-issuance-spec) 0.0) "Invalid min-amount")
      (enforce (>= (at 'max-supply fixed-issuance-spec) 0.0) "Invalid max-supply")
      (enforce (= (at 'precision fixed-issuance-spec) (at 'precision token)) "Invalid Precision")
      (insert supplies (at 'id token)
        { 'max-supply: (at 'max-supply fixed-issuance-spec)
        , 'min-amount: (at 'min-amount fixed-issuance-spec) }))
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (bind (get-supply token)
      { 'min-amount:=min-amount:decimal
      , 'max-supply:=max-supply:decimal
      }
      (enforce (>= amount min-amount) "mint amount < min-amount")
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
  ))

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

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
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
  [(create-table supplies) ])
