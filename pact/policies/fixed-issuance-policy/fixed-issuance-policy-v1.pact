(namespace (read-msg 'ns))

(module fixed-issuance-policy-v1 GOVERNANCE

  @doc "Policy for minting with a fixed issuance"

  (defcap GOVERNANCE ()
    (enforce-guard "marmalade-v2.marmalade-admin"))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)

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

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "The function is run at `create-token` step of marmalade.ledger.      \
    \ Required msg-data keys:                                                  \
    \ * fixed_issuance_spec:object{supply-schema} - registers minimum mint     \
    \ amount, max-supply, and precision information of the created token"
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) fixed-issuance-policy-v1))
    (let* (
            (fixed-issuance-spec:object{supply-schema} (read-msg FIXED-ISSUANCE-SPEC))
            )
      (enforce (> (at 'min-amount fixed-issuance-spec) 0.0) "Invalid min-amount")
      (enforce (>= (at 'max-supply fixed-issuance-spec) 0.0) "Invalid max-supply")      
      (enforce (= (at 'precision fixed-issuance-spec) (at 'precision token)) "Invalid Precision")
      (insert supplies (at 'id token) fixed-issuance-spec))
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount fixed-issuance-policy-v1))
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
)


(if (read-msg 'upgrade)
  ["upgrade complete"]
  [(create-table supplies) ])
