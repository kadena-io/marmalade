(namespace (read-msg 'ns))

(module timed-mint-policy-v1 GOVERNANCE

  @doc "Policy for timed-mint tokens with royalty and quoted sale in coin."

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)

  (defschema timed-mint-schema
    max-supply:decimal
    mint-start-time:time
    mint-end-time:time
  )

  (deftable timed-mint:{timed-mint-schema})

  (defconst TIMED-MINT-SPEC:string "timed_mint_spec")

   (defun enforce-init:bool
     ( token:object{token-info}
     )
     (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) timed-mint-policy-v1))
     (let* ( (spec:object{timed-mint-schema} (read-msg TIMED-MINT-SPEC))
             (max-supply:decimal (at 'max-supply spec))
             (mint-start-time:time (at 'mint-start-time spec))
             (mint-end-time:time (at 'mint-end-time spec))
           )
       (insert timed-mint (at 'id token)
         { 'max-supply: max-supply
         , 'mint-start-time:mint-start-time
         , 'mint-end-time:mint-end-time
         }))
     true
   )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount timed-mint-policy-v1))
    (let* ( (account-bal:decimal (try 0.0 (at 'balance (marmalade-v2.ledger.details (at 'id token) account))))
            (timed-mint:object{timed-mint-schema} (get-timed-mint token))
            (max-supply:decimal (at 'max-supply timed-mint))
            (mint-start-time:time (at 'mint-start-time timed-mint))
            (mint-end-time:time (at 'mint-end-time timed-mint)) )
      (enforce (= account-bal 0.0) "Account has already minted")
      (enforce (>= (at 'block-time (chain-data))  mint-start-time) "Mint has not started yet")
      (enforce (< (at 'block-time (chain-data))  mint-end-time) "Mint has ended")
    )
  )

  (defun get-timed-mint:object{timed-mint-schema} (token:object{token-info})
    (read timed-mint (at 'id token))
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
    (enforce false "Transfer prohibited")
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
  true
  (create-table timed-mint)
)