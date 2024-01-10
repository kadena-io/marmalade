(namespace (read-msg 'ns))

(module soul-bound-policy-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)

  (defschema mint-record 
    account:string
  )

  (deftable records:{mint-record})

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "The function is run at `create-token` step of marmalade-v2.ledger."

    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) soul-bound-policy-v1))

    (enforce (= (at 'precision token) 0) "Precision must be 0 for soul-bound tokens")

    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount soul-bound-policy-v1))

    (enforce (= amount 1.0) "Amount must be 1.0 for soul-bound tokens")

    (let* ((account-balance (try 0.0 (at 'balance (marmalade-v2.ledger.details (at 'id token) account) )))
          (mint-record (try { 'account: "" } (read records (at 'id token))))
          (has-mint-record (try false (!= (at 'account mint-record) ""))))
      (enforce (= account-balance 0.0) "Token was already minted")
      (enforce (not has-mint-record) "Token was already minted")
    )

    (insert records (at 'id token) { 'account: account })

    true
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (require-capability (BURN-CALL (at "id" token) account amount soul-bound-policy-v1))

    (enforce (= amount 1.0) "Amount must be 1.0 for soul-bound tokens")

    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer is not allowed!")
  )
)

(if (read-msg 'upgrade)
  true
  (create-table records)
)