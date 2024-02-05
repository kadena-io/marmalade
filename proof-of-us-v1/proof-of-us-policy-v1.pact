(namespace (read-msg 'ns))

(module proof-of-us-policy-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)
  (use marmalade-v2.collection-policy-v1)
  (use marmalade-v2.proof-of-us-v1)
  (use marmalade-v2.util-v1 [curr-time])

  (defconst EVENT-ID-MSG-KEY:string "event_id")

  (defun has-collection-policy:bool (policies)
    (> (length (filter (lambda (policy) (= policy marmalade-v2.collection-policy-v1)) policies)) 0))

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "The function is run at `create-token` step of marmalade-v2.ledger.create-token"

    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) proof-of-us-policy-v1))

    (enforce (= (at 'precision token) 0) "Precision must be 0 for proof-of-us tokens")

    (enforce (has-collection-policy (at 'policies token)) "Collection policy is required for proof-of-us tokens")
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount proof-of-us-policy-v1))

    (enforce (= amount 1.0) "Amount must be 1.0 for proof-of-us tokens")

    (validate-event (read-msg EVENT-ID-MSG-KEY))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn is not allowed!")
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

(enforce-guard ADMIN-KS)