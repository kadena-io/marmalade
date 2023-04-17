(namespace (read-msg 'ns))

;; DRAFT - discuss whether this should be in concrete policy

(module one-off-policy GOVERNANCE

  @doc "Policy for one-off policies "

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade.policy-manager)

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    ; (require-capabiltiy (CREATE_AND_MINT))
    (enforce-ledger)
    (enforce (= 0 (at 'precision token) ""))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    ; (require-capabiltiy (CREATE_AND_MINT))
    (enforce-ledger)
    (enforce (<= (+ amount (at 'supply token)) 1) "Exceeds max supply")
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    ;;TODO
    true
  )
)


(if (read-msg 'upgrade)
  ["upgrade complete"]
  [  ])
