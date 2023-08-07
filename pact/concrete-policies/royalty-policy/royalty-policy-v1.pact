(namespace (read-string 'ns))

(module royalty-policy-v1 GOVERNANCE

  @doc "Concrete policy to support royalty payouts in a specified fungible during sale."

  (defconst GOVERNANCE-KS:string (+ (read-string 'ns) ".marmalade-admin"))

  (defcap GOVERNANCE ()
    (enforce-keyset GOVERNANCE-KS))

  (use policy-manager)
  (use quote-manager)
  (use quote-manager [quote-spec quote-schema])
  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])


  (defschema royalty-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
  )

  (deftable royalties:{royalty-schema})

  (defconst ROYALTY_SPEC "royalty_spec"
    @doc "Payload field for token spec")

  (defun get-royalty:object{royalty-schema} (token:object{token-info})
    (read royalties (at 'id token))
  )

  (defcap ROYALTY:bool
    ( sale-id:string
      token-id:string
      royalty-payout:decimal
      creator:string
    )
    @event
    true
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (ledger.ledger-guard)))

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let* ( (spec:object{royalty-schema} (read-msg ROYALTY_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert royalties (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'royalty-rate: royalty-rate
        }))
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    true
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
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-royalty token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (let* ( (quote:object{quote-schema} (get-quote-info sale-id))
              (spec:object{quote-spec} (at 'spec quote))
              (price:decimal (at 'price spec))
              (sale-price:decimal (* amount price))
              (escrow-account:string (at 'account (get-escrow-account sale-id)))
              (royalty-payout:decimal
                 (floor (* sale-price royalty-rate) (fungible::precision))))
        (enforce (= (at 'token-id quote) (at 'id token)) "incorrect sale token")
        (if
          (> royalty-payout 0.0)
          [ (install-capability (fungible::TRANSFER escrow-account creator royalty-payout))
            (emit-event (ROYALTY sale-id (at 'id token) royalty-payout creator))
            (fungible::transfer escrow-account creator royalty-payout)
          ]
          "No royalty"
          )))
        true)

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    true
  )
)


(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table royalties)])
