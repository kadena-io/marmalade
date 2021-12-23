(namespace (read-msg 'ns))

(module fixed-quote-policy-royalty GOVERNANCE

  @doc "Policy for fixed issuance with simple quoted sale."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-ns-admin )))

  (implements kip.token-policy-v1_DRAFT3)
  (use kip.token-policy-v1_DRAFT3 [token-info])

  (defschema policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
    royalty-rate:decimal
  )

  (deftable policies:{policy-schema})

  (defconst QUOTE "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
   )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'max-supply:=max-supply:decimal
      }
      (enforce-guard (at 'mint-guard (get-policy token)))
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
  ))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let* ( (fungible:module{fungible-v2} (read-msg 'fungible ))
            (creator:string (read-msg 'creator ))
            (creator-guard:guard (read-keyset 'creator-guard ))
            (mint-guard:guard (read-keyset 'mint-guard ))
            (max-supply:decimal (read-decimal 'max-supply ))
            (min-amount:decimal (read-decimal 'min-amount ))
            (royalty-rate:decimal (read-decimal 'royalty-rate ))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "royalty rate is not valid")
      (insert policies (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'mint-guard: mint-guard
        , 'max-supply: max-supply
        , 'min-amount: min-amount
        , 'royalty-rate: royalty-rate }))
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient)) )
      (fungible::enforce-unit price)
      (enforce (< 0.0 price) "Offer amount must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
  ))

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'creator-guard:=creator-guard:guard
      , 'royalty-rate:= royalty-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          , 'recipient-guard := recipient-guard:guard
          }
          (fungible::transfer-create buyer creator creator-guard (* (* amount price) royalty-rate))
          (fungible::transfer-create buyer recipient recipient-guard (* (* amount price) (- 1 royalty-rate)))
        )
      ))
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  ;; dummy impl to address #928
  (implements gas-payer-v1)
  (defcap GAS_PAYER:bool
    ( user:string limit:integer price:decimal )
    (enforce false "Dummy implementation"))
  (defun create-gas-payer-guard:guard ()
    (enforce false "Dummy implementation"))
)


(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table policies) ])
