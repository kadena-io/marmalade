(namespace (read-msg 'ns))

(module fixed-quote-policy GOVERNANCE

  @doc "Policy for fixed issuance with simple quoted sale."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1_DRAFT3)
  (use kip.token-policy-v1_DRAFT3 [token-info])

  (defschema policy-schema
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
  )

  (deftable policies:{policy-schema})

  (defconst QUOTE "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    fungible:module{fungible-v2}
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

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
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
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (insert policies (at 'id token)
      { 'mint-guard: (read-keyset 'mint-guard)
      , 'max-supply: (read-decimal 'max-supply)
      , 'min-amount: (read-decimal 'min-amount) })
    true
  )


  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-sale-pact sale-id)
    (let* ( (spec:object{quote-spec} (read-msg QUOTE))
            (fungible:module{fungible-v2} (at 'fungible spec) )
            (price:decimal (at 'price spec)) )
      (fungible::enforce-unit price)
      (enforce (< 0.0 price) "Offer amount must be positive")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      amount:decimal
      sale-id:string )
    (enforce-sale-pact sale-id)
    (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
      (enforce (= qtoken (at 'id token)) "incorrect sale token")
      (bind spec
        { 'fungible := fungible:module{fungible-v2}
        , 'price := price:decimal
        , 'recipient := recipient:string
        , 'recipient-guard := recipient-guard:guard
        }
        (fungible::transfer-create buyer recipient recipient-guard (* amount price))
      )
    )
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
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      receiver:string
      target-chain:string
      amount:decimal )
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
