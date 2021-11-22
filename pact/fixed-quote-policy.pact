(namespace (read-msg 'ns))

(module fixed-quote-policy GOVERNANCE

  @doc "Policy for fixed issuance with simple quoted sale."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'hft-admin )))

  (implements token-policy-v1_DRAFT1)

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

  (defun init-fqp
    ( id:string
      mint-guard:guard
      max-supply:decimal
      min-amount:decimal
    )
    (insert policies id
      { 'mint-guard: mint-guard
      , 'max-supply: max-supply
      , 'min-amount: min-amount })
  )

  (defun get-policy:object{policy-schema} (token:object{token-policy-v1_DRAFT1.token-info})
    (read policies (at 'id token))
  )

  (defun enforce-mint:bool
    ( token:object{token-policy-v1_DRAFT1.token-info}
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
    ( token:object{token-policy-v1_DRAFT1.token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-policy-v1_DRAFT1.token-info}
    )
    (get-policy token)
    true
  )


  (defun init-sale:bool
    ( token:object{token-policy-v1_DRAFT1.token-info}
      seller:string
      amount:decimal
      sale:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-sale-pact sale)
    (let ( (spec:object{quote-spec} (read-msg QUOTE) ) )
      (insert quotes sale { 'id: (at 'id token), 'spec: spec }))
  )

  (defun enforce-sale:bool
    ( token:object{token-policy-v1_DRAFT1.token-info}
      seller:string
      buyer:string
      amount:decimal
      sale:string )
    (enforce-sale-pact sale)
    (with-read quotes sale { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
      (enforce (= qtoken (at 'id token)) "incorrect sale token")
      (bind spec
        { 'fungible := fungible:module{fungible-v2}
        , 'price := price:decimal
        , 'recipient := recipient:string
        , 'recipient-guard := recipient-guard:guard
        }
        (fungible::transfer-create buyer recipient recipient-guard price)
      )
    )
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-policy-v1_DRAFT1.token-info}
      sender:string
      receiver:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )
)


(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table policies) ])
