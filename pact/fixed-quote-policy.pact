(namespace (read-msg 'ns))

(module fixed-quote-policy GOVERNANCE

  @doc "Policy for fixed issuance with simple quoted sale."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'hft-admin )))

  (implements token-policy-v1)

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
    token:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (defun init-fqp
    ( token:string
      mint-guard:guard
      max-supply:decimal
    )
    (insert policies token
      { 'mint-guard: mint-guard
      , 'max-supply: max-supply })
  )

  (defun get-policy:object{token-schema} (token:object{token-info})
    (read policies (at 'token token))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (bind token token:string
    supply:decimal
    precision:integer
    manifest:object{token-manifest.manifest}
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'max-supply:=max-supply:decimal
      }
      (bind token )
      (enforce-guard (at 'mint-guard (get-policy token)))
      (enforce (<= (+ (at 'supply token))))
    (enforce ())
  )

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
    (get-policy token)
    true
  )


  (defun init-sale
    ( token:string
      seller:string
      amount:decimal
      sale:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-sale-pact sale)
    (let ( (spec:object{quote-spec} (read-msg QUOTE) ) )
      (insert quotes sale { 'token: token, 'spec: spec }))
  )

  (defun enforce-sale:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      amount:decimal
      sale:string )
    (enforce-sale-pact sale)
    (with-read quotes sale { 'token:= qtoken, 'spec:= spec:object{quote-spec} }
      (enforce (= qtoken (at 'token token)) "incorrect sale token")
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
    ( token:object{token-info}
      sender:string
      receiver:string
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
