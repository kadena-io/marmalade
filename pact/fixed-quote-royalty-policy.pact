(namespace (read-msg 'ns))

(module fixed-quote-royalty-policy GOVERNANCE

  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
    royalty-rate:decimal
    owner:string
    latest-spec
  )

  (deftable policies:{policy-schema})

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst MINT_PRICE 1.1
    @doc "MINT_PRICE")

  (defconst ADMIN_ADDRESS "admin"
    @doc "admin address which also recieves mint payouts")

  (defconst QUOTE-MSG-KEY "quote"
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


  ;;;;;;;;;;;;; DEFCAPS

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

;;;;;;;;; caps
  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    true
  )

  (defcap UPDATE-QUOTE (token-id:string new-latest)
    true
  )

  (defcap BUY (id:string receiver:string)
   (compose-capability (UPDATE-OWNER id receiver))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
   )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'min-amount:=min-amount:decimal
      , 'max-supply:=max-supply:decimal
      }
      (enforce-guard mint-guard)
      (enforce (>= amount min-amount) "mint amount < min-amount")
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
      (coin.transfer account ADMIN_ADDRESS MINT_PRICE)
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
    (let* ( (spec:object{policy-schema} (read-msg TOKEN_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (mint-guard:guard (at 'mint-guard spec))
            (max-supply:decimal (at 'max-supply spec))
            (min-amount:decimal (at 'min-amount spec))
            (owner:string (at 'owner spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (>= min-amount 0.0) "Invalid min-amount")
      (enforce (>= max-supply 0.0) "Invalid max-supply")
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert policies (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'mint-guard: mint-guard
        , 'max-supply: max-supply
        , 'min-amount: min-amount
        , 'royalty-rate: royalty-rate
        , 'owner: owner
        , 'latest-spec: {} }))
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
       ,'royalty-rate:= royalty-rate:decimal
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal
               (floor (* sale-price royalty-rate) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (update-latest-sale-for-token (at 'id token) spec sale-id 2)
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
      true
  )
  )

  (defun update-latest-sale-for-token (token-id:string spec:object{quote-spec} sale-id:string timeout:integer)

    (update-spec token-id {
      'sale-id:sale-id,
      'quote-spec:spec,
      'timeout:timeout
    })

  )

  (defun update-spec (token-id:string spec)
    (enforce-valid-spec spec)
    (update policies token-id
      {'latest-spec: spec}
    )
  )

  (defun enforce-valid-spec (spec:object{quote-expirey-schema})
    true
  )

  (defschema quote-expirey-schema
    sale-id:string
    quote-spec:object{quote-spec}
    timeout:integer)


  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-capability (BUY (at 'id token) buyer)
      (bind (get-policy token)
        { 'fungible := fungible:module{fungible-v2}
        , 'creator:= creator:string
        , 'royalty-rate:= royalty-rate:decimal
        }
        (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'price := price:decimal
            , 'recipient := recipient:string
            }
            (let* ((sale-price:decimal (* amount price))
                   (royalty-payout:decimal
                      (floor (* sale-price royalty-rate) (fungible::precision)))
                   (payout:decimal (- sale-price royalty-payout)) )
              (if
                (> royalty-payout 0.0)
                (fungible::transfer buyer creator royalty-payout)
                "No royalty")
              (fungible::transfer buyer recipient payout)))
              true
              (update-owner qtoken buyer)
          ))
    )
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited except for certain usecases that will come later..")
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



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; POLICY LEVEL LEDGER DEFUNS

  (defun update-owner (token-id:string new-owner:string)
    (require-capability (UPDATE-OWNER token-id new-owner))
    (update policies token-id
      {'owner: new-owner}
    )
  )





)



(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table policies) ])
