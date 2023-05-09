(namespace (read-msg 'ns))

(module fungible-quote-policy-v1 GOVERNANCE
  @doc "Concrete policy for a simple quoted sale"

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v2 )
  (implements marmalade.fungible-quote-policy-interface-v1)
  (use marmalade.fungible-quote-policy-interface-v1 [quote-spec quote-schema marketplace-fee-spec QUOTE-MSG-KEY MARKETPLACE-FEE-MSG-KEY ])
  (use kip.token-policy-v2 [token-info])

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (deftable quotes:{quote-schema})

  (defun get-quote:object{quote-schema} (sale-id:string)
    (read quotes sale-id))

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
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
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (fungible:module{fungible-v2} (at 'fungible spec) )
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price)) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price spec)))
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
    (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
      (enforce (= qtoken (at 'id token)) "incorrect sale token")

      (bind spec
        { 'fungible := fungible:module{fungible-v2}
        , 'price := price:decimal
        , 'recipient := recipient:string
        }

      (let* (
        (mk-fee-spec:object{marketplace-fee-spec} (read-msg MARKETPLACE-FEE-MSG-KEY))
        (mk-account:string (at 'marketplace-account mk-fee-spec))
        (mk-fee-percentage:decimal (at 'mk-fee-percentage mk-fee-spec))
        (mk-fee:decimal (floor (* mk-fee-percentage price) (fungible::precision)))
        (escrow-account:string (at 'account (policy-manager.get-escrow-account sale-id)))
      )
        (if (= "" mk-account)
          [ ]
          [
            (install-capability (fungible::TRANSFER escrow-account mk-account mk-fee))
            (fungible::transfer escrow-account mk-account mk-fee)
          ]
        )

        (let (
          (balance:decimal (fungible::get-balance escrow-account))
        )
        (install-capability (fungible::TRANSFER escrow-account recipient balance))
        (fungible::transfer escrow-account recipient balance)

    ))
    true
  )))

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
    (enforce false "Transfer prohibited")
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
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

)

(if (read-msg "upgrade")
  ["upgrade complete"]
  [ (create-table quotes)])
