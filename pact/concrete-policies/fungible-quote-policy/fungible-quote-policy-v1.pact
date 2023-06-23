(namespace (read-msg 'ns))

(module fungible-quote-policy-v1 GOVERNANCE
  @doc "Concrete policy for a simple quoted sale"

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v2 )
  (implements marmalade.fungible-quote-policy-interface-v1)
  (use marmalade.fungible-quote-policy-interface-v1 [quote-spec quote-schema marketplace-fee-spec QUOTE-MSG-KEY MARKETPLACE-FEE-MSG-KEY ])
  (use kip.token-policy-v2 [token-info])

  (defschema bids-schema
    token-id:string
    buyer:string
    buyer-guard:guard
    amount:decimal
    price:decimal
    status:integer
  )

  (defconst BID-STATUS-OPEN 0)
  (defconst BID-STATUS-ACCEPTED 1)
  (defconst BID-STATUS-WITHDRAWN 2)

  (deftable quotes:{quote-schema})
  (deftable bids:{bids-schema})

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

  (defcap BID:bool
    ( bid-id:string
      sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      buyer:string
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defcap BID-WITHDRAWN:bool
    ( bid-id:string
      sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      buyer:string
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defcap BID-ACCEPTED:bool
    ( bid-id:string
      sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      buyer:string
      buyer-guard:guard
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defcap BUYER:bool (bid-id:string) 
    @doc "Only accessible for buyer"
    (with-read bids bid-id { 'buyer-guard:= buyer-guard }
      (enforce-guard buyer-guard)
    )    
  )
  
  (defcap SELLER:bool (sale-id:string) 
    @doc "Only accessible for seller"
    (with-read quotes sale-id { 'spec:= spec:object{quote-spec} }      
      (enforce-guard (at 'seller-guard spec))      
    )    
  )  

  (defcap BID_PRIVATE:bool (bid-id:string) true)

  (defun bid-escrow-account:string (bid-id:string)
    (create-principal (create-capability-guard (BID_PRIVATE bid-id)))
  )

  (defun get-bid-id:string (sale-id:string buyer:string)
    (format "{}-{}" [sale-id buyer])
  )

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
            (seller-guard:guard (at 'seller-guard spec))
            (seller-details:object (fungible::details seller))
            (sale-price:decimal (* amount price)) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard seller-details) seller-guard)
        "Seller guard does not match")
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
        }

      (let* (
        (mk-fee-spec:object{marketplace-fee-spec} (try { "marketplace-account": "", "mk-fee-percentage": 0.0 } (read-msg MARKETPLACE-FEE-MSG-KEY)))
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
        (install-capability (fungible::TRANSFER escrow-account seller balance))
        (fungible::transfer escrow-account seller balance)

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
    true
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)    
    true
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

  (defun place-bid:bool
    ( token-id:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      price:decimal
      sale-id:string )      

      (with-read quotes sale-id { 
        'id:= qtoken
        ,'spec:= spec:object{quote-spec}         
      }
        (enforce (= qtoken token-id) "incorrect sale token")        

        (bind spec
          { 'fungible := fungible:module{fungible-v2}
            ,'amount := quote-amount:decimal
          }
          (let ((bid-id:string (get-bid-id sale-id buyer)))

          (enforce (= quote-amount amount) "Bid can only be placed for full quote amount")

          ;; Transfer amount from buyer to escrow-account
          (fungible::transfer-create buyer (bid-escrow-account bid-id) (create-capability-guard (BID_PRIVATE bid-id)) (* amount price))

          ;; Store bid
          (write bids bid-id {
            "token-id": token-id
            ,"buyer": buyer
            ,"buyer-guard": buyer-guard
            ,"amount": amount
            ,"price": price
            ,"status": BID-STATUS-OPEN
          })

          (emit-event (BID bid-id sale-id token-id amount price buyer))
        )))
  )

  (defun withdraw-bid:bool (bid-id:string sale-id:string)
    (with-read bids bid-id
      { 'token-id:= token-id,
        'buyer:= buyer,
        'amount:= amount,
        'price:= price,
        'status:= status
      }
      (with-capability (BUYER bid-id)
        (enforce (= status BID-STATUS-OPEN) "Bid is not open")
        (with-read quotes sale-id { 'spec:= spec:object{quote-spec} }
          (let* (
            (fungible:module{fungible-v2} (at 'fungible spec)))

            (with-capability (BID_PRIVATE bid-id)
              (install-capability (fungible::TRANSFER (bid-escrow-account bid-id) buyer (* amount price)))
              (fungible::transfer (bid-escrow-account bid-id) buyer (* amount price))
            )
            (update bids bid-id { 'status: BID-STATUS-WITHDRAWN })
          )
        )
        (emit-event (BID-WITHDRAWN bid-id sale-id token-id amount price buyer)))
    )
  )

  (defun accept-bid:bool (
      bid-id:string 
      buyer:string
      sale-id:string
      escrow-account:string
      escrow-guard:guard
    )
    (enforce-ledger)
    (with-capability (SELLER sale-id)
    (with-read bids bid-id
      { 'token-id:= token-id,
        'buyer:= bid-buyer,
        'buyer-guard:= buyer-guard,
        'amount:= amount,
        'price:= price,
        'status:= status
      }

      (enforce (= status BID-STATUS-OPEN) "Bid is not open")
      (enforce (= bid-buyer buyer) "Bid buyer does not match buyer")
      (with-read quotes sale-id { 'spec:= spec:object{quote-spec} }
        (let* (
          (fungible:module{fungible-v2} (at 'fungible spec))
          (seller-guard:guard (at 'seller-guard spec))
          (sale-price:decimal (floor (* price amount) (fungible::precision))))

          ; Update quote to reflect accepted bid (so other policies will have access to the right quote)
          (update quotes sale-id { 
            'spec: { 
              'fungible: fungible, 
              'price: price, 
              'seller-guard: seller-guard, 
              'amount: amount 
            }})

          ; Set bid status to accepted
          (update bids bid-id { 'status: BID-STATUS-ACCEPTED })   
          
          (with-capability (BID_PRIVATE bid-id)
            (install-capability (fungible::TRANSFER (bid-escrow-account bid-id) escrow-account sale-price))
            (fungible::transfer-create (bid-escrow-account bid-id) escrow-account escrow-guard sale-price)
          )
          (emit-event (BID-ACCEPTED bid-id sale-id token-id amount price buyer buyer-guard))
        )
      )      
    ))
  )
)

(if (read-msg "upgrade")
  ["upgrade complete"]
  [
    (create-table quotes)
    (create-table bids)
  ])
