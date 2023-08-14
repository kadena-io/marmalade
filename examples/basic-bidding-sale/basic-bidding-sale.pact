(namespace (read-msg 'ns))

(module basic-bidding-sale GOVERNANCE
  @doc "Example contract for a simple quoted sale with support for bidding"

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'basic-sale-admin )))

  (use marmalade-v2.quote-manager)
  (use marmalade-v2.quote-manager [quote-spec quote-msg quote-schema])

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

  (deftable bids:{bids-schema})

  (defcap BID:bool
    ( bid-id:string
      sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      buyer:string
    )
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
    (let (
      (quote:object{quote-schema} (marmalade-v2.quote-manager.get-quote-info sale-id)))

      (enforce-guard (at 'seller-guard quote))
    )
  )

  (defcap BASIC_BIDDING:bool ()
    true
  )

  (defcap BID_PRIVATE:bool (bid-id:string) true)

  (defun bid-escrow-account:string (bid-id:string)
    (create-principal (create-capability-guard (BID_PRIVATE bid-id)))
  )

  (defun get-bid-id:string (sale-id:string buyer:string)
    (format "{}-{}" [sale-id buyer])
  )

  ; ledger.sale has to have happened before this can be called
  (defun offer-for-auction:bool (sale-id:string)
    (with-capability (SELLER sale-id)
      (marmalade-v2.quote-manager.add-quote-guard sale-id (create-capability-guard (BASIC_BIDDING)))
    )
  )

  (defun place-bid:bool
    ( token-id:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      price:decimal
      sale-id:string )

      (let* (
        (quote:object{quote-schema} (marmalade-v2.quote-manager.get-quote-info sale-id))
        (qtoken:string (at 'token-id quote))
        (spec:object{quote-spec} (at 'spec quote)))

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
        (let* (
          (quote:object{quote-schema} (marmalade-v2.quote-manager.get-quote-info sale-id))
          (spec:object{quote-spec} (at 'spec quote))
          (fungible:module{fungible-v2} (at 'fungible spec)))

          (with-capability (BID_PRIVATE bid-id)
            (install-capability (fungible::TRANSFER (bid-escrow-account bid-id) buyer (* amount price)))
            (fungible::transfer (bid-escrow-account bid-id) buyer (* amount price))
          )
          (update bids bid-id { 'status: BID-STATUS-WITHDRAWN })
        )
        (emit-event (BID-WITHDRAWN bid-id sale-id token-id amount price buyer)))
    )
  )

  ; accept-bid is being called before the seller has continued the sale defpact on the ledger
  ; The bid will be marked as accepted and the funds will be transferred to the escrow account in the policy manager
  (defun accept-bid:bool (
      bid-id:string
      sale-id:string
    )
    (with-capability (SELLER sale-id)
    (with-read bids bid-id
      { 'token-id:= token-id,
        'buyer:= buyer,
        'buyer-guard:= buyer-guard,
        'amount:= amount,
        'price:= price,
        'status:= status
      }

      (enforce (= status BID-STATUS-OPEN) "Bid is not open")

      (let* (
        (escrow-account:object{fungible-account} (marmalade-v2.policy-manager.get-escrow-account sale-id))
        (quote:object{quote-schema} (marmalade-v2.quote-manager.get-quote-info sale-id))
        (spec:object{quote-spec} (at 'spec quote))
        (seller-guard:guard (at 'seller-guard quote))
        (fungible:module{fungible-v2} (at 'fungible spec))        
        (sale-price:decimal (floor (* price amount) (fungible::precision))))

        ; Set bid status to accepted
        (update bids bid-id { 'status: BID-STATUS-ACCEPTED })

        (with-capability (BASIC_BIDDING)
        (with-capability (BID_PRIVATE bid-id)
          ; Set quote in qoute-manager and transfer funds
          (marmalade-v2.policy-manager.reserve-sale sale-id price buyer buyer-guard (bid-escrow-account bid-id))
        ))
        (emit-event (BID-ACCEPTED bid-id sale-id token-id amount price buyer buyer-guard))
      )
    ))
  )
)

(if (read-msg "upgrade")
  ["upgrade complete"]
  [
    (create-table bids)
  ])
