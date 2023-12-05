(namespace (read-msg 'ns))

(module dutch-auction GOVERNANCE
  @doc "Dutch auction contract"

  (defconst ADMIN-KS:string "marmalade-sale.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (use marmalade-v2.policy-manager)
  (use marmalade-v2.policy-manager [BUYER-FUNGIBLE-ACCOUNT-MSG-KEY])
  (implements marmalade-v2.sale-v2)

  (defschema auctions-schema
    token-id:string
    start-date:integer
    end-date:integer
    start-price:decimal
    reserve-price:decimal
    sell-price:decimal
    buyer:string
    buyer-guard:guard
  )

  (deftable auctions:{auctions-schema})

  (defcap AUCTION_CREATED:bool
    ( sale-id:string
      token-id:string
    )
    @event
    true
  )

  (defcap MANAGE_AUCTION:bool (sale-id:string token-id:string)
    (let* (
      (quote-info:object{quote-schema} (get-quote-info sale-id))
      (seller:string (at 'seller quote-info)))
      (enforce-guard (marmalade-v2.ledger.account-guard token-id seller))
    )
  )

  (defcap PRICE_ACCEPTED:bool
    ( sale-id:string
      bidder:string
      bidder-guard:guard
      price:decimal
      token-id:string
    )
    @event
    true
  )

  (defcap DUMMY:bool () true)
  (defconst DUMMY_GUARD:guard (create-capability-guard (DUMMY)))

  (defun enforce-fungible-transfer:bool (sale-id:string)
    (require-capability (FUNGIBLE-TRANSFER-CALL sale-id) )
  )

  (defun enforce-quote-update:bool (sale-id:string price:decimal)
    (require-capability (SALE-GUARD-CALL sale-id price))
    (with-read auctions sale-id
      { 'token-id:= token-id,
        'start-date:= start-date,
        'end-date:= end-date,
        'sell-price:= sell-price
      }

      (enforce (> (curr-time) start-date) "Auction has not started yet")
      (enforce (< (curr-time) end-date) "Auction has already ended")
      (enforce (= sell-price 0.0) "Price has already been accepted")
      (let (
        (current-price:decimal (get-current-price sale-id))
        (buyer:string (read-msg "buyer"))
        (buyer-guard:guard (read-msg "buyer-guard")))

        (enforce (= price current-price) "Price does not match current price")

        ; Update auction with sell-price and buyer information
        (update auctions sale-id {
          "sell-price": current-price
          ,"buyer": buyer
          ,"buyer-guard": buyer-guard
        })

        (emit-event (PRICE_ACCEPTED sale-id buyer buyer-guard current-price token-id))
      )
    )
    true
  )

  (defun enforce-withdrawal:bool (sale-id:string)
    (with-read auctions sale-id
      { 'end-date:= end-date,
        'sell-price:= sell-price
      }
      (enforce (> (curr-time) end-date) "Auction is still ongoing or hasn't started yet")
      (enforce (= sell-price 0.0) "Price has been accepted, can't withdraw")
    )
    true
  )

  (defun curr-time:integer ()
    (floor (diff-time (at 'block-time (chain-data)) (time "1970-01-01T00:00:00Z"))))

  (defun validate-auction:bool (
    start-date:integer
    end-date:integer
    reserve-price:decimal
    start-price:decimal)
    (enforce (> start-date (curr-time)) "Start date must be in the future")
    (enforce (> end-date start-date) "End date must be after start date")
    (enforce (> reserve-price 0.0) "Reserve price must be greater than 0")
    (enforce (> start-price reserve-price) "Start price must be greater than reserve price")
  )

  (defun create-auction
    ( sale-id:string
      token-id:string
      start-date:integer
      end-date:integer
      reserve-price:decimal
      start-price:decimal    
    )
    (validate-auction start-date end-date reserve-price start-price)
    (let (
      (quote-info:object{quote-schema} (get-quote-info sale-id)))

      (enforce (= (at 'sale-price quote-info) 0.0) "Quote price must be 0")
      (enforce (= (at 'sale-type quote-info) "marmalade-sale.dutch-auction") "Quote does not support auction")
      (enforce (= token-id (at 'token-id quote-info)) "Token-id does not match quote token-id")
    )

    (with-capability (MANAGE_AUCTION sale-id token-id)
      (insert auctions sale-id {
        "token-id": token-id
        ,"start-date": start-date
        ,"end-date": end-date
        ,"start-price": start-price
        ,"reserve-price": reserve-price
        ,"sell-price": 0.0
        ,"buyer": ""
        ,"buyer-guard": DUMMY_GUARD
      })
      (emit-event (AUCTION_CREATED sale-id token-id))
    )
  )

  (defun update-auction
    ( sale-id:string
      start-date:integer
      end-date:integer
      start-price:decimal
      reserve-price:decimal
    )
    (validate-auction start-date end-date reserve-price start-price)

    (with-read auctions sale-id
      { 'token-id:= token-id,
        'start-date:= curr-start-date
      }
      (enforce (> curr-start-date (curr-time)) "Can't update auction after it has started")

      (with-capability (MANAGE_AUCTION sale-id token-id)
        (update auctions sale-id {
          "start-date": start-date
          ,"end-date": end-date
          ,"start-price": start-price
          ,"reserve-price": reserve-price
        })
      )
    )
  )

  (defun retrieve-auction (sale-id:string)
    (read auctions sale-id)
  )

  (defun get-current-price:decimal (sale-id:string)
    @doc "Returns the current price of the auction"
    (with-read auctions sale-id
      {
        'start-date:= start-date,
        'end-date:= end-date,
        'reserve-price:= reserve-price,
        'start-price:= start-price
      }

      (let* (
        (sale-period-hours:decimal  (* (round (/ (- end-date start-date) 3600.0)) 1.0))
        (period-passed-hours:decimal (* (round (/ (- (curr-time) start-date) 3600.0)) 1.0))
        (price-range:decimal (- start-price reserve-price))
        )
        (if (or (< (curr-time) start-date) (> (curr-time) end-date))
          0.0
          (round (- start-price (* (/ period-passed-hours sale-period-hours) price-range)) 2)
        )
      )
    )
  )
)

(if (read-msg "upgrade")
  ["upgrade complete"]
  [
    (create-table auctions)
  ]
)
