(namespace (read-msg 'ns))

(module quote-manager GOVERNANCE

  (use kip.token-policy-v2)

  (use kip.token-policy-v2 [token-info])
  (use util.guards1)

  (defcap GOVERNANCE ()
    (enforce-guard 'marmalade-admin ))

  (defschema ledger
    ledger:module{kip.poly-fungible-v3}
    ledger-guard:guard
  )

  (deftable ledgers:{ledger})

  (defschema quote-msg
    @doc "Quote data to include in payload"
    spec:object{quote-spec}
    seller-guard:guard
    quote-guards:[guard]
  )

  (defschema quote-spec
    @doc "Quote spec of the sale"
    fungible:module{fungible-v2}
    seller-account:object{fungible-account} ;; receives funds in this account
    price:decimal
    amount:decimal
  )

  (defschema quote-schema
    id:string
    spec:object{quote-spec}
    seller-guard:guard
    quote-guards:[guard]
  )

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defconst UPDATE-QUOTE-PRICE-MSG-KEY "update_quote_price"
    @doc "Payload field for quote spec")

  (defschema fungible-account
    @doc "Fungible account information"
    account:string
    guard:guard
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defcap QUOTE_GUARDS:bool
    ( sale-id:string
      token-id:string
      seller-guard:guard
      quote-guards:[guard]
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (deftable quotes:{quote-schema})

  (defcap UPDATE_QUOTE:bool (sale-id:string)
    (with-read quotes sale-id {
      "quote-guards":=quote-guards
      }
      (enforce-guard-any quote-guards)
    )
    true
  )

  (defcap UPDATE_QUOTE_GUARD:bool (sale-id:string)
    (with-read quotes {
      "seller-guard":= guard
      }
      (enforce-guard guard)
    )
    true
  )

  (defcap QUOTE_ESCROW (sale-id:string)
    true
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (get-ledger-guard (get-ledger-info)))
  )

  (defun get-ledger-info:object{ledger} ()
    (read ledgers "")
  )

  (defun token-buy (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy:module{kip.token-policy-v2})
   (policy::enforce-buy token seller buyer buyer-guard amount sale-id))

  (defun map-buy:[bool] (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy-list:[module{kip.token-policy-v2}])
   (map (token-buy token seller buyer buyer-guard amount sale-id) policy-list))

  (defun process-quoted-buy:bool
    ( sale-id:string
      token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      policies:[module{kip.token-policy-v2}]
    )
    ; (require-capability (BUY sale-id))
    (let* (
           (escrow-account:object{fungible-account} (get-escrow-account sale-id))
           (quote:object{quote-schema} (get-quote-info sale-id))
           (spec:object{quote-spec} (at 'spec quote))
           (fungible:module{fungible-v2} (at 'fungible spec))
           (seller-account:object{fungible-account} (at 'seller-account spec))
           (price:decimal (at 'price spec))
           (sale-price:decimal (floor (* price amount) (fungible::precision)))
      )
       ;; transfer fungible to escrow account
       (fungible::transfer-create buyer (at 'account escrow-account) (at 'guard escrow-account) sale-price)

       (with-capability (QUOTE_ESCROW sale-id)
         ;; Run policies::enforce-buy
         (map-buy token seller buyer buyer-guard amount sale-id policies)
         ;; Transfer Escrow account to seller
         (let (
               (balance:decimal (fungible::get-balance (at 'account escrow-account)))
             )
             (install-capability (fungible::TRANSFER (at 'account escrow-account) (at 'account seller-account) balance))
             (fungible::transfer (at 'account escrow-account) (at 'account seller-account) balance)
         )
       )
       true
   )
  )


;; Sale/Escrow Functions

  (defun update-quote-guards:bool (sale-id:string guards:[guard])
    (with-capability (UPDATE_QUOTE_GUARD sale-id)
      (with-read quotes {
          "id":=token-id
         ,"seller-guard":= seller-guard
        }
        (update quotes {
          "quote-guards": guards
        })
        true
    (emit-event (QUOTE_GUARDS sale-id token-id seller-guard guards))))
  )

  (defun add-quote:bool (sale-id:string token-id:string seller-guard:guard quote-guards:[guard] quote:object{quote-spec})
    @doc "Get Quote information"
    ; (require-capability (OFFER sale-id))
    (validate-quote quote)
    (insert quotes sale-id {
       "id": token-id
     , "seller-guard":seller-guard
     , "quote-guards": quote-guards
     , "spec": quote
    })
    (emit-event (QUOTE sale-id token-id quote))
    (emit-event (QUOTE_GUARDS sale-id token-id seller-guard quote-guards))
    true
  )

  (defun update-quote-price:bool (sale-id:string price:decimal)
    ; (require-capability (marmalade.policy-manager.BUY sale-id))
    (with-capability (UPDATE_QUOTE sale-id)
      (with-read quotes sale-id {
          "spec":= quote-spec
        }
        (bind quote-spec {
            "fungible":= fungible
           ,"amount":= amount
           ,"seller-account":= fungible-account
          }
          (update quotes sale-id {
            "spec": {
                "fungible": fungible
              , "amount": amount
              , "price": price
              , "seller-account": fungible-account
              }
            }))
      ))
      true
  )

  (defun get-quote-info:object{quote-schema} (sale-id:string)
   @doc "Get Quote information"
    (read quotes sale-id)
  )

  (defun get-escrow-account:object{fungible-account} (sale-id:string)
    { 'account: (create-principal (create-capability-guard (QUOTE_ESCROW sale-id)))
    , 'guard: (create-capability-guard (QUOTE_ESCROW sale-id))
    })

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun validate-fungible-account (fungible:module{fungible-v2} seller-account:object{fungible-account})
    (let ((seller-details (fungible::details (at 'account seller-account))))
      (enforce (=
        (at 'guard seller-details) (at 'guard seller-account))
            "Seller guard does not match"))
  )

  (defun validate-quote:bool (quote-spec:object{quote-spec})
    (let* ( (fungible:module{fungible-v2} (at 'fungible quote-spec) )
            (seller-account:object{fungible-account} (at 'seller-account quote-spec))
            (amount:decimal (at 'amount quote-spec))
            (price:decimal (at 'price quote-spec))
            (sale-price:decimal (* amount price)) )
      (validate-fungible-account fungible seller-account)
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      true)
  )

  (defun get-ledger-guard:guard (ledger:object{ledger})
    (at 'ledger-guard ledger)
  )

  (defun get-ledger-contract:module{kip.poly-fungible-v3} (ledger:object{ledger})
    (at 'ledger ledger)
  )

  (defun exists-msg-decimal:bool (msg:string)
    @doc "Checks env-data field and see if the msg is a decimal"
    (= (typeof  (try false (read-decimal msg))) "decimal")
  )

  (defun exists-msg-object:bool (msg:string)
    @doc "Checks env-data field and see if the msg is a object"
    (= (take 6 (typeof  (try false  (read-msg msg)))) "object")
  )

  (defun exists-quote:bool (sale-id:string)
    @doc "Looks up quote table for quote"
    (= (take 6 (typeof (try false (get-quote-info sale-id)))) "object")
  )
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  (create-table quotes)
)
