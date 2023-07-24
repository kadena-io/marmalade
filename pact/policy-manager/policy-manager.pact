(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard 'marmalade-admin ))

  ; (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info ])

  (defconst CONCRETE_POLICY_LIST
    [NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY] )

  (defconst NON_FUNGIBLE_POLICY 'non-fungible-policy )
  (defconst ROYALTY_POLICY 'royalty-policy )
  (defconst COLLECTION_POLICY 'collection-policy )
  (defconst GUARD_POLICY 'guard-policy )

  (defcap OFFER:bool
    ( sale-id:string
    )
    @doc "Capability to grant internal transaction inside OFFER"
    true
  )

  (defcap BUY:bool
    ( sale-id:string
    )
    @doc "Capability to grant internal transaction inside BUY"
    true
  )

  (defcap WITHDRAW:bool
    ( sale-id:string
    )
    @doc "Capability to grant internal transaction inside WITHDRAW"
    true
  )

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defconst UPDATE-QUOTE-PRICE-MSG-KEY "update_quote_price"
    @doc "Payload field for quote spec")

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
      (enforce-one quote-guards)
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

  (defschema ledger
    ledger:module{kip.poly-fungible-v3}
    ledger-guard:guard
  )

  (deftable ledgers:{ledger})

  (defschema concrete-policy
    policy:module{kip.token-policy-v2}
  )

  (deftable concrete-policies:{concrete-policy})

  (defcap ADMIN ()
    (enforce-guard 'marmalade-admin)
  )

  (defcap QUOTE_ESCROW (sale-id:string)
    true
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (get-ledger-guard (get-ledger-info)))
  )

  (defun init:bool(ledger-info:object{ledger})
    (with-capability (ADMIN)
      (insert ledgers "" ledger-info)
    )
    true
  )

  (defun get-ledger-info:object{ledger} ()
    (read ledgers "")
  )

  (defun is-used:bool (policies:[module{kip.token-policy-v2}] policy:string)
    (contains (get-concrete-policy policy) policies)
  )

  (defun enforce-init:[bool] (token:object{token-info})
    (enforce-ledger)
    (map-init token (at 'policies token))
  )

  (defun enforce-mint:[bool]
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-mint token account guard amount policies)))

  (defun enforce-burn:[bool]
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-burn token account amount policies)))

  (defun enforce-offer:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    ;; check if quote exists
    (if  (exists-msg-object QUOTE-MSG-KEY)
      ;; quote exists
      (with-capability (OFFER sale-id)
        (let* ( (quote:object{quote-msg} (read-msg QUOTE-MSG-KEY))
                (quote-spec (at 'spec quote))
                (seller-guard:guard (at 'seller-guard quote))
                (quote-guards:[guard] (at 'quote-guards quote)))
          (add-quote sale-id (at 'id token) seller-guard quote-guards quote-spec)
        )
      )
      ;; quote does not exist
      true
    )
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-offer token seller amount sale-id policies)))

  (defun enforce-withdraw:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-withdraw token seller amount sale-id policies)))

  (defun enforce-buy:[bool]
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-capability (BUY sale-id)
      (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        ;; Option - check if quote is saved at offer
        (if (exists-quote sale-id)
          ;; quote is used
          [    ;; update-quote msg exists
              (if (exists-msg-decimal UPDATE-QUOTE-PRICE-MSG-KEY)
                ;; updated quote is provided
                  (update-quote-price sale-id (read-decimal UPDATE-QUOTE-PRICE-MSG-KEY))
                ;; quote is not updated
                true
              )
              (process-quoted-buy sale-id token seller buyer buyer-guard amount policies)
          ]
          ;; quote is not used
          (map-buy token seller buyer buyer-guard amount sale-id policies)
        )
    ) )
  )

  (defun process-quoted-buy:bool
    ( sale-id:string
      token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      policies:[module{kip.token-policy-v2}]
    )
    (require-capability (BUY sale-id))
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

  (defun enforce-transfer:[bool]
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-transfer token sender guard receiver amount policies)))


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
    (require-capability (OFFER sale-id))
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
    (require-capability (BUY sale-id))
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

  (defun add-concrete-policy:bool (policy-field:string policy:module{kip.token-policy-v2})
    (contains policy-field CONCRETE_POLICY_LIST)
    (with-capability (ADMIN)
      (insert concrete-policies policy-field {
        "policy": policy
        }
      )
    true)
  )

  (defun update-concrete-policy:bool (policy-field:string policy:module{kip.token-policy-v2})
    (contains policy-field CONCRETE_POLICY_LIST)
    (with-capability (ADMIN)
      (update concrete-policies policy-field {
        "policy": policy
        }
      )
    true)
  )

  (defun get-concrete-policy:module{kip.token-policy-v2} (policy-field:string)
    (with-read concrete-policies policy-field {
      "policy":= policy
      }
      policy)
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

  ;;utility functions to map policies
 (defun token-init (token:object{token-info} policy:module{kip.token-policy-v2})
  (policy::enforce-init token))

 (defun map-init (token:object{token-info} policy-list:[module{kip.token-policy-v2}])
  (map (token-init token) policy-list))

 (defun token-mint (token:object{token-info} account:string guard:guard amount:decimal policy:module{kip.token-policy-v2})
  (policy::enforce-mint token account guard amount))

 (defun map-mint (token:object{token-info} account:string guard:guard amount:decimal policy-list:[module{kip.token-policy-v2}])
  (map (token-mint token account guard amount) policy-list))

 (defun token-burn (token:object{token-info} account:string amount:decimal policy:module{kip.token-policy-v2})
  (policy::enforce-burn token account amount))

 (defun map-burn (token:object{token-info} account:string amount:decimal policy-list:[module{kip.token-policy-v2}])
  (map (token-burn token account amount) policy-list))

 (defun token-offer (token:object{token-info} account:string amount:decimal sale-id:string policy:module{kip.token-policy-v2})
  (policy::enforce-offer token account amount sale-id))

 (defun map-offer (token:object{token-info} account:string amount:decimal sale-id:string policy-list:[module{kip.token-policy-v2}])
  (map (token-offer token account amount sale-id) policy-list))

  (defun token-withdraw (token:object{token-info} account:string amount:decimal sale-id:string policy:module{kip.token-policy-v2})
   (policy::enforce-withdraw token account amount sale-id))

  (defun map-withdraw (token:object{token-info} account:string amount:decimal sale-id:string policy-list:[module{kip.token-policy-v2}])
   (map (token-withdraw token account amount sale-id) policy-list))

 (defun token-buy (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy:module{kip.token-policy-v2})
  (policy::enforce-buy token seller buyer buyer-guard amount sale-id))

 (defun map-buy:[bool] (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy-list:[module{kip.token-policy-v2}])
  (map (token-buy token seller buyer buyer-guard amount sale-id) policy-list))

 (defun token-transfer (token:object{token-info} sender:string guard:guard receiver:string amount:decimal policy:module{kip.token-policy-v2})
  (policy::enforce-transfer  token sender guard receiver amount))

 (defun map-transfer (token:object{token-info} sender:string guard:guard receiver:string amount:decimal policy-list:[module{kip.token-policy-v2}])
  (map (token-transfer  token sender guard receiver amount) policy-list))
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table ledgers)
    (create-table concrete-policies)
    (create-table quotes)
  ])
