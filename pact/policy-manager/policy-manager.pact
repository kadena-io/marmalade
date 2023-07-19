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


  (defschema quote-spec
    @doc "Quote data to include in payload"
    fungible:module{fungible-v2}
    seller-account:object{fungible-account}
    quote-guards:[guard]
    price:decimal
    amount:decimal
  )

  (defschema quote-schema
    id:string
    spec:object{quote-spec}
    status:integer
  )

  (defschema fungible-account
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

  (defconst QUOTE-STATUS-OPEN 0)
  (defconst QUOTE-STATUS-WITHDRAWN 1)

  (deftable quotes:{quote-schema})

  (defcap UPDATE-QUOTE (sale-id:string)
    (with-read quotes sale-id {
      "quote-guards":=quote-guards
      }
      (enforce-one quote-guards)
    )
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
    ;; saves quote
    (with-capability (OFFER sale-id)
      (let* ( (quote:object{quote-spec} (read-msg QUOTE-MSG-KEY))
              (policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        (add-quote sale-id (at 'id token) quote)
        (map-offer token seller amount sale-id policies))))

  (defun enforce-withdraw:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-capability (WITHDRAW sale-id)
      (withdraw-quote sale-id (at 'id token) ))
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-withdraw token seller amount sale-id policies)))

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    ;; update quote
    (if (try false (read-msg QUOTE-MSG-KEY))
      (with-capability (BUY sale-id)
        (update-quote sale-id (read-msg QUOTE-MSG-KEY)))
      true
    )
    (let* (
           (policies:[module{kip.token-policy-v2}]  (at 'policies token))
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
     ;; Run policies::enforce-buy
     (map-buy token seller buyer buyer-guard amount sale-id policies)
     ;; Transfer Escrow account to seller
     (with-capability (QUOTE_ESCROW sale-id)
       (fungible::transfer-create (at 'account escrow-account) (at 'account seller-account) (at 'guard seller-account) sale-price))
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

; who gets access to update quote guards?
; (defun update-quote-guards:bool (guards:[guard])
;   (with-capability (UPDATE-QUOTE-GUARD)
;     (update quotes {
;       "quote-guards":guards
;     })
;     true)
; )

  (defun add-quote:bool (sale-id:string token-id:string quote:object{quote-spec})
    @doc "Get Quote information"
    (require-capability (OFFER sale-id))
    (enforce-sale-pact sale-id)
    (validate-quote quote)
    (insert quotes sale-id {"id": token-id, "quote": quote, "status": QUOTE-STATUS-OPEN})
    (emit-event (QUOTE sale-id token-id quote))
    true
  )

  (defun withdraw-quote:bool (sale-id:string)
    (require-capability (WITHDRAW sale-id))
    (update quotes {
      "STATUS":QUOTE-STATUS-WITHDRAWN
    })
    true
  )

  (defun update-quote:bool (sale-id:string quote:object{quote-spec})
    (require-capability (BUY sale-id))
    (with-capability (UPDATE-QUOTE)
      (update quotes {
        "quote":quote
      }))
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
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (fungible:module{fungible-v2} (at 'fungible spec) )
            (seller-account:object{fungible-account} (at 'seller-account spec))
            (amount:decimal (at 'amount spec))
            (price:decimal (at 'price spec))
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
