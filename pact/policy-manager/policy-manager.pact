(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard 'marmalade-admin ))

  ; (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info ])

  (defconst CONCRETE_POLICY_LIST
    [NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY] )

  (defconst NON_FUNGIBLE_POLICY 'non-fungible-policy )
  (defconst QUOTE_POLICY 'quote-policy )
  (defconst ROYALTY_POLICY 'royalty-policy )
  (defconst COLLECTION_POLICY 'collection-policy )
  (defconst GUARD_POLICY 'guard-policy )

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

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token))
          (quote-policy:module{kip.token-policy-v2, marmalade.fungible-quote-policy-interface-v1} (get-concrete-policy QUOTE_POLICY)))
      (if (is-used policies QUOTE_POLICY)
        ;; quote policy is used
        (let* ((quote:object{marmalade.fungible-quote-policy-interface-v1.quote-schema} (quote-policy::get-quote sale-id))
               (spec:object{marmalade.fungible-quote-policy-interface-v1.quote-spec} (at 'spec quote))
               (fungible:module{fungible-v2} (at 'fungible spec))
               (price:decimal (at 'price spec))
               (sale-price:decimal (floor (* price amount) (fungible::precision)))
               (escrow-guard:guard (create-capability-guard (QUOTE_ESCROW sale-id )))
               (escrow-account:string (create-principal escrow-guard))
               (bid-id:string (try "" (read-msg quote-policy::BID_ID-MSG-KEY)))
              )
        (with-capability (QUOTE_ESCROW sale-id)
          (if (= bid-id "")
            (fungible::transfer-create buyer escrow-account escrow-guard sale-price)
            (quote-policy::accept-bid bid-id buyer sale-id escrow-account escrow-guard)
          )

         (map-buy token seller buyer buyer-guard amount sale-id
            (filter (!= quote-policy) policies))
            (quote-policy::enforce-buy token seller buyer buyer-guard amount sale-id)
        ))

        ;; quote policy is not used
        (map-buy token seller buyer buyer-guard amount sale-id policies)
      )
    ))

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
  (defun get-escrow-account (sale-id:string)
    { 'account: (create-principal (create-capability-guard (QUOTE_ESCROW sale-id)))
    , 'guard: (create-capability-guard (QUOTE_ESCROW sale-id))
    })

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
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
  ])
