
(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard 'marmalade-admin ))

  ; (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [ token-info concrete-policy NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY])

  (defconst CONCRETE_POLICY_V1_LIST
    [NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY] )

  ;; schema to save policy list in table
  (defschema concrete-policy-manager
    non-fungible-policy:module{kip.token-policy-v2}
    quote-policy:module{kip.token-policy-v2}
    royalty-policy:module{kip.token-policy-v2}
    collection-policy:module{kip.token-policy-v2}
    guard-policy:module{kip.token-policy-v2}
  )

  ;; used in the filter
  (defconst CONCRETE_POLICY_LIST
    [NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY] )


  (defschema ledger
    ledger:module{kip.poly-fungible-v3} ;; marmalade.ledger
    guard:guard  ;;marmalade-ledger guard
    ;; ledger vs guard
    concrete-policies:object{concrete-policy-manager} ;; module ref to concrete-policies
  )

  (deftable ledgers:{ledger})

  (defun enforce-ledger:bool ()
    ;;dependency issue
    (enforce-guard (get-ledger-guard (get-ledger-info)))
  )

  (defcap ADMIN ()
    ;;add admin check
    (enforce-guard 'marmalade-admin)
  )

  (defcap QUOTE_ESCROW (sale-id:string)
    true
  )

  (defun init:bool(ledger-info:object{ledger})
    (with-capability (ADMIN)
      (insert ledgers "" ledger-info)
    )
    true
  )

  (defun get-concrete-policy:module{kip.token-policy-v2} (policy-field:string)
    (at policy-field (get-concrete-policies (get-ledger-info)))
  )

  (defun get-concrete-policies:object{concrete-policy-manager} (ledger:object{ledger})
    (at 'concrete-policies ledger)
  )

  (defun get-ledger-guard:guard (ledger:object{ledger})
    (at 'guard ledger)
  )

  (defun get-ledger-contract:module{kip.poly-fungible-v3} (ledger:object{ledger})
    (at 'ledger ledger)
  )

  (defun get-ledger-info:object{ledger} ()
    (read ledgers "") ;; default to ""
  )

  (defun enforce-init:[bool] (token:object{token-info})
    (enforce-ledger)
    (map-init token (at 'policies token))
  )

  (defun is-used:bool (policies:[module{kip.token-policy-v2}] policy:string)
    (contains (get-concrete-policy policy) policies)
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
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-offer token seller amount sale-id policies)))
;;[bool]
  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token))
          (quote-policy:module{kip.token-policy-v2, marmalade.fungible-quote-policy-interface-v1} (get-concrete-policy QUOTE_POLICY)))
      (if (is-used policies QUOTE_POLICY)
        ;; enforce-buy when quote policy is used
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

        ;; enforce-buy without the use of quote policy
        (map-buy token seller buyer buyer-guard amount sale-id policies)
      )
    ))

    (defun get-escrow-account (sale-id:string)
      { 'account: (create-principal (create-capability-guard (QUOTE_ESCROW sale-id)))
      , 'guard: (create-capability-guard (QUOTE_ESCROW sale-id))
      })

    (defun enforce-sale-pact:bool (sale:string)
      "Enforces that SALE is id for currently executing pact"
      (enforce (= sale (pact-id)) "Invalid pact/sale id")
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

  (defun enforce-crosschain:[bool]
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
      (map-crosschain token sender guard receiver target-chain amount policies)))

  (defun enforce-withdraw:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    ;;TODO
    true
  )

  (defun create-concrete-policy:object{concrete-policy} (policies:[module{kip.token-policy-v2}])
    (let ((registered-policies:object{concrete-policy-manager} (get-concrete-policies (get-ledger-info))))
      { 'quote-policy: (contains (at QUOTE_POLICY registered-policies) policies )
       ,'non-fungible-policy: (contains (at NON_FUNGIBLE_POLICY registered-policies) policies)
       ,'royalty-policy: (contains (at ROYALTY_POLICY registered-policies) policies)
       ,'collection-policy: (contains (at COLLECTION_POLICY registered-policies) policies)
       ,'guard-policy: (contains (at GUARD_POLICY registered-policies) policies)
      })
  )

  (defun create-policies (concrete-policy:object{concrete-policy})
    (let* ( (is-used-policy (lambda (policy-field:string) (at policy-field concrete-policy)))
            (used-policies:[string] (filter (is-used-policy) CONCRETE_POLICY_LIST)))
          (map (get-concrete-policy) used-policies))
  )

  ; (defun mint-guard ()
  ;   (if (is-used guard-policy)
  ;     (guard-policy.get-mint-guard)
  ;   )
  ; )

   ;;utility functions to map policy list
   (defun token-init (token:object{token-info} policy:module{kip.token-policy-v2})
     (policy::enforce-init token))

   (defun map-init (token:object{token-info} policy-list:[module{kip.token-policy-v2}])
     (map (token-init token) policy-list))

   (defun token-mint (token:object{token-info} account:string guard:guard amount:decimal policy:module{kip.token-policy-v2})
     (policy::enforce-mint token account guard amount))

   (defun map-mint (token:object{token-info} account:string guard:guard amount:decimal policy-list:[module{kip.token-policy-v2}])
     (map (token-mint token account guard amount) policy-list))

   (defun token-offer (token:object{token-info} account:string amount:decimal sale-id:string policy:module{kip.token-policy-v2})
     (policy::enforce-offer token account amount sale-id))

   (defun map-offer (token:object{token-info} account:string amount:decimal sale-id:string policy-list:[module{kip.token-policy-v2}])
     (map (token-offer token account amount sale-id) policy-list))

   (defun token-burn (token:object{token-info} account:string amount:decimal policy:module{kip.token-policy-v2})
     (policy::enforce-burn token account amount))

   (defun map-burn (token:object{token-info} account:string amount:decimal policy-list:[module{kip.token-policy-v2}])
     (map (token-burn token account amount) policy-list))

   (defun token-buy (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy:module{kip.token-policy-v2})
     (policy::enforce-buy token seller buyer buyer-guard amount sale-id))

   (defun map-buy:[bool] (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy-list:[module{kip.token-policy-v2}])
     (map (token-buy token seller buyer buyer-guard amount sale-id) policy-list))

   (defun token-transfer (token:object{token-info} sender:string guard:guard receiver:string amount:decimal policy:module{kip.token-policy-v2})
     (policy::enforce-transfer  token sender guard receiver amount))

   (defun map-transfer (token:object{token-info} sender:string guard:guard receiver:string amount:decimal policy-list:[module{kip.token-policy-v2}])
     (map (token-transfer  token sender guard receiver amount) policy-list))

   (defun token-crosschain (token:object{token-info} sender:string guard:guard receiver:string target-chain:string amount:decimal policy:module{kip.token-policy-v2})
     (policy::enforce-crosschain  token sender guard receiver target-chain amount))

   (defun map-crosschain (token:object{token-info} params:object sender:string guard:guard receiver:string target-chain:string amount:decimal policy-list:[module{kip.token-policy-v2}])
     (map (token-crosschain  token sender guard receiver target-chain amount) policy-list))
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table ledgers)
  ])
