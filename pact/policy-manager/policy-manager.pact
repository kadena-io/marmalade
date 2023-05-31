
(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard 'marmalade-admin ))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-policies token-info concrete-policy NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY COLLECTION_POLICY])

  (defschema concrete-policy-list
    policy-field:string
    policy:module{kip.token-policy-v2}
  )

  (deftable concrete-policy-table:{concrete-policy-list})

  (defconst CONCRETE_POLICY_V1_LIST
    [NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY COLLECTION_POLICY] )

  ;; schema to save policy list in table
  (defschema policies-list
    concrete-policies:[module{kip.token-policy-v2}]
    immutable-policies:[module{kip.token-policy-v2}]
    adjustable-policies:[module{kip.token-policy-v2}]
  )

  (defschema ledger-guard-schema
    guard:guard
  )
  (deftable ledger-guard-table:{ledger-guard-schema})

  (defun enforce-ledger:bool ()
    (enforce-guard (at "guard" (read ledger-guard-table "")))
  )

  (defcap CONCRETE_POLICY_ADMIN (policy-field:string)
    ;;add admin check
    (enforce-guard 'marmalade-admin)
  )

  (defcap QUOTE_ESCROW (sale-id:string)
    true
  )

  (defun init(marmalade-ledger-guard:guard)
    ;;TODO adds 4 concrete policies to concrete-policy table
    (insert ledger-guard-table "" {
      "guard": marmalade-ledger-guard
    })
    true
  )

  (defun add-concrete-policy (policy-field:string policy:module{kip.token-policy-v2} )
    (enforce (contains policy-field CONCRETE_POLICY_V1_LIST) "Not a concrete policy")
    (with-capability (CONCRETE_POLICY_ADMIN policy-field)
      (insert concrete-policy-table policy-field {
        'policy-field: policy-field
       ,'policy: policy
      })
    )
  )

  (defun get-concrete-policy:module{kip.token-policy-v2} (policy-field:string)
    (with-read concrete-policy-table policy-field
      {"policy":=policy }
      policy
    )
  )

  (defun get-policies-list:object{policies-list} (policies:object{token-policies})
    (let* ( (concrete-p:[module{kip.token-policy-v2}] (create-concrete-policy-list policies))
            (imm-p:[module{kip.token-policy-v2}] (at 'immutable-policies policies))
            (adj-p:[module{kip.token-policy-v2}] (at 'adjustable-policies policies)) )
      { 'concrete-policy: concrete-p
      , 'immutable-policy: imm-p
      , 'adjustable-policy: adj-p  } )
  )

  (defun merge-policies-list:[module{kip.token-policy-v2}] (policies:object{token-policies})
    (let* ( (concrete-p:[module{kip.token-policy-v2}] (create-concrete-policy-list policies ))
            (concrete-imm-p:[module{kip.token-policy-v2}] (+ concrete-p (at 'immutable-policies policies)))
            (concrete-imm-adj-p:[module{kip.token-policy-v2}] (+ concrete-imm-p (at 'adjustable-policies policies))) )
    concrete-imm-adj-p
    )
  )

  (defun enforce-init:bool (token:object{token-info})
    (enforce-ledger)
    (map-init token (merge-policies-list (at 'policies token)))
  )

  (defun create-concrete-policy-list:[module{kip.token-policy-v2}] (policies:object{token-policies})
    (let* ((is-used-policies (lambda (policy:string) (is-used policies policy)))
           (policy-fields:[string] (filter (is-used-policies) CONCRETE_POLICY_V1_LIST)))
      (map (get-concrete-policy) policy-fields))
  )

  (defun is-used:bool (policies:object{token-policies} policy:string)
    (at policy (at 'concrete-policies policies))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (let ((policies:object{token-policies}  (at 'policies token)))
      (map-mint token account guard amount
         (merge-policies-list policies))))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (let ((policies:object{token-policies}  (at 'policies token)))
      (map-burn token account amount
         (merge-policies-list policies))))

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (let ((policies:object{token-policies}  (at 'policies token)))
      (map-offer token seller amount sale-id
         (merge-policies-list policies))))

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (let ((policies:object{token-policies}  (at 'policies token))
          (quote-policy:module{kip.token-policy-v2, marmalade.fungible-quote-policy-interface-v1} (get-concrete-policy QUOTE_POLICY)))
      (if (is-used policies QUOTE_POLICY)
        (let* ((quote:object{marmalade.fungible-quote-policy-interface-v1.quote-schema} (quote-policy::get-quote sale-id))
               (spec:object{marmalade.fungible-quote-policy-interface-v1.quote-spec} (at 'spec quote))
               (fungible:module{fungible-v2} (at 'fungible spec))
               (price:decimal (at 'price spec))
               (sale-price:decimal (floor (* price amount) (fungible::precision)))
               (escrow-guard:guard (create-capability-guard (QUOTE_ESCROW sale-id )))
               (escrow-account:string (create-principal escrow-guard))
               )
        (with-capability (QUOTE_ESCROW sale-id)
          (fungible::transfer-create buyer escrow-account escrow-guard sale-price)
          (map-buy token seller buyer buyer-guard amount sale-id
            (filter (!= quote-policy) (merge-policies-list policies)))
            (quote-policy::enforce-buy token seller buyer buyer-guard amount sale-id)
          )) true)
    ))

    (defun get-escrow-account (sale-id:string)
      { 'account: (create-principal (create-capability-guard (QUOTE_ESCROW sale-id)))
      , 'guard: (create-capability-guard (QUOTE_ESCROW sale-id))
      })

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
    (let ((policies:object{token-policies}  (at 'policies token)))
      (map-transfer token sender guard receiver amount
        (merge-policies-list policies))))

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (let ((policies:object{token-policies}  (at 'policies token)))
      (map-crosschain token sender guard receiver target-chain amount
        (merge-policies-list policies))))

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    ;;TODO
    true
  )

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

   (defun map-buy (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy-list:[module{kip.token-policy-v2}])
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
  [ (create-table concrete-policy-table)
    (create-table ledger-guard-table)
  ])
