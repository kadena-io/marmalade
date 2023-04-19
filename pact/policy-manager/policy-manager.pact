
(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard 'marmalade-admin ))

  (implements kip.token-policy-v2)
  (use kip.concrete-policy-v1 [concrete-policy NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY])
  (use kip.token-policy-v2 [token-policies token-info])

  (defschema concrete-policy-list
    policy-field:string
    policy:module{kip.token-policy-v2}
  )

  (deftable concrete-policy-table:{concrete-policy-list})

  (defconst CONCRETE_POLICY_V1_LIST
    [NON_FUNGIBLE_POLICY QUOTE_POLICY ROYALTY_POLICY] )

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

  (defcap ROTATE_POLICY (token-id:string policy:object{token-policies})
    @event
    true
  )

  (defcap CONCRETE_POLICY_ADMIN (policy-field:string)
    ;;add admin check
    (enforce-guard 'marmalade-admin)
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

  ; (defun get-policies:object{token-policies} (token:object{token-info})
  ;   (at 'policies token)
  ; )

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
    (let ((policies:object{token-policies}  (at 'policies token)))
      (map-buy token seller buyer buyer-guard amount sale-id
        (merge-policies-list policies))))

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

   (defun token-offer (token:object{token-info} account:string guard:guard amount:decimal policy:module{kip.token-policy-v2})
     (policy::enforce-offer token account guard amount))

   (defun map-offer (token:object{token-info} account:string amount:decimal guard:guard policy-list:[module{kip.token-policy-v2}])
     (map (token-offer token account guard amount) policy-list))

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
    ; (create-table policy-table)
    (create-table ledger-guard-table)
  ])
