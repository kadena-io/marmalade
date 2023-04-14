
(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)

  ;; fixed-quote-policy

  (use kip.token-policy-v1 [token-info])
  (use marmalade.ledger)

  (defschema concrete-policy-list
    policy-field:string
    policy:module{kip.token-policy-v1}
  )

  (deftable concrete-policy-table:{concrete-policy-list})

  (defschema concrete-policy-v1
    quote-policy:bool
    royalty-policy:bool
    collection-policy:bool
  )

  (defconst FIXED_ISSUANCE_POLICY 'fixed-issuance-policy)
  (defconst QUOTE_POLICY 'quote-policy)
  (defconst ROYALTY_POLICY 'royalty-policy)
  (defconst COLLECTION_POLICY 'royalty-policy)

  (defconst CONCRETE_POLICY_V1_LIST ['quote-policy 'royalty-policy 'collection-policy ] )

  (defschema token-policies
    concrete-policies:object{concrete-policy-v1}
    immutable-policies:[module{kip.token-policy-v1}]
    adjustable-policies:[module{kip.token-policy-v1}]
  )

  (defschema policy-list
    concrete-policies:[module{kip.token-policy-v1}]
    immutable-policies:[module{kip.token-policy-v1}]
    adjustable-policies:[module{kip.token-policy-v1}]
  )

  (defschema policies
    policy: object{policy-list}
  )

  (deftable policy-table:{policies})

  (defcap ADJUST_POLICY (token-id:string account:string)
    (enforce (= (get-balance token-id account) (total-supply token-id)) "Account doesn't own token")
    (enforce-guard (account-guard token-id account)))

  (defcap ROTATE_POLICY (token-id:string policy:object{policies})
    @event
    true
  )

  (defcap CONCRETE_POLICY_ADMIN()
    ;;add admin check
    true
  )

  (defcap INIT_TOKEN (token:object{token-info})
    ;; check for admin authority when initiating token
    true)

  ;;adds concrete policies to concrete-policy table
  (defun init()
    true
  )

  (defun add-concrete-policy (policy-field:string policy:module{kip.token-policy-v1} )
    (with-capability (CONCRETE_POLICY_ADMIN)
      (insert policy-field {
        'policy-field: policy-field
       ,'policy: policy
      })
    )
  )

  (defun concrete-policy-used (policies:{token-policies} policy-field:string)
    (at 'concrete-policies (at policy-field ))
  )

  (defun add-policy
    ( token-id:string
      account:string
      policies:[module{kip.token-policy-v1}] )
    (with-capability (ADJUST_POLICY token-id account) ;; needs sigs from token owner
      (with-read policy-table token-id {
        "policy":= old-policy
        }
        (let* ( (imm-p:[module{kip.token-policy-v1}] (at 'immutable-policy old-policy))
                (adj-p:[module{kip.token-policy-v1}] (at 'adjustable-policy old-policy))
                (new-policy:object{policies} {
                  "immutable-policy": imm-p
                , "adjustable-policy": (+ adj-p policies)}) )
        (update policy-table token-id {
          "policy": new-policy
        })
      (emit-event (ROTATE_POLICY token-id new-policy)))
    ))
  )

  (defun remove-policy
    ( token:string
      account:string
      policy-idx:integer )
    (let ((token-id:string (at 'id token)))
      (with-capability (ADJUST_POLICY token-id account)
        (with-read policy-table token-id {
          "policy":= old-policy
          }
          (let* ( (imm-p:[module{kip.token-policy-v1}] (at 'immutable-policy old-policy))
                  (adj-p:[module{kip.token-policy-v1}] (at 'adjustable-policy old-policy))
                  (new-policy:object{policies} {
                      "immutable-policy": imm-p
                    , "adjustable-policy":
                        (+ (take policy-idx adj-p)
                           (take (- policy-idx  (length  adj-p)) adj-p)) }) )
          (update policy-table token-id {
            "policy": new-policy
          })
      (emit-event (ROTATE_POLICY token-id new-policy)))))
  ))

  (defun get-policies:object{policy-list} (token:object{token-info})
    (read policy-table (at 'id token))
  )

  ;; if policy-manager is dependent on marmalade, then mamalade can't depend on policy-manager

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    (token:object{token-info})
    (enforce-ledger)
    (with-capability (INIT_TOKEN token)
      (insert policies (at 'id token)
        (read-policy-list (at 'policies token))
      )
      (create-multi-policy token policy)
      (map-init token (+ imm-p adj-p))
    )
  )

  (defun merge-policies:[module{kip.token-policy-v1}] (policies:object{token-policies})
    (let* ( (policies:object{token-policies})
            (concrete-p-read:object{concrete-policy-v1} (at 'concrete-policy policy))
            (concrete-p:[module{kip.token-policy-v1}] (create-concrete-policy-list concrete-p-read))
            (imm-p:[module{kip.token-policy-v1}] (at 'immutable-policy policy))
            (adj-p:[module{kip.token-policy-v1}] (at 'adjustable-policy policy)) )
      (fold (+) [concrete-p imm-p adj-p] [])
  )

  (defun create-concrete-policy-list (policies:object{concrete-policy-v1})
    (map (get-concrete-policy) (filter (is-used policies) CONCRETE_POLICY_V1_LIST))
  )

  (defun get-concrete-policy:module{kip.token-policy-v1} (policy:string)
    (with-read concrete-policy-table policy {
      'policy:=concrete-policy
    })
    concrete-policy
  )

  (defun is-used:bool (policies:object{concrete-policy-v1} policy:string)
    (at policy policies)
  )

  (defun read-policy-list:object{policies} (policies:{token-policies})
    (let* ( (policy:object{policy-list} )
            (concrete-p:[module{kip.token-policy-v1}] (at 'concrete-policy policy))
            (imm-p:[module{kip.token-policy-v1}] (at 'immutable-policy policy))
            (adj-p:[module{kip.token-policy-v1}] (at 'adjustable-policy policy)) )
      { "concrete-policies": concrete-p
      , "immutable-policies": imm-p
      , "adjustable-policies": adj-p
      })
  )

  (defun create-multi-policy
    ( token:object{token-info}
      policy:object{policies} )
    (require-capability (INIT_TOKEN token))
    (enforce-ledger)
    (insert policy-table (at 'id token) {
      "policy": policy
    })
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      ;;order issue?
      (map-offer token account guard amount
         (merge-policies curr-policy))))


  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      (map-burn token account amount
         (merge-policies curr-policy))))

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (with-read policy-table (at 'id token) {
      "policy":= curr-policy
      }
      (map-offer token seller amount sale-id
         (merge-policies curr-policy))))

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)

    (if (concrete-policy-used QUOTE_POLICY)
      ;; add payment logic

    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      (map-buy token seller buyer buyer-guard amount sale-id
        (merge-policies curr-policy)))))

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      (map-transfer token sender guard receiver amount
        (merge-policies curr-policy))))

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      (map-crosschain token sender guard receiver target-chain amount
        (merge-policies curr-policy))))

   ;;utility functions to map policy list
   (defun token-init (token:object{token-info} policy:module{kip.token-policy-v1})
     (policy::enforce-init token))

   (defun map-init (token:object{token-info} policy-list:[module{kip.token-policy-v1}])
     (map (token-init token) policy-list))

   (defun token-offer (token:object{token-info} account:string guard:guard amount:decimal policy:module{kip.token-policy-v1})
     (policy::enforce-offer token account guard amount))

   (defun map-offer (token:object{token-info} account:string amount:decimal guard:guard policy-list:[module{kip.token-policy-v1}])
     (map (token-offer token account guard amount) policy-list))

   (defun token-burn (token:object{token-info} account:string amount:decimal policy:module{kip.token-policy-v1})
     (policy::enforce-burn token account amount))

   (defun map-burn (token:object{token-info} account:string amount:decimal policy-list:[module{kip.token-policy-v1}])
     (map (token-burn token account amount) policy-list))

   (defun token-buy (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy:module{kip.token-policy-v1})
     (policy::enforce-buy token seller buyer buyer-guard amount sale-id))

   (defun map-buy (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string policy-list:[module{kip.token-policy-v1}])
     (map (token-buy token seller buyer buyer-guard amount sale-id) policy-list))

   (defun token-transfer (token:object{token-info} sender:string guard:guard receiver:string amount:decimal policy:module{kip.token-policy-v1})
     (policy::enforce-transfer  token sender guard receiver amount))

   (defun map-transfer (token:object{token-info} sender:string guard:guard receiver:string amount:decimal policy-list:[module{kip.token-policy-v1}])
     (map (token-transfer  token sender guard receiver amount) policy-list))

   (defun token-crosschain (token:object{token-info} sender:string guard:guard receiver:string target-chain:string amount:decimal policy:module{kip.token-policy-v1})
     (policy::enforce-crosschain  token sender guard receiver target-chain amount))

   (defun map-crosschain (token:object{token-info} params:object sender:string guard:guard receiver:string target-chain:string amount:decimal policy-list:[module{kip.token-policy-v1}])
     (map (token-crosschain  token sender guard receiver target-chain amount) policy-list))
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table concrete-policy-table)
    (create-table policy-table)
    (init) ])
