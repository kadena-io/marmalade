
(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [concrete-policy-v1 token-policies token-info ])

  (defschema concrete-policy-list
    policy-field:string
    policy:module{kip.token-policy-v2}
  )

  (deftable concrete-policy-table:{concrete-policy-list})

  (defconst CONCRETE_POLICY_V1_LIST
    [FIXED_ISSUANCE_POLICY QUOTE_POLICY ROYALTY_POLICY COLLECTION_POLICY] )

  (defconst FIXED_ISSUANCE_POLICY 'fixed-issuance-policy )
  (defconst QUOTE_POLICY 'quote-policy )
  (defconst ROYALTY_POLICY 'royalty-policy )
  (defconst COLLECTION_POLICY 'royalty-policy )

  ;; schema to save policy list in table
  (defschema policies-list
    concrete-policies:[module{kip.token-policy-v2}]
    immutable-policies:[module{kip.token-policy-v2}]
    adjustable-policies:[module{kip.token-policy-v2}]
  )

  ; (defschema policies
  ;   policy: object{token-policies}
  ; )
  ;
  ; (deftable policy-table:{policies})

  (defschema ledger-guard-schema
    guard:guard
  )
  (deftable ledger-guard-table:{ledger-guard-schema})

  (defun enforce-ledger:bool ()
    (enforce-guard (at "guard" (read ledger-guard-table "")))
  )

  ;; dependent on marmalade
  ; (defcap ADJUST_POLICY (token-id:string account:string)
  ;   (enforce (= (get-balance token-id account) (total-supply token-id)) "Account doesn't own token")
  ;   (enforce-guard (account-guard token-id account)))

  (defcap ROTATE_POLICY (token-id:string policy:object{token-policies})
    @event
    true
  )

  (defcap CONCRETE_POLICY_ADMIN(policy-field:string)
    ;;add admin check

    true
  )

  (defun TOKEN_INIT ()
    ;; capability used to guard `create-multi-policy` to only be called inside enforce-init
    true
  )

  (defun LEDGER ()
    ;;TODO enforce that the enforce-** functions cannot be called anywhere other than ledger
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
    (with-capability (CONCRETE_POLICY_ADMIN)
      (insert policy-field {
        'policy-field: policy-field
       ,'policy: policy
      })
    )
  )

  ; (defun get-policies:object{token-policies} (token:object{token-info})
  ;   (at 'policies token)
  ; )

  (defun get-policies-list:object{policies-list} (policies:object{token-policies})
    (let* ( (concrete-p:[module{kip.token-policy-v2}] (create-concrete-policy-list (at 'concrete-policy policies)))
            (imm-p:[module{kip.token-policy-v2}] (at 'immutable-policy policies))
            (adj-p:[module{kip.token-policy-v2}] (at 'adjustable-policy policies)) )
      { 'concrete-policy: concrete-p
      , 'immutable-policy: imm-p
      , 'adjustable-policy: adj-p  } )
  )

  (defun merge-policies-list (policies:object{token-policies})
    (let* ( (concrete-p:[module{kip.token-policy-v2}] (create-concrete-policy-list (at 'concrete-policy policies)))
            (imm-p:[module{kip.token-policy-v2}] (at 'immutable-policy policies))
            (adj-p:[module{kip.token-policy-v2}] (at 'adjustable-policy policies)) )
    (fold (+) [concrete-p imm-p adj-p] []))
  )

  (defun enforce-init:bool (token:object{token-info})
    (with-capability (LEDGER)
      (with-capability (TOKEN_INIT)
        ;; add policies to table
        ; (create-multi-policy token (at 'policies token))
        ;; runs all policies from the polciy list
        (map-init token (merge-policies-list (at 'policies token))))
    )
  )

  (defun create-concrete-policy-list:[module{kip.token-policy-v2}] (policies:object{concrete-policy-v1} policy:string)
    (filter (is-used) CONCRETE_POLICY_V1_LIST [])
  )

  (defun is-used:bool (policies:object{concrete-policy-v1} policy:string)
    (at policy policies)
  )

  ; (defun create-multi-policy ( token:object{token-info} )
  ;   (require-capability (TOKEN_INIT token))
  ;   (insert policy-table (at 'id token) {
  ;     "policies": (at 'policies token)
  ;   })
  ; )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (let ((policies:object{token-policies}  (at 'policies token)))
      ;;order issue?
      (map-offer token account guard amount
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
    true
  )

    ;;functions to add/remove adjustable policies

    ; (defun add-policy
    ;   ( token-id:string
    ;     account:string
    ;     policies:[module{kip.token-policy-v2}] )
    ;   (with-capability (ADJUST_POLICY token-id account) ;; needs sigs from token owner
    ;     (with-read policy-table token-id {
    ;       "policy":= old-policy
    ;       }
    ;       (let* ( (imm-p:[module{kip.token-policy-v2}] (at 'immutable-policy old-policy))
    ;               (adj-p:[module{kip.token-policy-v2}] (at 'adjustable-policy old-policy))
    ;               (new-policy:object{policies} {
    ;                 "immutable-policy": imm-p
    ;               , "adjustable-policy": (+ adj-p policies)}) )
    ;       (update policy-table token-id {
    ;         "policy": new-policy
    ;       })
    ;     (emit-event (ROTATE_POLICY token-id new-policy)))
    ;   ))
    ; )
    ;
    ; (defun remove-policy
    ;   ( token:string
    ;     account:string
    ;     policy-idx:integer )
    ;   (let ((token-id:string (at 'id token)))
    ;     (with-capability (ADJUST_POLICY token-id account)
    ;       (with-read policy-table token-id {
    ;         "policy":= old-policy
    ;         }
    ;         (let* ( (imm-p:[module{kip.token-policy-v2}] (at 'immutable-policy old-policy))
    ;                 (adj-p:[module{kip.token-policy-v2}] (at 'adjustable-policy old-policy))
    ;                 (new-policy:object{policies} {
    ;                     "immutable-policy": imm-p
    ;                   , "adjustable-policy":
    ;                       (+ (take policy-idx adj-p)
    ;                          (take (- policy-idx  (length  adj-p)) adj-p)) }) )
    ;         (update policy-table token-id {
    ;           "policy": new-policy
    ;         })
    ;     (emit-event (ROTATE_POLICY token-id new-policy)))))
    ; ))


    ;; dependent on marmalade
    ; (defun enforce-ledger:bool ()
    ;   (enforce-guard (marmalade.ledger.ledger-guard))
    ; )

   ;;utility functions to map policy list
   (defun token-init (token:object{token-info} policy:module{kip.token-policy-v2})
     (policy::enforce-init token))

   (defun map-init (token:object{token-info} policy-list:[module{kip.token-policy-v2}])
     (map (token-init token) policy-list))

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

; examples to be put in utils?
; (defconst DEFAULT
;   { 'concrete-policies:
;      { 'quote-policy:true
;        'royalty-policy:true
;        'collection-policy:true
;      }
;   ,'immutable-policies: []
;   ,'adjustable-policies: []
;   }
; )
;
; (defun create-single-policy (policy:module{token-policy-v1})
;   { 'concrete-policies:
;      { 'quote-policy:false
;        'royalty-policy:false
;        'collection-policy:false
;      }
;   ,'immutable-policies: [policy]
;   ,'adjustable-policies: []
; })


(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table concrete-policy-table)
    ; (create-table policy-table)
    (create-table ledger-guard-table)
  ])
