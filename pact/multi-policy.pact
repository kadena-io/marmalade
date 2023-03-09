
(namespace (read-msg 'ns))

(module multi-policy GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema policies
    policy: {
      "immutable-policy": [module{kip.token-policy-v1}],
      "adjustable-policy": [module{kip.token-policy-v1}]
    }
  )

  (deftable policy-table:{policies})

  (defcap ADJUST_POLICY (token-id:string account:string)
    (enforce-guard (marmalade.ledger.account-guard token-id account)))

  (defun create-multi-policy
    ( token:object{token-info}
      policy:object{policies} )
    (enforce-ledger)
    (insert policy-table token-id {
      "policy": policy
      })
  )

  (defun add-policy
    ( token:object{token-info}
      policies:[module{kip.token-policy-v1}] )
    (with-capability (ADJUST_POLICY token-id) ;; needs sigs from token owner
      (with-read policy-table token-id {
        "policy": old-policy
        }
        (let* ( (imm-p:[module{kip.token-policy-v1}] (at 'immutable-policy old-policy))
                (adj-p:[module{kip.token-policy-v1}] (at 'adjustable-policy old-policy))
                (new-policy:object{policies} {
                  "immutable-policy": imm-p
                , "adjustable-policy": (+ (at 'adjustable-policy old-policy) policies)}) )
        (update policy-table token-id {
          "policy": new-policy
        })
    )))
    (emit-event (ROTATE_POLICY token-id new-policy))
  )

  (defun remove-policy
    ( token:object{token-info}
      policy-idx:integer )
    (with-capability (ADJUST_POLICY token-id) ;; needs sigs from token owner
      (with-read policy-table token-id {
        "policy": old-policy
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
  )

  (defun get-policies:[module{kip.token-policy-v1}] (token:object{token-info})
    (read policy-table (at 'id token))
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    (token:object{token-info})
    (enforce-ledger)
    (create-multi-policy token (read-msg 'policy-list ))
    (map-init token (+ (at 'immutable-policy (read-msg 'policy-list ))
                       (at 'adjustable-policy (read-msg 'policy-list )))))

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
         (+ (at 'immutable-policy curr-policy)
            (at 'adjustable-policy curr-policy)))))


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
         (+ (at 'immutable-policy curr-policy)
            (at 'adjustable-policy curr-policy)))))

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
         (+ (at 'immutable-policy curr-policy)
            (at 'adjustable-policy curr-policy)))))

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      (map-buy token seller buyer buyer-guard amount sale-id
        (+
          (+ (at 'immutable-policy curr-policy)
              (at 'adjustable-policy curr-policy))
          (read-msg 'marketplace-policy )))))


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
        (+ (at 'immutable-policy curr-policy)
           (at 'adjustable-policy curr-policy)))))

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
        (+ (at 'immutable-policy curr-policy)
           (at 'adjustable-policy curr-policy)))))

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

   (defun map-crosschain (token:object{token-info} sender:string guard:guard receiver:string target-chain:string amount:decimal policy-list:[module{kip.token-policy-v1}])
     (map (token-crosschain  token sender guard receiver target-chain amount) policy-list))


(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table policy-table) ])
