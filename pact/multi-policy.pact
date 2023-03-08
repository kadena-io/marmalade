
(namespace (read-msg 'ns))

(module multi-policy GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema policies
    policy: {
      "immutable-policy": [module{kip.token-policy-v1}],
      "utility-policy": [module{kip.token-policy-v1}]
    }
  )

  (deftable policy-table:{policies})

  (defcap ADJUST_POLICY (token-id:string account:string)
    (enforce-guard (marmalade.ledger.account-guard token-id account)
  )

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
        (update policy-table token-id {
          "policy": {
            "immutable-policy": (at 'immutable-policy old-policy)
           ,"utility-policy": (+ (at 'utility-policy old-policy) policies)
          }
        })
    ))
    (emit-event (ROTATE_POLICY token-id {
      "immutable-policy": (at 'immutable-policy old-policy)
     ,"utility-policy": (+ (at 'utility-policy old-policy) policies)
    }))
  )

  (defun remove-policy
    ( token:object{token-info}
      policy-idx:integer )
    (with-capability (ADJUST_POLICY token-id) ;; needs sigs from token owner
      (with-read policy-table token-id {
        "policy": old-policy
        }
        (update policy-table token-id {
          "policy": {
            "immutable-policy": (at 'immutable-policy old-policy)
           ,"utility-policy":
             (+ (take policy-idx (at 'utility-policy old-policy))
                (take (- policy-idx  (length  (at 'utility-policy old-policy)))))
          }
        })
    ))
    (emit-event (ROTATE_POLICY token-id {
      "immutable-policy": (at 'immutable-policy old-policy)
     ,"utility-policy": (+ (at 'utility-policy old-policy) policies)
    }))
  )

  (defun get-policies:[module{kip.token-policy-v1}] (token:object{token-info})
    (read policy-table (at 'id token))
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
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
      (let (
        (multi
          (lambda (token account guard amount policy:module{kip.token-policy-v1})
            (policy::enforce-mint token account guard amount))))
            (let ((multi-list
              (lambda
                (token account guard amount policy-list:[module{kip.token-policy-v1}])
                  (map (multi token account guard amount) policy-list))))
                  (multi-list token account guard amount
                    (+ (at 'immutable-policy curr-policy) (at 'utility-policy curr-policy)))))))

  (defun enforce-init:bool
    (token:object{token-info})
    (enforce-ledger)
    (create-multi-policy token (read-msg 'policy-list ))
      (let (
        (multi
          (lambda (token policy:module{kip.token-policy-v1})
            (policy::enforce-init token))))
            (let ((multi-list
              (lambda
                (token policy-list:[module{kip.token-policy-v1}])
                  (map (multi token) policyL))))
                  (multi-list token (+
                    (at 'immutable-policy (read-msg 'policy-list ))
                    (at 'utility-policy (read-msg 'policy-list )))))))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      (let (
        (multi
          (lambda (token account amount policy:module{kip.token-policy-v1})
            (policy::enforce-burn token account amount))))
            (let ((multi-list
              (lambda
                (token account amount policy-list:[module{kip.token-policy-v1}])
                  (map (multi token account amount) policy-list))))
                  (multi-list token account amount
                    (+ (at 'immutable-policy curr-policy) (at 'utility-policy curr-policy)))))))


  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (with-read policy-table (at 'id token ) {
      "policy":= curr-policy
      }
      (let (
        (multi
          (lambda (token seller amount sale-id policy:module{kip.token-policy-v1})
            (policy::enforce-offer token seller amount sale-id))))
            (let ((multi-list
              (lambda
                (token seller amount sale-id policy-list:[module{kip.token-policy-v1}])
                  (map (multi token seller amount sale-id) policy-list))))
                  (multi-list token seller amount sale-id
                     (+ (at 'immutable-policy curr-policy) (at 'utility-policy curr-policy)))))))

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
      (let (
        (multi
          (lambda (token seller buyer buyer-guard amount sale-id policy:module{kip.token-policy-v1})
            (policy::enforce-buy token seller buyer buyer-guard amount sale-id))))
            (let ((multi-list
              (lambda
                (token seller buyer buyer-guard amount sale-id policy-list:[module{kip.token-policy-v1}])
                  (map (multi token seller buyer buyer-guard amount sale-id) policy-list))))
                  (multi-list token seller buyer buyer-guard amount sale-id
                    (+
                      (+ (at 'immutable-policy curr-policy) (at 'utility-policy curr-policy)))
                      (read-msg 'marketplace-policy )
                      )
                  ))))

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
      (let (
        (multi
          (lambda (token sender guard receiver amount policy:module{kip.token-policy-v1})
            (policy::enforce-transfer token sender guard receiver amount))))
            (let ((multi-list
              (lambda
                (token sender guard receiver amount policy-list:[module{kip.token-policy-v1}])
                  (map (multi token sender guard receiver amount) policy-list))))
                  (multi-list token sender guard receiver amount
                    (+ (at 'immutable-policy curr-policy) (at 'utility-policy curr-policy)))
                  ))))

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce false ""))
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table policy-table) ])
