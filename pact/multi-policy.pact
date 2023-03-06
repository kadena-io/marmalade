
(namespace (read-msg 'ns))

(module multi-policy GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema policies
    policy:[module{kip.token-policy-v1}]
  )

  (deftable policy-table:{policies})

  (defun create-multi-policy (token-id:string policyL:[module{kip.token-policy-v1}])
    (insert policy-table token-id {
      "policy": policyL
      })
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
      "policy":= policyList
      }
      ;;order issue? 
      (let (
        (multi
          (lambda (token account guard amount policy:module{kip.token-policy-v1})
            (policy::enforce-mint token account guard amount))))
            (let ((multi-list
              (lambda
                (token account guard amount policyList:[module{kip.token-policy-v1}])
                  (map (multi token account guard amount) policyList))))
                  (multi-list token account guard amount policyList)))))

  (defun enforce-init:bool
    (token:object{token-info})
    (enforce-ledger)
    (create-multi-policy (at 'id token)(read-msg 'policy-list ))
      (let (
        (multi
          (lambda (token policy:module{kip.token-policy-v1})
            (policy::enforce-init token))))
            (let ((multi-list
              (lambda
                (token policyL:[module{kip.token-policy-v1}])
                  (map (multi token) policyL))))
                  (multi-list token (read-msg 'policy-list )))))


  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false ""))
    ; (enforce-ledger)
    ; (with-read policies (read-msg 'policy-id ) {
    ;   "policy":= policyList
    ;   }
    ;   (let (
    ;     (multi
    ;       (lambda (token account amount policy:module{kip.token-policy-v1})
    ;         (policy::enforce-burn token account amount))))
    ;     (let (
    ;       (multi-list
    ;         (lambda (token account amount policyL:[module{kip.token-policy-v1}])
    ;           (map (multi token account amount ) policyL))))
    ;       ))
    ;   (multi-list token account amount  policyList))

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce false "")
      ; (enforce-ledger)
      ; (with-read policies (read-msg 'policy-id ) {
      ;   "policy":= policyList
      ;   }
      ;   (let (
      ;     (multi
      ;       (lambda (token seller amount sale-id policy:module{kip.token-policy-v1})
      ;         (policy::enforce-offer token seller amount sale-id )))
      ;     (multi-list
      ;       (lambda (token seller amount sale-id policy-list:[module{kip.token-policy-v1}])
      ;         (map (multi token seller amount sale-id ) policy-list)))
      ;     )
      ;   (multi-list token seller amount sale-id  policyList))))
      )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
      (enforce false ""))
      ; (enforce-ledger)
      ; (with-read policies (read-msg 'policy-id ) {
      ;   "policy":= policyList
      ;   }
      ;   (let (
      ;     (multi
      ;       (lambda (token seller buyer buyer-guard amount sale-id policy:module{kip.token-policy-v1})
      ;         (policy::enforce-buy token seller buyer buyer-guard amount sale-id)))
      ;     (multi-list
      ;       (lambda (token seller buyer buyer-guard amount sale-id policy-list:[module{kip.token-policy-v1}])
      ;         (map (multi  token seller buyer buyer-guard amount sale-id) policy-list)))
      ;     )
      ;   (multi-list token seller buyer buyer-guard amount sale-id policyList))))

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false ""))
    ;
    ; (enforce-ledger)
    ; (with-read policies (read-msg 'policy-id ) {
    ;   "policy":= policyList
    ;   }
    ;   (let (
    ;     (multi
    ;       (lambda (token sender guard receiver amount policy:module{kip.token-policy-v1})
    ;         (policy::enforce-transfer token sender guard receiver amount)))
    ;     (multi-list
    ;       (lambda (token sender guard receiver amount policy-list:[module{kip.token-policy-v1}])
    ;         (map (multi  token sender guard receiver amount) policy-list)))
    ;     )
    ;   (multi-list token sender guard receiver amount policyList))))


  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce false ""))
    ; (with-read policies (read-msg 'policy-id ) {
    ;   "policy":= policyList
    ;   }
    ;   (map (enforce-crosschain token sender guard receiver target-chain amount) policyList)))

)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table policy-table) ])
