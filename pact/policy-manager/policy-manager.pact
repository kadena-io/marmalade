(namespace (read-msg 'ns))

(module policy-manager GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-keyset "marmalade.marmalade-admin"))

  (use kip.token-policy-v2 [token-info])
  (use marmalade.quote-manager)
  (use marmalade.quote-manager [quote-spec quote-msg])

  (defcap POLICY_MANAGER:bool ()
    @doc "Ledger module guard for policies to be able to validate access to policy operations."
    true
  )

  (defun policy-manager-guard:guard ()
    (create-capability-guard (POLICY_MANAGER))
  )

  ;; Saves ledger guard information
  (defschema ledger
    ledger-guard:guard
  )

  (deftable ledgers:{ledger}
    @doc "Designed to save one ledger in a single row")

  (defun enforce-ledger:bool ()
    @doc "Enforces that function is called from the saved ledger"
    (with-read ledgers "" {
      "ledger-guard":= ledger-guard
      }
      (enforce-guard ledger-guard)
    )
  )

  (defun init:bool(ledger-guard:guard)
    @doc "Must be initiated with ledger information"
    (with-capability (GOVERNANCE)
      (insert ledgers "" {
        "ledger-guard": ledger-guard
      })
    )
    true
  )

  ;; Saves Concrete policy information
  (defschema concrete-policy
    policy:module{kip.token-policy-v2}
  )

  (defcap CONCRETE_POLICY:bool (policy-field:string policy:module{kip.token-policy-v2})
    @doc "Event emission purpose"
    @event
    true
  )

  (deftable concrete-policies:{concrete-policy})

  (defconst NON_FUNGIBLE_POLICY:string 'non-fungible-policy )
  (defconst ROYALTY_POLICY:string 'royalty-policy )
  (defconst COLLECTION_POLICY:string 'collection-policy )
  (defconst GUARD_POLICY:string 'guard-policy )
  (defconst CONCRETE_POLICY_LIST:[string]
    [NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY] )

  (defun write-concrete-policy:bool (policy-field:string policy:module{kip.token-policy-v2})
    (contains policy-field CONCRETE_POLICY_LIST)
    (with-capability (GOVERNANCE)
      (write concrete-policies policy-field {
        "policy": policy
        }
      )
      (emit-event (CONCRETE_POLICY policy-field policy))
    true)
  )

  (defun get-concrete-policy:module{kip.token-policy-v2} (policy-field:string)
    (with-read concrete-policies policy-field {
      "policy":= policy
      }
      policy)
  )

  ;; Capbilities to guard internal functions

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


  ;; Map list of policy functions

  (defun enforce-init:[bool]
    (token:object{token-info})
    (enforce-ledger)
    (with-capability (POLICY_MANAGER)
      (map-init token (at 'policies token))
    )
  )

  (defun enforce-mint:[bool]
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (with-capability (POLICY_MANAGER)
      (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        (map-mint token account guard amount policies))
    )
  )

  (defun enforce-burn:[bool]
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (with-capability (POLICY_MANAGER)
      (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        (map-burn token account amount policies))
    )
  )

  (defun enforce-offer:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-capability (POLICY_MANAGER)
      (optional-add-quote sale-id (at 'id token))
      (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        (map-offer token seller amount sale-id policies))))

  (defun enforce-withdraw:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-capability (POLICY_MANAGER)
      (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        (map-withdraw token seller amount sale-id policies))))

  (defun enforce-buy:[bool]
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-capability (POLICY_MANAGER)
      (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        ;; Option - check if quote is saved at offer
        (if (exists-quote sale-id)
          ;; quote is used
          [    ;; update-quote msg exists
            (optional-update-quote-price sale-id)
            (map-escrowed-buy sale-id token seller buyer buyer-guard amount policies)
          ]
          ;; quote is not used
          (map-buy token seller buyer buyer-guard amount sale-id policies)
        )
  )))

  (defun enforce-transfer:[bool]
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (with-capability (POLICY_MANAGER)
      (let ((policies:[module{kip.token-policy-v2}]  (at 'policies token)))
        (map-transfer token sender guard receiver amount policies))))


;; Sale/Escrow Functions
  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
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
