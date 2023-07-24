(namespace (read-msg 'ns))

(module util-v1 GOVERNANCE
  (use kip.token-policy-v2)
  (use marmalade.policy-manager )
  (use marmalade.policy-manager [CONCRETE_POLICY_LIST NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY])

  (defschema concrete-policy-bool
    non-fungible-policy:bool
    royalty-policy:bool
    collection-policy:bool
    guard-policy:bool
  )

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (defconst DEFAULT:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy:false
     ,'guard-policy: true
     })

  (defconst DEFAULT_ROYALTY:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy:false
     ,'guard-policy: true
    }
  )

  (defconst DEFAULT_COLLECTION:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy: true
     ,'guard-policy: true
    }
  )

  (defconst DEFAULT_COLLECTION_ROYALTY:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy: true
     ,'guard-policy: true
    }
  )

  (defconst EMPTY:object{concrete-policy-bool}
    { 'non-fungible-policy: false
     ,'royalty-policy: false
     ,'collection-policy:false
     ,'guard-policy: false
    }
  )

  (defun create-policies (concrete-policy:object{concrete-policy-bool})
    (let* ( (is-used-policy (lambda (policy-field:string) (at policy-field concrete-policy)))
            (used-policies:[string] (filter (is-used-policy) CONCRETE_POLICY_LIST)))
          (map (get-concrete-policy) used-policies))
  )

  (defun create-concrete-policy:object{concrete-policy-bool} (policies:[module{kip.token-policy-v2}])
    { 'non-fungible-policy: (contains (get-concrete-policy NON_FUNGIBLE_POLICY) policies)
     ,'royalty-policy: (contains (get-concrete-policy ROYALTY_POLICY) policies)
     ,'collection-policy: (contains (get-concrete-policy COLLECTION_POLICY) policies)
     ,'guard-policy: (contains (get-concrete-policy GUARD_POLICY) policies)
    }
  )
)
