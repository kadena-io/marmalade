(namespace (read-msg 'ns))

(module util-v1 GOVERNANCE
  (use kip.token-policy-v2 [ concrete-policy QUOTE_POLICY NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY])
  (use marmalade.policy-manager )

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))


  (defconst DEFAULT:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy:false
     ,'guard-policy: true
     })

  (defconst DEFAULT_ROYALTY:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy:false
     ,'guard-policy: true
    }
  )

  (defconst DEFAULT_COLLECTION:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy: true
     ,'guard-policy: true
    }
  )

  (defconst DEFAULT_COLLECTION_ROYALTY:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy: true
     ,'guard-policy: true
    }
  )

  (defconst EMPTY:object{concrete-policy}
    { 'quote-policy: false
     ,'non-fungible-policy: false
     ,'royalty-policy: false
     ,'collection-policy:false
     ,'guard-policy: false
    }
  )

  (defun create-policies (concrete-policy:object{concrete-policy})
    (let* ( (is-used-policy (lambda (policy-field:string) (at policy-field concrete-policy)))
            (used-policies:[string] (filter (is-used-policy) CONCRETE_POLICY_LIST)))
          (map (get-concrete-policy) used-policies))
  )

)
