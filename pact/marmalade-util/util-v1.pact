(namespace (read-msg 'ns))

(module util-v1 GOVERNANCE
  (use kip.token-policy-v2 [token-policies concrete-policy QUOTE_POLICY NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY])

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (defconst DEFAULT:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy:false
     })

  (defconst DEFAULT_ROYALTY:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy:false
    }
  )

  (defconst DEFAULT_COLLECTION:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy:true
    }
  )

  (defconst DEFAULT_COLLECTION_ROYALTY:object{concrete-policy}
    { 'quote-policy: true
     ,'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy:true
    }
  )

  (defconst EMPTY:object{concrete-policy}
    { 'quote-policy: false
     ,'non-fungible-policy: false
     ,'royalty-policy: false
     ,'collection-policy:false
    }
  )

  (defun create-default-policies:object{token-policies} (concrete-policies:object{concrete-policy})
    {
      'concrete-policies: concrete-policies
     ,'immutable-policies: []
     ,'adjustable-policies: []
    }
  )

  (defun create-single-policy:object{token-policies} (policy:module{kip.token-policy-v2})
    {
      'concrete-policies: EMPTY
     ,'immutable-policies: [policy]
     ,'adjustable-policies: []
    }
  )
)
