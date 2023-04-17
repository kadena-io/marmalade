(namespace 'kip)

(interface concrete-policy-v1

  ;;TODO - this should be moved out
  (defschema concrete-policy
    fixed-issuance-policy:bool
    quote-policy:bool
    royalty-policy:bool
    collection-policy:bool
  )
  ; (defconst FIXED_ISSUANCE_POLICY 'fixed-issuance-policy )
  ; (defconst QUOTE_POLICY 'quote-policy )
  ; (defconst ROYALTY_POLICY 'royalty-policy ) ;; depend
  ; (defconst COLLECTION_POLICY 'royalty-policy )

)
