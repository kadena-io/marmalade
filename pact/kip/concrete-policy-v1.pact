(namespace 'kip)

(interface concrete-policy-v1

  (defschema concrete-policy
    non-fungible-policy:bool
    quote-policy:bool
    royalty-policy:bool
  )

  (defconst NON_FUNGIBLE_POLICY 'non-fungible-policy )
  (defconst QUOTE_POLICY 'quote-policy )
  (defconst ROYALTY_POLICY 'royalty-policy )

)
