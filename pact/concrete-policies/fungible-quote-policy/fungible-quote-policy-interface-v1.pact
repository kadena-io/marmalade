(namespace (read-msg 'ns))

(interface fungible-quote-policy-interface-v1

  @doc "Interface for for a simple quoted sale"

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
  )

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defconst MARKETPLACE-FEE-MSG-KEY "marketplace-fee"
    @doc "Payload field for marketplace fee spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    fungible:module{fungible-v2}
    price:decimal
    recipient:string
    recipient-guard:guard
  )

  (defschema marketplace-fee-spec
    @doc "Marketplace fee data to include in payload"
    marketplace-account:string
    mk-fee-percentage:decimal
  )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (defun get-quote:object{quote-schema} (sale-id:string)
    @doc "Get Quote information"
  )

)
