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

  (defconst BID_ID-MSG-KEY "bid-id"
    @doc "Payload field for bid-id")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    fungible:module{fungible-v2}
    price:decimal
    amount:decimal
    seller-guard:guard
  )

  (defschema marketplace-fee-spec
    @doc "Marketplace fee data to include in payload"
    marketplace-account:string
    mk-fee-percentage:decimal
  )

  (defschema quote-schema
    id:string
    spec:object{quote-spec}
  )

  (defun get-quote:object{quote-schema} (sale-id:string)
    @doc "Get Quote information"
  )

  (defun accept-bid:bool (
    bid-id:string
    buyer:string
    sale-id:string
    escrow-account:string
    escrow-guard:guard)
    @doc "Aceept a bid and transfer the bid amount from the bid-escrow account"
  )

)
