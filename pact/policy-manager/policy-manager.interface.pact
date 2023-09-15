(namespace (read-msg 'ns))

(interface policy-manager-v1

  (use kip.token-policy-v2)

  (defcap ADD-QUOTE-CALL:bool (sale-id:string token-id:string price:decimal)
    @doc
    "Capability securing the modref call for add-quote"
  )

  (defcap CLOSE-QUOTE-CALL:bool (sale-id:string)
    @doc
    "Capability securing the modref call for close-quote"
  )

  (defcap UPDATE-QUOTE-PRICE-CALL:bool (sale-id:string price:decimal buyer:string)
    @doc
    "Capability securing the modref call for update-quote-price"
  )
)
