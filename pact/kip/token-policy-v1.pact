(namespace 'kip)

(interface token-policy-v1_DRAFT1

  (defschema token-info
    id:string
    supply:decimal
    precision:integer
    manifest:object{kip.token-manifest.manifest})

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    @doc "Minting policy for TOKEN"
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    @doc "Burning policy for TOKEN"
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )

  (defun enforce-init:bool
    (token:object{token-info})
    @doc "Enforce rules on TOKEN initiation."
  )

  (defun init-sale:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale:string )
    @doc "Initialize SALE by SELLER of AMOUNT of TOKEN."
  )


  (defun enforce-sale:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      amount:decimal
      sale:string )
    @doc "Enforce rules on SALE by SELLER to BUYER AMOUNT of TOKEN."
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      receiver:string
      amount:decimal )
    @doc "Enforce rules on transfer of TOKEN AMOUNT from SENDER to RECEIVER."
  )
)
