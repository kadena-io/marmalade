(namespace (read-msg 'ns))

(interface ledger-v1

  (defcap INIT-CALL:bool (id:string precision:integer uri:string)
    @doc
      " "
  )

  (defcap TRANSFER-CALL:bool (id:string sender:string receiver:string amount:decimal)
    @doc
      " "
  )

  (defcap MINT-CALL:bool (id:string account:string amount:decimal)
    @doc
      " "
  )

  (defcap BURN-CALL:bool (id:string account:string amount:decimal)
    @doc
      " "
  )

  (defcap OFFER-CALL:bool (id:string seller:string amount:decimal sale-id:string)
    @doc
      " "
  )

  (defcap WITHDRAW-CALL:bool (id:string seller:string amount:decimal sale-id:string)
    @doc
      " "
  )

  (defcap BUY-CALL:bool (id:string seller:string buyer:string amount:decimal sale-id:string)
    @doc
      " "
  )

)
