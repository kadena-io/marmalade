
(namespace (read-msg 'ns))

(module guard-policy-v1 GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])

  (defschema guards
    mint-guard:guard
    burn-guard:guard
    sale-guard:guard
    transfer-guard:guard
  )

  (deftable policy-guards:{guards})

  (defconst MINT_GUARD:string "mint-guard")
  (defconst BURN_GUARD:string "burn-guard")
  (defconst SALE_GUARD:string "sale-guard")
  (defconst TRANSFER_GUARD:string "transfer-guard")

  (defconst GUARD_SUCCESS:guard (create-user-guard (success)))
  (defconst GUARD_FAILURE:guard (create-user-guard (failure)))

  (defcap GUARDS:object{guards} (guards:object{guards})
    @event
    guards
  )

  (defun success:bool ()
    true
  )

  (defun failure:bool ()
    (enforce false "Disabled")
    true
  )

  (defun get-mint-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      'mint-guard:= mint-g
    }
    mint-g
    )
  )

  (defun get-burn-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      "burn-guard":= burn-guard
    }
    burn-guard
    )
  )

  (defun get-sale-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      "sale-guard":= sale-guard
    }
    sale-guard
    )
  )

  (defun get-transfer-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      "transfer-guard":= transfer-guard
    }
    transfer-guard
    )
  )

  (defun get-guards:object{guards} (token:object{token-info})
    (read policy-guards (at 'id token))
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let ((guards:object{guards}
      { 'mint-guard: (try GUARD_SUCCESS (read-msg MINT_GUARD) ) ;; type error becomes successful guard
      , 'burn-guard: (try GUARD_SUCCESS (read-msg BURN_GUARD) )
      , 'sale-guard: (try GUARD_SUCCESS (read-msg SALE_GUARD) )
      , 'transfer-guard: (try GUARD_SUCCESS (read-msg TRANSFER_GUARD) ) } ))
    (insert policy-guards (at 'id token)
      guards)
    (emit-event (GUARDS guards)) )
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (enforce-guard (at MINT_GUARD (get-guards token)))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce-guard (at BURN_GUARD (get-guards token)))
  )

  (defcap CREATE_MINT ()
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (enforce-guard (at SALE_GUARD (get-guards token)))
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (enforce-guard (at SALE_GUARD (get-guards token)))
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (enforce-guard (at SALE_GUARD (get-guards token)))
  )

  (defun enforce-sale-pact:bool (sale-id:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale-id (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce-guard (at TRANSFER_GUARD (get-guards token)))
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table policy-guards) ])
