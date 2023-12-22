
(namespace (read-string 'ns))

(module guard-policy-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use policy-manager)

  (defschema guards
    mint-guard:guard
    burn-guard:guard
    sale-guard:guard
    transfer-guard:guard
  )

  (deftable policy-guards:{guards})

  (defconst MINT-GUARD-MSG-KEY:string "mint_guard")
  (defconst BURN-GUARD-MSG-KEY:string "burn_guard")
  (defconst SALE-GUARD-MSG-KEY:string "sale_guard")
  (defconst TRANSFER-GUARD-MSG-KEY:string "transfer_guard")

  (defconst GUARD_SUCCESS:guard (create-user-guard (success)))
  (defconst GUARD_FAILURE:guard (create-user-guard (failure)))

  (defcap GUARDS:bool (token-id:string guards:object{guards})
    @doc "Emits event for discovery"
    @event
    true
  )

  (defcap MINT (token-id:string account:string amount:decimal)
    (enforce-guard (get-mint-guard token-id))
  )

  (defcap BURN (token-id:string account:string amount:decimal)
    (enforce-guard (get-burn-guard token-id))
  )

  (defcap SALE (token-id:string seller:string amount:decimal)
    (enforce-guard (get-sale-guard token-id))
  )

  (defcap TRANSFER (token-id:string sender:string receiver:string amount:decimal)
    (enforce-guard (get-transfer-guard token-id))
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
      'mint-guard:= mint-guard
    }
    mint-guard
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

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "Executed at `create-token` step of marmalade.ledger. Registers  guards for \
    \ 'mint', 'burn', 'sale', 'transfer' operations of the created token.            \
    \ Required msg-data keys:                                                        \
    \ * (optional) mint_guard:string -  mint-guard and adds success guard if absent. \
    \ * (optional) burn_guard:string -  burn-guard and adds success guard if absent. \
    \ * (optional) sale_guard:string -  sale-guard and adds success guard if absent. \
    \ * (optional) transfer_guard:string -  transfer-guard and adds success guard if absent. \
    \ the created token"
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) guard-policy-v1))
    (let ((guards:object{guards}
      { 'mint-guard: (try GUARD_SUCCESS (read-msg MINT-GUARD-MSG-KEY) ) ;; type error becomes successful guard
      , 'burn-guard: (try GUARD_SUCCESS (read-msg BURN-GUARD-MSG-KEY) )
      , 'sale-guard: (try GUARD_SUCCESS (read-msg SALE-GUARD-MSG-KEY) )
      , 'transfer-guard: (try GUARD_SUCCESS (read-msg TRANSFER-GUARD-MSG-KEY) ) } ))
    (insert policy-guards (at 'id token)
      guards)
    (emit-event (GUARDS (at "id" token) guards)) )
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount guard-policy-v1))
    (with-capability (MINT (at 'id token) account amount)
      true
    )
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (require-capability (BURN-CALL (at "id" token) account amount guard-policy-v1))
    (with-capability (BURN (at 'id token) account amount)
      true
    )
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (require-capability (OFFER-CALL (at "id" token) seller amount sale-id timeout guard-policy-v1))
    (enforce-sale-pact sale-id)
    (with-capability (SALE (at 'id token) seller amount)
      true
    )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (require-capability (BUY-CALL (at "id" token) seller buyer amount sale-id guard-policy-v1))
    (enforce-sale-pact sale-id)
    (with-capability (SALE (at 'id token) seller amount)
      true
    )
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (require-capability (WITHDRAW-CALL (at "id" token) seller amount sale-id timeout guard-policy-v1))
    (enforce-sale-pact sale-id)
    (with-capability (SALE (at 'id token) seller amount)
      true
    )
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (require-capability (TRANSFER-CALL (at "id" token) sender receiver amount guard-policy-v1))
    (with-capability (TRANSFER (at 'id token) sender receiver amount)
      true
    )
  )

  (defun enforce-sale-pact:bool (sale-id:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale-id (pact-id)) "Invalid pact/sale id")
  )
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table policy-guards) ]
  )

(enforce-guard ADMIN-KS)
