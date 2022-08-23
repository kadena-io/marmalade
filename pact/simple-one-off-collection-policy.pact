
(namespace (read-msg 'ns))

(module simple-1-off-whitelist-collection-policy GOVERNANCE

  @doc "Collection token policy."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)

  (use kip.token-policy-v1 [token-info])

  (defschema collection
    id:string
    tokens:list
  )

  (defschema token
    id:string
    collection-id:string
    supply:decimal
  )

  (defschema account
    account:string
    guard:guard
    tokens:list
  )

  (defschema whitelist
    id:string
    collection-id:string
    account:string
  )

  (deftable collections:{collection})
  (deftable tokens:{token})
  (deftable accounts:{account})
  (deftable whitelists:{whitelist})

  (defcap INTERNAL () true)

  ; (defcap ADD_TO_COLLECTION:bool (collection-id:string)
  ;   (enforce-guard (keyset-ref-guard 'marmalade-admin ))
  ; )
  ;
  ; (defcap CREATE_TOKEN:bool (collection-id:string token-id:string)
  ;   (enforce-ledger)
  ;   (enforce-guard (keyset-ref-guard 'marmalade-admin ))
  ; )

  (defcap ISSUER ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin ))
    true
  )

  (defcap MINT (token-id:string)
    @event
    (enforce-ledger)
  )

  (defcap ADD_TO_COLLECTION:bool (collection-id:string token-id:string total-unique-tokens:integer)
    @event
    true)

  (defcap ADD_WHITELIST:bool (whitelist-id:string collection-id:string)
    @event
    true)

  (defun get-policy:object{token} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun init-collection:bool (collection-id:string whitelist-ids:list)
    (with-capability (ISSUER)
      (insert collections collection-id {
        "id": collection-id,
        "tokens": []
        })
      (map (add-whitelist collection-id) whitelist-ids)
    )
    true
  )

  (defun add-whitelist:bool (collection-id:string whitelist-id:string)
    (with-capability (INTERNAL)
      (insert whitelists whitelist-id {
        'id: whitelist-id
       ,'collection-id: collection-id
       ,'account:""
      })
    )
    (emit-event (ADD_WHITELIST whitelist-id collection-id))
  )

  ;; when buyer pays for the whitelist, issuer registers the whitelist
  (defun register-whitelist-buyer:bool (whitelist-id:string account:string)
    (with-capability (ISSUER)
      (update whitelists whitelist-id {
        'account: account
      }))
    true)

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (read-msg 'collection-id ))
            (manifest-hash:string (at 'hash (at 'manifest token)))
            (precision:integer (at 'precision token)))
    ;;one-off
    (enforce (=  (at 'precision token) 0) "Invalid precision")
    ;;issuer creates token
    (with-capability (ISSUER)
      (with-read collection collection-id {
        'tokens:= collection-list,
        'total-unique-tokens:= total-unique-tokens
        }
        (insert tokens token-id
          { "id" : token-id
           ,"collection-id" : collection-id
           ,"supply": 0.0
          })
          (add-to-collection collection-id (+ token-id collection-list) (+ 1 total-unique-tokens))
          true))
      ))

  (defun enforce-whitelist:bool (whitelist-id:string account:string)
    (with-read whitelists whitelist-id {
      "account":= whitelisted-account
      }
      (enforce (!= whitelisted-account "") "Whitelist account is empty")
      (enforce (= account whitelisted-account) "Account is not whitelisted"))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (let* ( (token-id:string  (at 'id token))
            (whitelist-id:string (at 'hash (at 'manifest token)))
            (collection-id:string (at 'collection-id (get-token token-id))))
    (with-read tokens token-id {
      'supply:= supply
      }
      (enforce (= supply 0.0) "token has been minted")
      (enforce-whitelist whitelist-id account)
      (with-capability (MINT token-id)
        (add-token-in-account token-id account guard)
      )
  )))

  (defun add-to-collection (collection-id:string token-list:list total-unique-tokens:integer)
    (with-capability (INTERNAL)
    (update collections collection-id {
      'tokens:token-list
     ,'total-unique-tokens:total-unique-tokens
      })
     )
   (emit-event (ADD_TO_COLLECTION collection-id token-list total-unique-tokens))
  )

 (defun add-token-in-account (token-id:string account:string guard:guard)
   (with-capability (INTERNAL)
     (with-default-read accounts account {
        'tokens: []
      } {
        'tokens:=tokens
      }
      (update accounts account {
         'tokens: (+ token-id tokens)
       })
      )
    )
  )

  (defun delete-token-in-account (token-id:string account:string guard:guard)
    (with-capability (INTERNAL)
      (with-default-read accounts account {
         'tokens: []
       } {
         'tokens:=tokens
       }
       (update accounts account {
          'tokens: (filter (!= token-id) tokens)
        })
     )
   )
 )


  (defun get-collection:object{collection} (collection-id:string )
    (read collection collection-id)
  )

  (defun get-token:object{token} (token-id:string )
    (read tokens token-id)
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn prohibited")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    ;;todo
    true
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
      ;;todo
    true
    )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table accounts)
    (create-table tokens)
    (create-table collections)
    (create-table whitelists) ])
