(namespace 'kip)

(interface whitelist-v1

  (defcap WHITELIST:bool (whitelist-id:string))

  (defschema whitelist
    whitelisted: list
  )

  (defun enforce-whitelist:bool (whitelist-id:string account:string))
)

(interface collection-v1

  (defcap CREATE_COLLECTION:bool (collection-id:string))

  (defcap CREATE_TOKEN:bool (collection-id:string token-id:string))

  (defcap COLLECTION:bool (collection-id:string)
    @event)

  (defcap TOKEN:bool (token-id:string collection-id:string supply:decimal)
    @event)

  (defschema account
    account:string
    guard:guard
    tokens:list
  )

  (defschema token
    id:string
    collection-id:string
    supply:decimal
  )

  (defschema collection
    id:string
    tokens:list
  )

  (defun init-collection:bool (collection-id:string))
  (defun get-collection:object{collection} (collection-id:string))
  (defun get-token:object{token} (token-id:string))

)

(namespace (read-msg 'ns))

(module one-off-collection-policy GOVERNANCE

  @doc "Collection token policy."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)
  (implements kip.collection-v1)
  (implements kip.whitelist-v1)
  (use kip.token-policy-v1 [token-info])


  (deftable accounts:{kip.collection-v1.account})

  (deftable tokens:{kip.collection-v1.token})

  (deftable collections:{kip.collection-v1.collection})

  (deftable whitelists:{kip.whitelist-v1.whitelist})

  (defcap UPDATE_OWNER (id:string owner:string guard:guard )
    @event true)

  (defcap INTERNAL () true)


  (defcap CREATE_COLLECTION:bool (collection-id:string)
    (enforce-guard (keyset-ref-guard 'marmalade-admin ))
  )

  (defcap CREATE_TOKEN:bool (collection-id:string token-id:string)
    (enforce-ledger)
    (enforce-guard (keyset-ref-guard 'marmalade-admin ))
  )

  (defcap WHITELIST:bool (whitelist-id:string)
    (enforce-guard (keyset-ref-guard 'marmalade-admin ))
  )

  (defcap COLLECTION:bool (collection-id:string)
    @event
    true)

  (defcap TOKEN:bool (token-id:string collection-id:string supply:decimal)
    @event
    true)

  (defcap MINT (id:string)
    (enforce-ledger)
    (enforce-guard (keyset-ref-guard 'marmalade-admin ))
  )

  (defun get-policy:object{token} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun init-collection:bool ( collection-id:string )
    (with-capability (CREATE_COLLECTION collection-id)
      (insert collection collection-id {
        "id": collection-id,
        "tokens": []
        })
      (emit-event COLLECTION collection-id 0)
      )
  )

  (defun add-whitelist:bool
    ( token-id:string accounts:[string])
    (with-capability (WHITELIST)
      (insert whitelist token-id {
        "whitelisted": accounts
      })
    )
  )

  (defun get-collection:object{collection} (collection-id:string )
    (read collection collection-id)
  )

  (defun get-token:object{token} (token-id:string )
    (read tokens token-id)
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (read-msg 'collection-id )))
    (with-capability (CREATE_TOKEN collection-id token-id)
        (with-read collection collection-id {
          'tokens:= collection-list,
          'total-unique-tokens:= total-unique-tokens
          }
        (enforce (=  (at 'precision token) 0) "Invalid precision")
        (insert tokens token-id
          { "id" : token-id
           ,"supply": 0.0
           ,"collection-id" : collection-id
          })
        (emit-event TOKEN token-id 0.0)
        (update-collection collection-id (+ token-id collection-list) (+ 1 total-unique-tokens))
        true))
      ))

  (defun enforce-whitelist:bool (whitelist-id:string account:string)
    (with-capability (WHITELIST whitelist-id)
      (with-read whitelists whitelist-id {
        "whitelisted":= accounts
        }
        (enforce (contains account accounts) "not whitelisted"))
      (with-read accounts account {
          'guard:= guard
        }
        (enforce-guard guard)
      )
    )
  )

  (defun update-collection (collection-id:string token-list:list total-unique-tokens:integer)
    (with-capability (INTERNAL)
    (update collections collection-id {
      'tokens:token-list
     ,'total-unique-tokens:total-unique-tokens
      })
     )
   (emit-event COLLECTION collection-id token-list total-unique-tokens)
  )

  (defun update-token (token-id:string supply:decimal)
    (with-capability (INTERNAL)
      (update tokens token-id {
         'supply: supply
       })
     )
     (emit-event TOKEN token-id supply)
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

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (at 'collection-id (get-token token-id))))
    (enforce-whitelist token-id account)
    (with-capability (MINT (at 'id token))
      (with-read tokens (at 'id token) {
        'supply:= supply
        }
        (enforce-1-off (+ amount supply))
      (update-token (at 'id token) (+ amount supply)))
      (add-token-in-account token-id account guard)
      )
  ))

  (defun enforce-1-off (supply:decimal)
    (enforce (= 1.0 supply) "Only one token may be minted")
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
    (create-table collections) ])
