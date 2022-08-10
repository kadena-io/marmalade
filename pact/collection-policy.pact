(namespace (read-msg 'ns))

(interface whitelist-v1
  (defun whitelist:bool (collection-id:string account:string))
  )

(interface whitelist-policy
  (defschema whitelist
    collection-id:string
    whitelist:module{whitelist-v1}
  )
  (defun enforce-whitelist (collection-id:string account:string whitelist:module{whitelist-v1}))
)


(interface collection

  (defcap CREATE_COLLECTION (collection-id:string))

  (defcap CREATE_TOKEN (collection-id:string token-id:string))

  (defcap COLLECTION (collection-id:string total-unique-tokens:integer)
    @event)

  (defcap TOKEN (token-id:string supply:decimal)
    @event)

  (defschema owner
    account:string
    guard:guard
    owned-tokens:[string]
  )

  (defschema token
    id:string
    collection-id:string
    owners: [string]
    supply:decimal
    )

  (defschema collection
    id:string
    tokens:[string]
    total-unique-tokens:integer
    )

  (defun init-collection:bool (id:string))
  (defun get-tokens (account:string))
  (defun get-collection (collection-id:string ))
  (defun get-token-collection (token-id:string))
)

(module collection-policy GOVERNANCE

  @doc "Collection token policy."

  (implements collection)
  (implements whitelist-policy)

  (defcap GOVERNANCE ()
    (enforce-keyset 'admin))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema owner
    account:string
    guard:guard
    owned-tokens:[string]
  )

  (deftable owners:{owner})

  (defschema token
    id:string
    collection-id:string
    owners: [string]
    supply:decimal
  )

  (deftable tokens:{token})

  (defschema collection
    id:string
    tokens:[string]
    total-unique-tokens:integer
    )

  (deftable collections:{collection})

  (defschema whitelist
    collection-id:string
    whitelist:module{whitelist-v1}
    )

  (deftable whitelists:{whitelist})

  (defcap UPDATE_OWNER (id:string owner:string guard:guard )
    @event true)

  (defcap INTERNAL () true)


  (defcap CREATE_COLLECTION (collection-id:string)
    (enforce-keyset 'admin)
  )

  (defcap CREATE_TOKEN (collection-id:string token-id:string)
    (enforce-ledger)
    (enforce-keyset 'admin)
  )

  (defcap COLLECTION (collection-id:string total-unique-tokens:integer)
    @event
    true)

  (defcap TOKEN (token-id:string supply:decimal)
    @event
    true)

  (defcap MINT (id:string)
    (enforce-ledger)
    (enforce-keyset 'admin)
  )

  (defun get-policy:object{token} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun init-collection:bool
    ( id:string )
    (with-capability (CREATE_COLLECTION)
      (insert collection id {
        "id": id,
        "tokens": [],
        "total-unique-tokens": 0
        })
      (insert whitelist id {
        'collection-id: id,
        'whitelist: (read-msg 'whitelist-module)
        })
      (emit-event COLLECTION id 0)
      )
    )
  )

  (defun get-collection (collection-id:string )
    (at 'tokens (read collection collection-id))
  )

  (defun get-token-collection (token-id:string)
    (at 'collection-id (read tokens token-id))
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

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (with-capability (CREATE_TOKEN (at 'id token))
        (with-read collection (read-msg 'collection-id ) {
          'tokens:= collection-list,
          'total-unique-tokens:= total-unique-tokens
          }
        (enforce (= precision 0) "Invalid precision")
        (insert tokens id
          { "id" : id
           ,"owners": []
           ,"supply": 0.0
           ,"collection-id" : collection-id
          })
        (emit-event TOKEN id 0.0)
        (update-collection collection-id (+ id collection-list) (+ 1 total-unique-tokens))
        true))
      )

  (defun enforce-whitelist:bool (collection-id:string account:string whitelist:module{whitelist-v1})
    (whitelist::whitelist collection-id account)
  )

  (defun update-token (token-id:string owners:list supply:decimal)
    (with-capability (INTERNAL)
      (update tokens token-id {
         'owners: owners,
         'supply: supply
       })
     )
     (emit-event TOKEN token-id supply)
   )

   (defun update-owner (account:string owned-tokens:list)
     (with-capability (INTERNAL)
       (update owners account {
           "owned-tokens": [owners]
        })
      )
    )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (let* ( (collection-id:string (get-token-collection (at 'id token))
      (with-read whitelists collection-id) {
        "whitelist":=whitelist
        }
      (enforce-whitelist collection-id account whitelist))))
    (with-capability (MINT (at 'id token))
      (with-read tokens (at 'id token) {
        'owners:= owners,
        'supply:= supply
        }
        (with-read owners account {
          "owned-tokens":=owned-tokens
          }
      (update-owner account (+ (at 'id token) owned-tokens))
      (update-token (at 'id token) (+ account owners) (+ amount supply))))
      ;; filter duplicates
      ;; first time owner creation
  ))

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

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table tokens)
    (create-table collections) ])


;;whitelist interface that reserves id's for minting
;; enforce id in enforce-init for principals - manifest hash
;; create-principal
;; "t:{manifest-hash}" -front-run possible
;; ignore indexing for now.
;; distinguish collection-based token
