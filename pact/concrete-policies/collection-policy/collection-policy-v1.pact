
(namespace (read-msg 'ns))

(module collection-policy-v1 GOVERNANCE

  @doc "Collection token policy."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin)))

  (implements kip.token-policy-v2)

  (use kip.token-policy-v2 [token-info])

  (defschema collection
    id:string
    name:string
    size:integer
    max-size:integer
    operator-guard:guard
  )

  (defschema token
    id:string
    collection-id:string
  )

  (deftable collections:{collection})
  (deftable tokens:{token})

  (defcap OPERATOR (collection-id:string)
    (with-read collections collection-id {
      'operator-guard:= operator-guard:guard
      }
      (enforce-guard operator-guard))
  )

  (defcap COLLECTION:bool (collection-id:string collection-name:string collection-size:integer)
    @event
    true)

  (defcap TOKEN_COLLECTION:bool (token-id:string collection-id:string)
    @event
    true)

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
    true
  )

  (defun create-collection:bool
    ( collection-name:string
      collection-size:integer
      operator-guard:guard
      )
      (enforce (>= collection-size 0) "Collection size must be positive")
      (let ((collection-id:string (create-collection-id collection-name) ))
        (with-capability (COLLECTION collection-id collection-name collection-size)
          (insert collections collection-id {
           "id": collection-id
           ,"name": collection-name
           ,"max-size": collection-size
           ,"size": 0
           ,"operator-guard": operator-guard
          })
        )
      )
      true
  )

  (defun enforce-init:bool (token:object{token-info})
    (enforce-ledger)
    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (read-msg "collection-id")) )
    ;;Enforce operator guard
    (with-capability (OPERATOR collection-id)
      (with-read collections collection-id {
        "max-size":= max-size
       ,"size":= size
        }
      (if (= 0 max-size) "No size limit" (enforce (> max-size size) "Exceeds collection size"))

      (update collections collection-id {
        "size": (+ 1 size)
      }))
      (insert tokens token-id
        { "id" : token-id
         ,"collection-id" : collection-id
      })
    )
    (emit-event (TOKEN_COLLECTION token-id collection-id)))
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    true
  )


  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    (enforce-ledger)
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string
    )
    (enforce-ledger)
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    (enforce-ledger)
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal
    )
    (enforce-ledger)
  )

  ;;UTILITY FUNCTIONS

  (defun create-collection-id (collection-name:string)
    (format "collection:{}" [(hash collection-name)])
  )

  (defun get-collection:object{collection} (collection-id:string )
    (read collections collection-id)
  )

  (defun get-token:object{token} (token-id:string)
    (read tokens token-id)
  )

)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table tokens)
    (create-table collections) ])
