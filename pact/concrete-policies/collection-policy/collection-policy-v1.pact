
(namespace (read-msg 'ns))

(module collection-policy-v1 GOVERNANCE

  @doc "Collection token policy."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin)))

  (implements kip.token-policy-v2)

  (use kip.token-policy-v2 [token-info])

  (defschema collection
    id:string
    size:integer
    max-size:integer
    operator-guard:guard
  )

  (defschema token
    id:string
    collection-id:string
    mint-guard:guard
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

  (defcap MINT (token-id:string account:string account-guard:guard mint-guard:guard)
    (enforce (validate-principal account-guard account) "Not a valid account")
    (enforce-guard mint-guard)
    true
  )

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
            (mint-guard:guard (read-msg 'mint-guard ))
            (collection-id:string (read-msg 'collection-id )) )
    ;;Enforce operator guard
    (with-capability (OPERATOR token-id)
      (with-read collections collection-id {
        "max-size":= max-size
       ,"size":= size
        }
      (enforce (> max-size size) "Exceeds collection size")
      (update collections collection-id {
        "size": (+ 1 size)
      }))
      (insert tokens token-id
        { "id" : token-id
         ,"collection-id" : collection-id
         ,"mint-guard": mint-guard
      })
    ))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)

    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (read-msg "collection-id")) )

      (with-read tokens token-id {
        'mint-guard:= mint-guard
        }
        (with-capability (MINT token-id account mint-guard)
            true
        ))))

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

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )


  ;;UTILITY FUNCTIONS

  (defun create-collection-id (collection-name:string)
    (format "collection:{}" [collection-name])
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
