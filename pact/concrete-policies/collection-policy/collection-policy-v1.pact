
(namespace (read-msg 'ns))

(module collection-policy-v1 GOVERNANCE

  @doc "Collection token policy."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin)))

  (implements kip.token-policy-v2)

  (use kip.token-policy-v2 [token-info])

  (defschema collection
    id:string
    current-collection-size:decimal
    max-collection-size:decimal
    operator-guard:guard
  )

  (defschema token
    id:string
    collection-id:string
    max-supply:decimal
    mint-guard:guard
  )

  (deftable collections:{collection})
  (deftable tokens:{token})

  (defcap INTERNAL () true)

  (defcap OPERATOR (collection-id:string)
    (with-read collections collection-id {
      'operator-guard:= operator-guard:guard
      }
      (enforce-guard operator-guard))
  )

  (defcap INIT_COLLECTION:bool (collection-id:string collection-size:integer)
    @event
    true)

  (defcap RESERVE_TOKEN:bool (collection-id:string token-id:string account:string account-guard:guard )
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

  (defun init-collection:bool
    ( collection-id:string
      collection-size:integer
      operator-guard:guard
      )
      (with-capability (INIT_COLLECTION collection-id collection-size)
        (insert collections collection-id {
          "id": collection-id
          ,"max-collection-size": collection-size ;;(pre-defined, optional)
          ,"current-collection-size": 0.0
          ,"operator-guard": operator-guard
        }
        )
      )
      true
  )

  (defun enforce-init:bool (token:object{token-info})
    (enforce-ledger)
    (let* ( (token-id:string  (at 'id token))
            (mint-guard:guard (read-msg 'mint-guard ))
            (collection-id:string (read-msg 'collection-id ))
            (max-supply:decimal (read-msg 'max-supply )) )
    ;;Enforce operator guard
    (with-capability (OPERATOR token-id)
      (with-read collections collection-id {
        "max-collection-size":= max-collection-size
       ,"current-collection-size":= current-collection-size
        }
        (enforce (>= max-collection-size (+ max-supply current-collection-size)) "Exceeds collection size")
      (update collections collection-id {
        "current-collection-size": (+ max-supply current-collection-size)
      }))
      (insert tokens token-id
        { "id" : token-id
         ,"max-supply": max-supply
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
        'max-supply:= max-supply,
        'mint-guard:= mint-guard
        }
        (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
        (with-capability (MINT token-id account mint-guard)
            true
        ))))

  ;;GET FUNCTIONS

  (defun get-policy:object{token} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun get-collection:object{collection} (collection-id:string )
    (read collections collection-id)
  )

  (defun get-token:object{token} (token-id:string)
    (read tokens token-id)
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "BURN prohibited")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    true
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string
    )
    true
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    true
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal
    )
    true
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
  [ (create-table tokens)
    (create-table collections) ])
