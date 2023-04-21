
(namespace (read-msg 'ns))

(module collection-policy-v1 GOVERNANCE

  @doc "Collection token policy."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin)))

  (implements kip.token-policy-v2)

  (use kip.token-policy-v2 [token-info])

  (defschema collection
    id:string
    collection-size:integer
    collection-hash:string
    tokens:[string]
    operator-guard:guard
  )

  (defschema token
    id:string
    collection-id:string
    supply:decimal
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

  (defcap INIT_COLLECTION:bool (collection-id:string collection-size:integer collection-hash:string)
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
    (collection-id:string
      collection-size:integer
      collection-hash:string
      tokens:[string]
      operator-guard:guard
      )
      (with-capability (INIT_COLLECTION collection-id collection-size collection-hash)
        (enforce (= collection-hash (hash tokens)) "Token manifests don't match" )
        (insert collections collection-id {
          "id": collection-id
          ,"collection-size": collection-size
          ,"collection-hash": collection-hash
          ,"tokens": tokens
          ,"operator-guard": operator-guard
        }
        )
      )
      true
  )

  (defun enforce-init:bool (token:object{token-info})
    (enforce-ledger)
    (let* ( (token-id:string  (at 'id token))
            (precision:integer (at 'precision token))
            (mint-guard:guard (read-msg 'mint-guard ))
            (collection-id:string (read-msg 'collection-id )) )
    ;;Enforce operator guard
    (with-capability (OPERATOR token-id)
      ;;enforce one-off
      (enforce (= precision 0) "Invalid precision")

      ;;Validate if token belongs to collection
      (with-read collections collection-id {
        "tokens":= tokens
        }
        (enforce (contains tokens token-id) "Token does not belong to collection")
      )

      (insert tokens token-id
        { "id" : token-id
          ,"collection-id" : collection-id
          ,"supply": 0.0
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
    (enforce (= amount 1.0) "Invalid mint amount")

    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (read-msg "collection-id")) )

      (with-read tokens token-id {
        'supply:= supply,
        'mint-guard:= mint-guard
        }
        (enforce (= supply 0.0) "token has been minted")

        (with-capability (MINT token-id account mint-guard)
          (update tokens token-id
            { "supply": 1.0
            })
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
