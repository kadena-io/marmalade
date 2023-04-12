
(namespace (read-msg 'ns))

(module simple-1-off-whitelist-collection-policy GOVERNANCE

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
    slots:[string]
    fungible:module{fungible-v2}
    price:decimal
    operator-account:string
    operator-guard:guard
    shift-index:integer
  )

  (defschema token
    id:string
    collection-id:string
    supply:decimal
  )

  (defschema whitelist-info
    collection-id:string
    index:integer
    account:string
    guard:guard
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

  (defcap INIT_COLLECTION:bool (collection-id:string collection-size:integer fungible:module{fungible-v2} price:decimal operator:string)
    @event
    (fungible::details operator))

  (defcap RESERVE_WHITELIST:bool (collection-id:string account:string index:integer)
    @event
    true)

  (defcap SHIFT:bool (collection-id:string index:integer)
    @event
    true)

  (defcap REVEAL_TOKENS:bool (collection-id:string tokens:[string])
    @event
    true)

  (defcap RESERVED:bool (token-id:string whitelist-info:object{whitelist-info} )
    (let* ( (collection-id:string (at 'collection-id whitelist-info))
            (whitelist-index:integer  (at 'index whitelist-info))
            (whitelist-account:string  (at 'account whitelist-info)) )
      (with-read collections collection-id
        {
          'slots:= slots
         ,'shift-index:= shift-index
         ,'collection-size:= collection-size
         ,'tokens:= tokens
        }
        (enforce (= (at whitelist-index slots) whitelist-account) "Mismatching whitelist index" )
        (enforce (= (at (mod (+ shift-index whitelist-index) collection-size) tokens) token-id) "Account is not whitelisted for the token")))
        true
  )

  (defcap CREATE_TOKEN (token-id:string account:string)
    (let* ( (mint-guard:guard  (at 'guard (read-msg 'whitelist-info ))) )
      (enforce (validate-principal mint-guard account) "Not a valid account")
      (enforce-guard mint-guard))
  )

  (defcap MINT (token-id:string account:string)
    (let* ( (mint-guard:guard  (at 'guard (read-msg 'whitelist-info ))) )
      (enforce (validate-principal mint-guard account) "Not a valid account")
      (enforce-guard mint-guard))
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
    true
  )

  (defun init-collection:bool
    (collection-id:string
     collection-size:integer
     collection-hash:string
     operator:string
     operator-guard:guard
     fungible:module{fungible-v2}
     price:decimal )
     (with-capability (INIT_COLLECTION collection-id collection-size fungible price operator)
       (insert collections collection-id {
         "id": collection-id
        ,"collection-size": collection-size
        ,"collection-hash": collection-hash
        ,"tokens": []
        ,"slots": []
        ,"operator-account": operator
        ,"operator-guard": operator-guard
        ,"price": price
        ,"fungible": fungible
        ,"shift-index": 0
       }
       )
     )
     true
  )

  (defun operator:string (collection-id:string)
    (with-read collections collection-id {
      "operator-account":= operator
    }
    operator
  )
  )

  (defun reserve-whitelist:bool (collection-id:string account:string)
    (enforce (is-principal account) "Invalid account name")
    (with-read collections collection-id {
       "collection-size":= collection-size:integer
      ,"operator-account":=operator:string
      ,"fungible":=fungible:module{fungible-v2}
      ,"price":=price
      ,"slots":= slots
      }
      (enforce (>= collection-size (length slots)) "Pre-sale has ended")
      (fungible::transfer account operator price)
      (update collections collection-id {
        "slots": (+ slots [account])
        })
      ;;Buyers know their index from the emitted event. Index is needed in mint.
      (emit-event (RESERVE_WHITELIST collection-id account (length slots)))
      ;; If slot is full, then choose a shift index
      (if (= (- collection-size 1) (length slots))
        (update-shift-index collection-id collection-size)
        true )
    ))

  (defun update-shift-index (collection-id:string collection-size:integer)
    (with-capability (INTERNAL)
      (update collections collection-id {
        "shift-index": (random collection-size)
        })
    )
    (emit-event (SHIFT collection-id (random collection-size)))
  )

  (defun random:integer (collection-size:integer)
    (mod (at 'block-height (chain-data)) collection-size)
  )

  (defun reveal-tokens:[string] (collection-id:string token-ids:[string])
    (with-read collections collection-id {
        "slots":= slots
       ,"collection-size":= collection-size
       ,"collection-hash":=collection-hash
      }
      (enforce (= collection-hash (hash token-ids)) "Token manifests don't match")
      (with-capability (OPERATOR collection-id)
        (enforce (= collection-size (length slots)) "Pre-sale has not ended")
        (enforce (= (length token-ids) collection-size) "Token list is invalid")
        (update collections collection-id {
          'tokens: token-ids
          }))
        (emit-event (REVEAL_TOKENS collection-id token-ids))))

  (defun enforce-init:bool (token:object{token-info})
    (enforce-ledger)
    (let* ( (token-id:string  (at 'id token))
            (manifest-hash:string (at 'hash (at 'manifest token)))
            (precision:integer (at 'precision token))
            (whitelist-info:object{whitelist-info} (read-msg 'whitelist-info ))
            (collection-id:string (at 'collection-id whitelist-info)) )
    ;;Enforce whitelist guard
    (with-capability (CREATE_TOKEN token-id (at 'account whitelist-info))
      ;;enforce one-off
      (enforce (= precision 0) "Invalid precision")

      ;;Check whitelist index matches the signer
      (with-capability (RESERVED token-id whitelist-info)
        (insert tokens token-id
          { "id" : token-id
           ,"collection-id" : collection-id
           ,"supply": 0.0
          })
          true)))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)

    ;;enforce one-off
    (enforce (= amount 1.0) "Invalid mint amount")

    (let* ( (token-id:string  (at 'id token))
            (whitelist-info:object{whitelist-info} (read-msg 'whitelist-info ))
            (collection-id:string (at 'collection-id whitelist-info)) )
        (enforce (= account (at 'account whitelist-info)) "Mismatching mint account")
        (enforce (= guard (at 'guard whitelist-info)) "Mismatching mint guard")
        ;;Enforce whitelist guard
        (with-capability (MINT token-id account)
          (with-read tokens token-id {
            'supply:= supply
            }
            (enforce (= supply 0.0) "token has been minted")
            ;;Check whitelist index matches the account
            (with-capability (RESERVED token-id whitelist-info)
              (update tokens token-id
                { "supply": 1.0
                })
                true)))))

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
    (enforce false "SALE prohibited")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
      (enforce false "SALE prohibited")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
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
