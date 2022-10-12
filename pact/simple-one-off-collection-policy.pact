
(namespace (read-msg 'ns))

(module simple-1-off-whitelist-collection-policy GOVERNANCE

  @doc "Collection token policy."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)

  (use kip.token-policy-v1 [token-info])

  (defschema collection
    id:string
    total-unique-tokens:integer
    collection-size:integer
    tokens:[string]
    slots:[string]
    reservation-price:decimal
    reservation-fungible:module{fungible-v2}
    operator-account:string
    operator-guard:guard
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

  (deftable collections:{collection})
  (deftable tokens:{token})
  (deftable accounts:{account})

  (defcap INTERNAL () true)

  (defcap OPERATOR (collection-id:string)
    (with-read collections collection-id {
      'operator-guard:= operator-guard:guard
      }
      (enforce-guard operator-guard))
  )

  (defcap MINT (token-id:string)
    (enforce-ledger)
  )

  (defcap ADD_TO_COLLECTION:bool (collection-id:string total-unique-tokens:integer)
    @event
    true)

  (defcap INIT_COLLECTION:bool (collection-id:string collection-size:integer fungible:module{fungible-v2} price:decimal operator:string)
    @event
    true)

  (defcap RESERVE_SALE:bool (collection-id:string account:string index:integer)
    @event
    true)

  (defcap REVEAL_TOKENS:bool (collection-id:string tokens:list)
    @event
    true)

  (defun get-policy:object{token} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  ;;BIDDING
  (defun init-collection:bool
    (collection-id:string
     collection-size:integer
     fungible:module{fungible-v2}
     price:decimal
     operator:string
     operator-guard:guard )
      (insert collections collection-id {
        "id": collection-id
       ,"collection-size": collection-size
       ,"total-unique-tokens": 0
       ,"tokens": []
       ,"slots": []
       ,"reservation-price": price
       ,"reservation-fungible": fungible
       ,"operator-account": operator
       ,"operator-guard": operator-guard
      })
     (emit-event (INIT_COLLECTION collection-id collection-size fungible price operator))
  )

  (defun reserve-whitelist:bool (collection-id:string buyer:string)
    (with-read collections collection-id {
       "collection-size":= collection-size:integer
       ,"reservation-price":=amount:decimal
       ,"reservation-fungible":=fungible:module{fungible-v2}
       ,"operator-account":=operator:string
       ,"slots":= slots
      }
      (enforce (>= collection-size (length slots)) "bid has ended")
      (fungible::transfer buyer operator amount)
      (update collections collection-id {
        "slots": (+ [buyer] slots)
        })
      (emit-event (RESERVE_SALE collection-id buyer (length slots)))))


  (defun whitelist-key (collection-id:string index:integer)
    (format "{}:{}"[collection-id index])
  )

  (defun reveal-whitelist:list (collection-id:string token-ids:list)
    (with-read collections collection-id {
      "slots":= slots
     ,"collection-size":= collection-size
      }
      (with-capability (OPERATOR collection-id)
      (enforce (= collection-size (length slots)) "bid is in the process")
      (enforce (= (length token-ids) collection-size) "token list is invalid")
      (update collections collection-id {
        'tokens: (sort token-ids)
        ;;pre-project ordering
        ;;1. operator creates list of token ordering
        ;;2. oprator hashes the ordering
        ;;3. operator uploads hash at init-collection(upload policy module)
        ;;4. buyers buy their slots (index) - reserve-whitelist
        ;;5. shuffle the slots (operator cannot influence this) -  use the last block height % collection size, etc, to shuffle (last whitelist sale blockheight)
        ;;6. At reveal, token ordering is revealed, and buyers get the token at their index reveal-whitelist


        }))
      (emit-event (REVEAL_TOKENS collection-id (sort token-ids)))))

  (defun token-id:string (token-manifest-hash:string)
    (format "t:{}" [token-manifest-hash])
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (read-msg 'collection-id ))
            (manifest-hash:string (at 'hash (at 'manifest token)))
            (precision:integer (at 'precision token)))
    ;;one-off
    (enforce (= (at 'precision token) 0) "Invalid precision")
    ;; enforce token-id matches manifest hash

    (enforce (= (format "t:{}" [manifest-hash]) token-id) "Invalid token-id" )
    ;;issuer creates token
    (with-capability (OPERATOR collection-id)
      (with-read collections collection-id {
        'tokens:= collection-list,
        'total-unique-tokens:= total-unique-tokens
        }
        (insert tokens token-id
          { "id" : token-id
           ,"collection-id" : collection-id
           ,"supply": 0.0
          })
          (add-to-collection collection-id (+ 1 total-unique-tokens))
          true))
      ))

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (let* ( (token-id:string  (at 'id token))
            (whitelist-id:string (at 'hash (at 'manifest token)))
            (collection-id:string (at 'collection-id (get-token token-id))))
      (with-capability (OPERATOR collection-id)
        (with-read tokens token-id {
          'supply:= supply
          }
          (enforce (= supply 0.0) "token has been minted")
          (with-read collections collection-id
            {
              'slots:= slots
            }
           (enforce (contains account slots) "Account is not whitelisted")
            (with-capability (MINT token-id)
              (update-account token-id account guard)
            )
          ))
      )))

  (defun add-to-collection (collection-id:string total-unique-tokens:integer)
    (with-capability (INTERNAL)
    (update collections collection-id {
      'total-unique-tokens:  total-unique-tokens
      })
     )
   (emit-event (ADD_TO_COLLECTION collection-id total-unique-tokens))
  )

 (defun update-account (token-id:string account:string guard:guard)
  (require-capability (MINT token-id))
   (with-default-read accounts account {
      'tokens: [],
      'guard: guard
    } {
      'tokens:= tokens,
      'guard:= old-guard
    }
    (enforce (= guard old-guard) "account guards do not match")
    (write accounts account {
      'account:account,
      'guard: guard,
      'tokens: (+ [token-id] tokens)
     })
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
    (create-table collections) ])
