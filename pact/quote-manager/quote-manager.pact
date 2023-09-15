(namespace (read-string 'ns))

(module quote-manager GOVERNANCE

  (use policy-manager-v1)
  (use kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use util.guards1)

  (defconst GOVERNANCE-KS:string (+ (read-string 'ns) ".marmalade-admin"))

  (defcap GOVERNANCE ()
    (enforce-guard GOVERNANCE-KS))

  ; Saves reference to policy-manager
  (defschema policy-manager
    policy-manager-impl:module{policy-manager-v1}
  )

  (deftable policy-managers:{policy-manager}
    @doc "Singleton table for policy-manager reference storage"
  )

  (defun retrieve-policy-manager:module{policy-manager-v1} ()
    @doc "Retrieves the ledger implementation"
    (at 'policy-manager-impl (read policy-managers ""))
  )

  (defun init:bool(policy-manager:module{policy-manager-v1})
    @doc "Must be initiated with policy-manager information"
    (with-capability (GOVERNANCE)
      (insert policy-managers "" {
        "policy-manager-impl": policy-manager
      })
    )
    true
  )

  (defschema quote-msg
    @doc "Quote data to include in payload"
    spec:object{quote-spec}
    seller-guard:guard
    quote-guards:[guard]
  )

  (defschema quote-spec
    @doc "Quote spec of the sale"
    fungible:module{fungible-v2}
    seller-fungible-account:object{fungible-account}
    price:decimal
    amount:decimal
  )

  (defschema quote-schema
    @doc "Quote schema used in the quotes table"
    token-id:string
    spec:object{quote-spec}
    seller-guard:guard
    quote-guards:[guard]
    reserved:string
    active:bool
  )

  (defschema fungible-account
    @doc "account and guard information of a fungible"
    account:string
    guard:guard
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      spec:object{quote-spec}
    )
    @event
    true
  )


  (defcap QUOTE-GUARDS:bool
    ( sale-id:string
      token-id:string
      seller-guard:guard
      quote-guards:[guard]
    )
    @event
    true
  )

  (deftable quotes:{quote-schema})

  (defcap UPDATE-QUOTE-PRICE:bool (sale-id:string price:decimal buyer:string)
    @doc "Enforces quote-guards on update-quote-price"
    (with-read quotes sale-id {
      "quote-guards":=quote-guards
      }
      (enforce-guard-any quote-guards)
    )
    true
  )

  (defcap UPDATE-QUOTE-GUARD:bool (sale-id:string)
    @doc "Enforces seller-guard on update-quote-guard"
    (with-read quotes sale-id {
      "seller-guard":= guard
      }
      (enforce-guard guard)
    )
    true
  )

;; Quote storage functions
  (defun remove-quote-guard:bool (sale-id:string guard:guard)
    @doc "Removes a quote-guard if signed by the seller guard"
    (with-capability (UPDATE-QUOTE-GUARD sale-id)
      (with-read quotes sale-id {
          "token-id":=token-id
         ,"seller-guard":= seller-guard
         ,"quote-guards":= quote-guards
        }

        (let ((updated-guards (filter (!= guard) quote-guards)))
          (update quotes sale-id {
            "quote-guards": updated-guards
          })
        (emit-event (QUOTE-GUARDS sale-id token-id seller-guard updated-guards)))))
  )

  (defun add-quote-guard:bool (sale-id:string guard:guard)
    @doc "Adds a quote-guard if signed by the seller guard"
    (with-capability (UPDATE-QUOTE-GUARD sale-id)
      (with-read quotes sale-id {
          "token-id":=token-id
         ,"seller-guard":= seller-guard
         ,"quote-guards":= quote-guards
        }

        (let ((updated-guards (+ quote-guards [guard])))
          (update quotes sale-id {
            "quote-guards": updated-guards
          })
        (emit-event (QUOTE-GUARDS sale-id token-id seller-guard updated-guards)))))
  )

  (defun add-quote:bool (sale-id:string token-id:string quote-msg:object{quote-msg})
    @doc "Add quote in transaction data"
    (let* ( (quote-spec:object{quote-spec} (at 'spec quote-msg))
            (seller-guard:guard (at 'seller-guard quote-msg))
            (quote-guards:[guard] (at 'quote-guards quote-msg))
            (policy-manager:module{policy-manager-v1} (retrieve-policy-manager)))
        (require-capability (policy-manager::ADD-QUOTE-CALL sale-id token-id (at 'price quote-spec)))
        (validate-quote quote-spec)
        (insert quotes sale-id {
           "token-id": token-id
         , "seller-guard":seller-guard
         , "quote-guards": quote-guards
         , "spec": quote-spec
         , "reserved": ""
         , "active": true
        })
        (emit-event (QUOTE sale-id token-id quote-spec))
        (emit-event (QUOTE-GUARDS sale-id token-id seller-guard quote-guards))
       true
    )
  )

  (defun update-quote-price:bool (sale-id:string price:decimal buyer:string)
    @doc "Updates the quote price and sets the designated buyer"
    (let ((policy-manager:module{policy-manager-v1} (retrieve-policy-manager)))
      (require-capability (policy-manager::UPDATE-QUOTE-PRICE-CALL sale-id price buyer))
    )
    (with-capability (UPDATE-QUOTE-PRICE sale-id price buyer)
      (with-read quotes sale-id {
          "spec":= quote-spec
        }
        (bind quote-spec {
            "fungible":= fungible
           ,"amount":= amount
           ,"seller-fungible-account":= fungible-account
          }
          (update quotes sale-id {
            "spec": {
                "fungible": fungible
              , "amount": amount
              , "price": price
              , "seller-fungible-account": fungible-account
            }
            , "reserved": buyer
            , "active": false
            }))
      ))
    true
  )

  (defun close-quote:bool (sale-id:string)
    @doc "Closes quote at withdraw or buy"
    (let ((policy-manager:module{policy-manager-v1} (retrieve-policy-manager)))
      (require-capability (policy-manager::CLOSE-QUOTE-CALL sale-id))
    )
    (update quotes sale-id {
      "active": false
    })
    true
  )

  (defun get-quote-info:object{quote-schema} (sale-id:string)
   @doc "Get Quote information"
    (read quotes sale-id)
  )

  ;; Validate functions
  (defun validate-fungible-account (fungible:module{fungible-v2} account:object{fungible-account})
    (let ((seller-details (fungible::details (at 'account account))))
      (enforce (=
        (at 'guard seller-details) (at 'guard account))
            "Account guard does not match"))
  )

  (defun validate-quote:bool (quote-spec:object{quote-spec})
    (let* ( (fungible:module{fungible-v2} (at 'fungible quote-spec) )
            (seller-fungible-account:object{fungible-account} (at 'seller-fungible-account quote-spec))
            (amount:decimal (at 'amount quote-spec))
            (price:decimal (at 'price quote-spec))
            (sale-price:decimal (* amount price)) )
      (validate-fungible-account fungible seller-fungible-account)
      (fungible::enforce-unit sale-price)
      (enforce (>= price 0.0) "Offer price must be positive or zero")
      true)
  )

  (defun enforce-quote-active:bool (sale-id:string)
    (with-read quotes sale-id {
      "active":= active
      }
      (enforce active "QUOTE: Inactive")
    )
  )
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [(create-table policy-managers)
   (create-table quotes)
  ]
)
