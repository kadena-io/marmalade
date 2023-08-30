(namespace (read-string 'ns))

(module quote-manager GOVERNANCE

  (use kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use util.guards1)

  (defconst GOVERNANCE-KS:string (+ (read-string 'ns) ".marmalade-admin"))

  (defcap GOVERNANCE ()
    (enforce-keyset GOVERNANCE-KS))

  ;; Saves Policy Manager Guard information
  (defschema policy-manager
    policy-manager-guard:guard
  )

  (deftable policy-managers:{policy-manager}
    @doc "Singleton table for policy-manager guard storage"
  )

  (defun enforce-policy-manager:bool ()
    @doc "Enforces that function is called from the saved policy-manager"
    (with-read policy-managers "" {
      "policy-manager-guard":= policy-manager-guard
      }
      (enforce-guard policy-manager-guard)
    )
  )

  (defun init:bool(policy-manager-guard:guard)
    @doc "Must be initiated with policy-manager information"
    (with-capability (GOVERNANCE)
      (insert policy-managers "" {
        "policy-manager-guard": policy-manager-guard
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

  (defcap QUOTE_PRICE_UPDATE:bool
    ( sale-id:string
      price:decimal
      buyer:string
    )
    @event
    true
  )

  (defcap QUOTE_GUARDS:bool
    ( sale-id:string
      token-id:string
      seller-guard:guard
      quote-guards:[guard]
    )
    @event
    true
  )

  (deftable quotes:{quote-schema})

  (defcap UPDATE_QUOTE_PRICE:bool (sale-id:string)
    @doc "Enforces quote-guards on update-quote-price"
    (with-read quotes sale-id {
      "quote-guards":=quote-guards
      }
      (enforce-guard-any quote-guards)
    )
    true
  )

  (defcap UPDATE_QUOTE_GUARD:bool (sale-id:string)
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
    (with-capability (UPDATE_QUOTE_GUARD sale-id)
      (with-read quotes sale-id {
          "token-id":=token-id
         ,"seller-guard":= seller-guard
         ,"quote-guards":= quote-guards
        }

        (let ((updated-guards (filter (!= guard) quote-guards)))
          (update quotes sale-id {
            "quote-guards": updated-guards
          })
        (emit-event (QUOTE_GUARDS sale-id token-id seller-guard updated-guards)))))
  )

  (defun add-quote-guard:bool (sale-id:string guard:guard)
    @doc "Adds a quote-guard if signed by the seller guard"
    (with-capability (UPDATE_QUOTE_GUARD sale-id)
      (with-read quotes sale-id {
          "token-id":=token-id
         ,"seller-guard":= seller-guard
         ,"quote-guards":= quote-guards
        }

        (let ((updated-guards (+ quote-guards [guard])))
          (update quotes sale-id {
            "quote-guards": updated-guards
          })
        (emit-event (QUOTE_GUARDS sale-id token-id seller-guard updated-guards)))))
  )

  (defun add-quote:bool (sale-id:string token-id:string quote-msg:object{quote-msg})
    @doc "Add quote if quote-msg exists in transaction data"
    (enforce-policy-manager)
    (let* ( (quote-spec:object{quote-spec} (at 'spec quote-msg))
            (seller-guard:guard (at 'seller-guard quote-msg))
            (quote-guards:[guard] (at 'quote-guards quote-msg)))
        (validate-quote quote-spec)
        (insert quotes sale-id {
           "token-id": token-id
         , "seller-guard":seller-guard
         , "quote-guards": quote-guards
         , "spec": quote-spec
         , "reserved": ""
        })
        (emit-event (QUOTE sale-id token-id quote-spec))
        (emit-event (QUOTE_GUARDS sale-id token-id seller-guard quote-guards))
       true
    )
  )

  (defun update-quote-price:bool (sale-id:string price:decimal buyer:string)
    @doc "Updates the quote price and sets the designated buyer"
    (enforce-policy-manager)
    (with-capability (UPDATE_QUOTE_PRICE sale-id)
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
            }))
      )
      (emit-event (QUOTE_PRICE_UPDATE sale-id price buyer)))
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
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [(create-table policy-managers)
   (create-table quotes)
  ]
)
