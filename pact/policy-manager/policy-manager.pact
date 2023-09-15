(namespace (read-string 'ns))

(module policy-manager GOVERNANCE

  (defconst GOVERNANCE-KS:string (+ (read-string 'ns) ".marmalade-admin"))

  (defcap GOVERNANCE ()
    (enforce-guard GOVERNANCE-KS))

  (implements policy-manager-v1)
  (use kip.token-policy-v2 [token-info])
  (use util.fungible-util)
  (use ledger-v1)
  (use quote-manager)
  (use quote-manager [quote-spec quote-msg fungible-account])

  (defconst QUOTE-MSG-KEY:string "quote"
    @doc "Payload field for quote spec")

  (defconst BUYER-FUNGIBLE-ACCOUNT-MSG-KEY "buyer_fungible_account"
    @doc "Payload field for buyer's fungible account")

  (defcap ESCROW (sale-id:string)
    @doc "Capability to be used as escrow's capability guard"
    true
  )

  (defcap MAP-ESCROWED-BUY:bool (
    sale-id:string
    token:object{token-info}
    seller:string
    buyer:string
    buyer-guard:guard
    amount:decimal
    policies:[module{kip.token-policy-v2}]
  )
    @doc "Capability to grant internal access to the map-escrowed-buy function"
    true
  )

  ;;
  ;; policy-manager-v1 caps to be able to validate access to quote-manager & policy operations.
  ;;
  (defcap INIT-CALL:bool (id:string precision:integer uri:string policy:module{kip.token-policy-v2})
    true
  )

  (defcap TRANSFER-CALL:bool (id:string sender:string receiver:string amount:decimal policy:module{kip.token-policy-v2})
    true
  )

  (defcap MINT-CALL:bool (id:string account:string amount:decimal policy:module{kip.token-policy-v2})
    true
  )

  (defcap BURN-CALL:bool (id:string account:string amount:decimal policy:module{kip.token-policy-v2})
    true
  )

  (defcap OFFER-CALL:bool (id:string seller:string amount:decimal sale-id:string timeout:integer policy:module{kip.token-policy-v2})
    true
  )

  (defcap WITHDRAW-CALL:bool (id:string seller:string amount:decimal sale-id:string timeout:integer policy:module{kip.token-policy-v2})
    true
  )

  (defcap BUY-CALL:bool (id:string seller:string buyer:string amount:decimal sale-id:string policy:module{kip.token-policy-v2})
    true
  )

  (defcap ADD-QUOTE-CALL:bool (sale-id:string token-id:string price:decimal)
    true
  )

  (defcap CLOSE-QUOTE-CALL:bool (sale-id:string)
    true
  )

  (defcap UPDATE-QUOTE-PRICE-CALL:bool (sale-id:string price:decimal buyer:string)
    true
  )

  (defun get-escrow-account:object{fungible-account} (sale-id:string)
    { 'account: (create-principal (create-capability-guard (ESCROW sale-id)))
    , 'guard: (create-capability-guard (ESCROW sale-id))
    })

  ; Saves reference to ledger
  (defschema ledger
    ledger-impl:module{ledger-v1}
  )

  (deftable ledgers:{ledger}
    @doc "Singleton table for ledger reference storage")

  (defun retrieve-ledger:module{ledger-v1} ()
    @doc "Retrieves the ledger implementation"
    (at 'ledger-impl (read ledgers ""))
  )

  (defun init:bool(ledger:module{ledger-v1})
    @doc "Must be initiated with ledger implementation"
    (with-capability (GOVERNANCE)
      (insert ledgers "" {
        "ledger-impl": ledger
      })
    )
    true
  )

  ; Saves Concrete policy information
  (defschema concrete-policy
    policy:module{kip.token-policy-v2}
  )

  (defcap CONCRETE-POLICY:bool (policy-field:string policy:module{kip.token-policy-v2})
    @event
    (enforce-guard GOVERNANCE-KS))

  (deftable concrete-policies:{concrete-policy})

  (defconst NON_FUNGIBLE_POLICY:string 'non-fungible-policy )
  (defconst ROYALTY_POLICY:string 'royalty-policy )
  (defconst COLLECTION_POLICY:string 'collection-policy )
  (defconst GUARD_POLICY:string 'guard-policy )
  (defconst CONCRETE_POLICY_LIST:[string]
    [NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY] )

  (defun write-concrete-policy:bool (policy-field:string policy:module{kip.token-policy-v2})
    (contains policy-field CONCRETE_POLICY_LIST)
    (with-capability (CONCRETE-POLICY policy-field policy)
      (write concrete-policies policy-field {
        "policy": policy
        }
      )
    true)
  )

  (defun get-concrete-policy:module{kip.token-policy-v2} (policy-field:string)
    (with-read concrete-policies policy-field {
      "policy":= policy
      }
      policy)
  )

  ; Capbilities to guard internal functions

  (defcap OFFER:bool
    ( sale-id:string
    )
    @doc "Capability to grant internal transaction inside OFFER"
    true
  )

  (defcap BUY:bool
    ( sale-id:string
    )
    @doc "Capability to grant internal transaction inside BUY"
    true
  )

  (defcap WITHDRAW:bool
    ( sale-id:string
    )
    @doc "Capability to grant internal transaction inside WITHDRAW"
    true
  )


  ; Map list of policy functions

  (defun enforce-init:[bool]
    (token:object{token-info})
    (let ((ledger:module{ledger-v1} (retrieve-ledger)))
      (require-capability (ledger::INIT-CALL (at "id" token) (at "precision" token) (at "uri" token)))
    )

    (map (lambda (policy:module{kip.token-policy-v2})
      (with-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) policy)
        (policy::enforce-init token)
      )
    ) (at 'policies token))
  )

  (defun enforce-mint:[bool]
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (let ((ledger:module{ledger-v1} (retrieve-ledger)))
      (require-capability (ledger::MINT-CALL (at "id" token) account amount))
    )
    (map (lambda (policy:module{kip.token-policy-v2})
      (with-capability (MINT-CALL (at "id" token) account amount policy)
        (policy::enforce-mint token account guard amount)
      )
    ) (at 'policies token))
  )

  (defun enforce-burn:[bool]
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (let ((ledger:module{ledger-v1} (retrieve-ledger)))
      (require-capability (ledger::BURN-CALL (at "id" token) account amount))
    )
    (map (lambda (policy:module{kip.token-policy-v2})
      (with-capability (BURN-CALL (at "id" token) account amount policy)
        (policy::enforce-burn token account amount)
      )
    ) (at 'policies token))
  )

  (defun enforce-offer:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    @doc " Executed at `offer` step of marmalade.ledger.                             \
    \ Required msg-data keys:                                                        \
    \ * (optional) quote:object{quote-msg} - sale is registered as a quoted fungible \
    \ sale if present. If absent, sale proceeds without quotes."
    (let ((ledger:module{ledger-v1} (retrieve-ledger)))
      (require-capability (ledger::OFFER-CALL (at "id" token) seller amount timeout sale-id))
    )
    (enforce-sale-pact sale-id)
    ; Check if quote-msg exists
    (if (exists-msg-quote QUOTE-MSG-KEY)
      ; true - insert quote message and create escrow account in fungible
      [
        (let* (
          (quote:object{quote-msg} (read-msg QUOTE-MSG-KEY))
          (quote-spec:object{quote-spec} (at 'spec quote))
          (fungible:module{fungible-v2} (at 'fungible quote-spec))
          (escrow-account:object{fungible-account} (get-escrow-account sale-id))
        )
          (with-capability (ADD-QUOTE-CALL sale-id (at 'id token) (at 'price quote-spec))
            (add-quote sale-id (at 'id token) quote)
          )
          (fungible::create-account (at 'account escrow-account) (at 'guard escrow-account))
        )
      ]
      ; false - skip
      true)
    (map (lambda (policy:module{kip.token-policy-v2})
      (with-capability (OFFER-CALL (at "id" token) seller amount sale-id timeout policy)
        (policy::enforce-offer token seller amount timeout sale-id)
      )
    ) (at 'policies token))
  )

  (defun enforce-withdraw:[bool]
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    @doc " Executed at `withdraw` step of marmalade.ledger."
    (let ((ledger:module{ledger-v1} (retrieve-ledger)))
      (require-capability (ledger::WITHDRAW-CALL (at "id" token) seller amount timeout sale-id))
    )
    (enforce-sale-pact sale-id)

    (if (exists-quote sale-id)
      (let* (
        (quote (get-quote-info sale-id)))
        (enforce-quote-active sale-id)
        (with-capability (CLOSE-QUOTE-CALL sale-id)
          (close-quote sale-id)
        )
      )
      true
    )
    (map (lambda (policy:module{kip.token-policy-v2})
      (with-capability (WITHDRAW-CALL (at "id" token) seller amount sale-id timeout policy)
        (policy::enforce-withdraw token seller amount timeout sale-id)
      )
    ) (at 'policies token))
  )

  (defun enforce-buy:[bool]
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
      @doc " Executed at `buy` step of marmalade.ledger.                                 \
      \ Required msg-data keys:                                                          \
      \ * (optional) buyer_fungible_account:string - The fungible account of the buyer   \
      \ which transfers the fungible to the escrow account. Only required if the sale is \
      \ a quoted sale. "
    (let ((ledger:module{ledger-v1} (retrieve-ledger)))
      (require-capability (ledger::BUY-CALL (at "id" token) seller buyer amount sale-id))
    )
    (enforce-sale-pact sale-id)

    ; Checks if quote is saved at offer
    (if (exists-quote sale-id)
      ; quote is used
      [
        (let* (
          (quote (get-quote-info sale-id))
          (spec:object{quote-spec} (at 'spec quote))
          (price:decimal (at 'price spec)))

          ; Checks if price is final
          (enforce (> price 0.0) "Price must be finalized before buy")
          (with-capability (MAP-ESCROWED-BUY sale-id token seller buyer buyer-guard amount (at 'policies token))
            (map-escrowed-buy sale-id token seller buyer buyer-guard amount (at 'policies token))
          )
          (with-capability (CLOSE-QUOTE-CALL sale-id)
            (close-quote sale-id)
          )
        )
      ]
      ; quote is not used
      (map (lambda (policy:module{kip.token-policy-v2})
        (with-capability (BUY-CALL (at "id" token) seller buyer amount sale-id policy)
          (policy::enforce-buy token seller buyer buyer-guard amount sale-id)
        )
      ) (at 'policies token))
    )
  )

  (defun enforce-transfer:[bool]
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (let ((ledger:module{ledger-v1} (retrieve-ledger)))
      (require-capability (ledger::TRANSFER-CALL (at "id" token) sender receiver amount))
    )
    (map (lambda (policy:module{kip.token-policy-v2})
      (with-capability (TRANSFER-CALL (at "id" token) sender receiver amount policy)
        (policy::enforce-transfer token sender guard receiver amount)
      )
    ) (at 'policies token))
  )

  ; Sale/Escrow Functions
  (defcap RESERVE-SALE-AT-PRICE:bool
    ( sale-id:string
      price:decimal
      buyer:string
      buyer-guard:guard
    )
    @event
    true
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun reserve-sale-at-price:bool (
    sale-id:string
    price:decimal
    buyer:string
    buyer-guard:guard
    quote-account:string
    )
    @doc "Reserves the token for buyer and transfers funds"

    (enforce (> price 0.0) "price must be positive")
    (enforce-reserved buyer buyer-guard)
    (enforce-quote-active sale-id)
    (with-capability (UPDATE-QUOTE-PRICE-CALL sale-id price buyer)
      (with-capability (RESERVE-SALE-AT-PRICE sale-id price buyer buyer-guard)
        (install-capability (UPDATE-QUOTE-PRICE sale-id price buyer))
        (update-quote-price sale-id price buyer)
      )
    )

    (let* (
      (escrow-account:object{fungible-account} (get-escrow-account sale-id))
      (quote (get-quote-info sale-id))
      (spec:object{quote-spec} (at 'spec quote))
      (fungible:module{fungible-v2} (at 'fungible spec))
      (amount:decimal (at 'amount spec))
      (sale-price:decimal (floor (* price amount) (fungible::precision))))

      ; Transfer buy-amount to escrow account
      (install-capability (fungible::TRANSFER quote-account (at 'account escrow-account) sale-price))
      (fungible::transfer-create quote-account (at 'account escrow-account) (at 'guard escrow-account) sale-price)
    )
    true
  )

  (defun map-escrowed-buy:bool
    ( sale-id:string
      token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      policies:[module{kip.token-policy-v2}]
    )
    (require-capability (MAP-ESCROWED-BUY sale-id token seller buyer buyer-guard amount policies))
    (let* (
           (escrow-account:object{fungible-account} (get-escrow-account sale-id))
           (quote:object{quote-schema} (get-quote-info sale-id))
           (reserved-buyer:string (at 'reserved quote))
           (spec:object{quote-spec} (at 'spec quote))
           (fungible:module{fungible-v2} (at 'fungible spec))
           (seller-fungible-account:object{fungible-account} (at 'seller-fungible-account spec))
           (price:decimal (at 'price spec))
           (sale-price:decimal (floor (* price amount) (fungible::precision)))
      )

       (if (= reserved-buyer "")
        ; No reserved buyer, transfer from buyer to escrow
        (fungible::transfer-create (read-msg BUYER-FUNGIBLE-ACCOUNT-MSG-KEY) (at 'account escrow-account) (at 'guard escrow-account) sale-price)
        ; Reserved buyer, escrow has already been funded
        (enforce (= reserved-buyer buyer) "Reserved buyer must be buyer")
       )

       (with-capability (ESCROW sale-id)
         ; Run policies::enforce-buy
         (map (lambda (policy:module{kip.token-policy-v2})
            (with-capability (BUY-CALL (at "id" token) seller buyer amount sale-id policy)
              (policy::enforce-buy token seller buyer buyer-guard amount sale-id)
            )
          ) (at 'policies token))
         ; Transfer Escrow account to seller
         (let (
               (balance:decimal (fungible::get-balance (at 'account escrow-account)))
             )
             (install-capability (fungible::TRANSFER (at 'account escrow-account) (at 'account seller-fungible-account) balance))
             (fungible::transfer (at 'account escrow-account) (at 'account seller-fungible-account) balance)
         )
       )
       true
    )
  )

  ; Utility functions

  (defun exists-quote:bool (sale-id:string)
    @doc "Looks up quote table for quote"
    (try false (let ((q (get-quote-info sale-id))) true))
  )

  (defun exists-msg-decimal:bool (msg:string)
    @doc "Checks env-data field and see if the msg is a decimal"
    (let  ((d:decimal (try -1.0 (read-decimal msg))))
      (!= d -1.0))
  )

  (defun exists-msg-quote:bool (msg:string)
    @doc "Checks env-data field and see if the msg is a object"
    (let ((o:object (try {} (read-msg msg))))
      (!= o {}))
  )
)

(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table ledgers)
    (create-table concrete-policies)
  ])
