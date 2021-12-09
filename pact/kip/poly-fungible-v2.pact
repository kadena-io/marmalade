(enforce-pact-version "3.7")

(namespace 'kip)

(interface poly-fungible-v2_DRAFT2

  (defschema account-details
    @doc
      " Account details: token ID, account name, balance, and guard."
    @model
      [ (invariant (!= id ""))
        (invariant (!= account ""))
        (invariant (>= balance 0.0))
      ]
    id:string
    account:string
    balance:decimal
    guard:guard)

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc
      " Manage transferring AMOUNT of ID from SENDER to RECEIVER. \
      \ As event, also used to notify burn (with \"\" RECEIVER) \
      \ and create (with \"\" SENDER)."
    @managed amount TRANSFER-mgr
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    @doc " Manages TRANSFER cap AMOUNT where MANAGED is the installed quantity \
         \ and REQUESTED is the quantity attempting to be granted."
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc " Emitted when supply is updated, if supported."
    @event
  )

  (defcap TOKEN:bool (id:string)
    @doc " Emitted when token ID is created."
    @event
  )

  (defun precision:integer (id:string)
    @doc
      " Return maximum decimal precision for ID."
  )

  (defun enforce-unit:bool
    ( id:string
      amount:decimal
    )
    @doc
      " Enforce that AMOUNT meets minimum precision allowed for ID."
  )

  (defun create-account:string
    ( id:string
      account:string
      guard:guard
    )
    @doc
      " Create ACCOUNT for ID with 0.0 balance, with GUARD controlling access."
    @model
      [ (property (!= id ""))
        (property (!= account ""))
      ]
  )

  (defun get-balance:decimal
    ( id:string
      account:string
    )
    @doc
      " Get balance of ID for ACCOUNT. Fails if account does not exist."
  )

  (defun details:object{account-details}
    ( id:string
      account:string
    )
    @doc
      " Get details of ACCOUNT under ID. Fails if account does not exist."
  )

  (defun rotate:string
    ( id:string
      account:string
      new-guard:guard )
    @doc
      " Rotate guard for ACCOUNT for ID to NEW-GUARD, validating against existing guard."
    @model
      [ (property (!= id ""))
        (property (!= account ""))
      ]

  )

  (defun transfer:string
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc
      " Transfer AMOUNT of ID between accounts SENDER and RECEIVER. \
      \ Fails if SENDER does not exist. Managed by TRANSFER."
    @model
      [ (property (> amount 0.0))
        (property (!= id ""))
        (property (!= sender ""))
        (property (!= receiver ""))
        (property (!= sender receiver))
      ]
  )

  (defun transfer-create:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    @doc
      " Transfer AMOUNT of ID between accounts SENDER and RECEIVER. \
      \ If RECEIVER exists, RECEIVER-GUARD must match existing guard; \
      \ if RECEIVER does not exist, account is created. \
      \ Managed by TRANSFER."
    @model
      [ (property (> amount 0.0))
        (property (!= id ""))
        (property (!= sender ""))
        (property (!= receiver ""))
        (property (!= sender receiver))
      ]
  )

  (defpact transfer-crosschain:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal
    )
    @doc
      " Transfer AMOUNT of ID between accounts SENDER on source chain \
      \ and RECEIVER on TARGET-CHAIN. If RECEIVER exists, RECEIVER-GUARD \
      \ must match existing guard. If RECEIVER does not exist, account is created."
    @model
      [ (property (> amount 0.0))
        (property (!= id ""))
        (property (!= sender ""))
        (property (!= receiver ""))
        (property (!= target-chain ""))
      ]
  )

  (defun total-supply:decimal (id:string)
    @doc
      " Give total available quantity of ID. If not supported, return 0."
  )

  (defun get-manifest:object{kip.token-manifest.manifest} (id:string)
    @doc
      " Give manifest for ID."
  )

  ;;
  ;; Sale API
  ;;

  (defcap SALE:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
  )

  (defcap OFFER:bool
    (id:string seller:string amount:decimal timeout:integer)
    @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
    @managed
  )

  (defcap WITHDRAW:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after TIMEOUT."
    @event
  )

  (defcap BUY:bool
    (id:string seller:string buyer:string amount:decimal timeout:integer sale-id:string)
    @doc "Completes sale OFFER to BUYER."
    @managed
  )

  (defpact sale:bool
    ( id:string
      seller:string
      amount:decimal
      timeout:integer
    )
    @doc " Offer->buy escrow pact of AMOUNT of token ID by SELLER with TIMEOUT in blocks. \
         \ Step 1 is offer with withdraw rollback after timeout. \
         \ Step 2 is buy, which completes using 'buyer' and 'buyer-guard' payload values."
  )

)
