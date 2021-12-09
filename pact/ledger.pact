(namespace (read-msg 'ns))

(module ledger GOVERNANCE

  @model
    [
      (defproperty valid-account (account:string)
          (> (length account) 2))
    ]

  (use util.fungible-util)
  (use kip.token-manifest)

  (implements kip.poly-fungible-v2_DRAFT2)
  (use kip.poly-fungible-v2_DRAFT2 [account-details])

  ;;
  ;; Tables/Schemas
  ;;

  (deftable ledger:{account-details})

  (defschema token-schema
    id:string
    manifest:object{manifest}
    precision:integer
    supply:decimal
    policy:module{kip.token-policy-v1_DRAFT2}
  )

  (deftable tokens:{token-schema})

  ;;
  ;; Capabilities
  ;;

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'hft-admin)))

  ;;
  ;; poly-fungible-v2 caps
  ;;

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @event true
  )

  (defcap TOKEN:bool (id:string)
    @event
    true
  )

  ;;
  ;; Implementation caps
  ;;

  (defcap ROTATE (id:string account:string)
    @doc "Autonomously managed capability for guard rotation"
    @managed
    true)

  (defcap DEBIT (id:string sender:string)
    (enforce-guard (account-guard id sender))
  )

  (defun account-guard:guard (id:string account:string)
    (with-read ledger (key id account) { 'guard := guard } guard)
  )

  (defcap CREDIT (id:string receiver:string) true)


  (defcap UPDATE_SUPPLY ()
    "private cap for update-supply"
    true)

  (defcap MINT (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (compose-capability (CREDIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap BURN (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (compose-capability (DEBIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defschema policy-info
    policy:module{kip.token-policy-v1_DRAFT2}
    token:object{kip.token-policy-v1_DRAFT2.token-info}
  )

  (defun get-policy-info:object{policy-info} (id:string)
    (with-read tokens id
      { 'policy := policy:module{kip.token-policy-v1_DRAFT2}
      , 'supply := supply
      , 'precision := precision
      , 'manifest := manifest
      }
      { 'policy: policy
      , 'token:
        { 'id: id
        , 'supply: supply
        , 'precision: precision
        , 'manifest: manifest
        } } )
  )


  (defun create-account:string
    ( id:string
      account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger (key id account)
      { "balance" : 0.0
      , "guard"   : guard
      , "id" : id
      , "account" : account
      })
  )

  (defun total-supply:decimal (id:string)
    (with-default-read tokens id
      { 'supply : 0.0 }
      { 'supply := s }
      s)
  )

  (defun create-token
    ( id:string
      precision:integer
      manifest:object{manifest}
      policy:module{kip.token-policy-v1_DRAFT2}
    )
    (enforce-verify-manifest manifest)
    (policy::enforce-init
      { 'id: id, 'supply: 0.0, 'precision: precision, 'manifest: manifest })
    (emit-event (TOKEN id))
    (insert tokens id {
      "id": id,
      "precision": precision,
      "manifest": manifest,
      "supply": 0.0,
      "policy": policy
      })
  )

  (defun truncate:decimal (id:string amount:decimal)
    (floor amount (precision id))
  )

  (defun get-balance:decimal (id:string account:string)
    (at 'balance (read ledger (key id account)))
  )

  (defun details:object{account-details}
    ( id:string account:string )
    (read ledger (key id account))
  )

  (defun rotate:string (id:string account:string new-guard:guard)
    (with-capability (ROTATE id account)
      (enforce-transfer-policy id account account 0.0)
      (with-read ledger (key id account)
        { "guard" := old-guard }

        (enforce-guard old-guard)
        (update ledger (key id account)
          { "guard" : new-guard }))))

  (defun transfer:string
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)
    (with-capability (TRANSFER id sender receiver amount)
      (enforce-transfer-policy id sender receiver amount)
      (debit id sender amount)
      (with-read ledger (key id receiver)
        { "guard" := g }
        (credit id receiver g amount))
    )
  )

  (defun enforce-transfer-policy
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1_DRAFT2}
      , 'token := token }
      (policy::enforce-transfer token sender receiver amount))
  )

  (defun transfer-create:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)

    (with-capability (TRANSFER id sender receiver amount)
      (enforce-transfer-policy id sender receiver amount)
      (debit id sender amount)
      (credit id receiver receiver-guard amount))
  )

  (defun mint:string
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    (with-capability (MINT id account amount)
      (bind (get-policy-info id)
        { 'policy := policy:module{kip.token-policy-v1_DRAFT2}
        , 'token := token }
        (policy::enforce-mint token account amount))
      (credit id account guard amount)
      (update-supply id amount))
  )

  (defun burn:string
    ( id:string
      account:string
      amount:decimal
    )
    (with-capability (BURN id account amount)
      (bind (get-policy-info id)
        { 'policy := policy:module{kip.token-policy-v1_DRAFT2}
        , 'token := token }
        (policy::enforce-burn token account amount))
      (debit id account amount)
      (update-supply id (- amount)))
  )

  (defun debit:string
    ( id:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT id account))

    (enforce-unit id amount)

    (with-read ledger (key id account)
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update ledger (key id account)
        { "balance" : (- balance amount) }
        ))
  )

  (defun credit:string
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]
    (enforce-valid-account account)
    (enforce-unit id amount)

    (require-capability (CREDIT id account))

    (with-default-read ledger (key id account)
      { "balance" : -1.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard)
        "account guards do not match")

      (let ((is-new
             (if (= balance -1.0)
                 (enforce-reserved account guard)
               false)))

      (write ledger (key id account)
        { "balance" : (if is-new amount (+ balance amount))
        , "guard"   : retg
        , "id"   : id
        , "account" : account
        })))
  )

  (defun credit-account:string
    ( id:string
      account:string
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT"
    (credit id account (account-guard id account) amount)
  )



  (defun update-supply (id:string amount:decimal)
    (require-capability (UPDATE_SUPPLY))
    (with-default-read tokens id
      { 'supply: 0.0 }
      { 'supply := s }
      (let ((new-supply (+ s amount)))
        (emit-event (SUPPLY id new-supply))
        (update tokens id {'supply: new-supply })))
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    (let ((p (precision id)))
    (enforce
      (= (floor amount p)
         amount)
      "precision violation"))
  )

  (defun precision:integer (id:string)
    (at 'precision (read tokens id))
  )

  (defpact transfer-crosschain:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (format "{}" [(enforce false "cross chain not supported")]))
    )

  ;;
  ;; ACCESSORS
  ;;


  (defun key ( id:string account:string )
    @doc "DB key for ledger account"
    (format "{}:{}" [id account])
  )

  (defun get-manifest:object{manifest} (id:string)
    (at 'manifest (read tokens id)))

  (defun get-token-keys:[string] ()
    "Get all token identifiers"
    (keys tokens))

  (defun get-tokens:[object{token-schema}] ()
    "Get all tokens"
     (map (read tokens) (keys tokens)))

  (defun get-token:object{token-schema} (id:string)
    "Read token"
    (read tokens id)
  )

  ;;
  ;; sale
  ;;

  (defcap SALE:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (compose-capability (OFFER id seller amount timeout))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap OFFER:bool
    (id:string seller:string amount:decimal timeout:integer)
    @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
    @managed
    (enforce (sale-active timeout) "SALE: invalid timeout")
    (compose-capability (DEBIT id seller))
    (compose-capability (CREDIT id (sale-account)))
  )

  (defcap WITHDRAW:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after timeout."
    @event
    (enforce (not (sale-active timeout)) "WITHDRAW: still active")
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id seller))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap BUY:bool
    (id:string seller:string buyer:string amount:decimal timeout:integer sale-id:string)
    @doc "Completes sale OFFER to BUYER."
    @managed
    (enforce (sale-active timeout) "BUY: expired")
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id buyer))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap SALE_PRIVATE (sale-id:string) true)

  (defpact sale:bool
    ( id:string
      seller:string
      amount:decimal
      timeout:integer
    )
    (step-with-rollback
      (with-capability (SALE id seller amount timeout (pact-id))
        (offer id seller amount))
      (with-capability (WITHDRAW id seller amount timeout (pact-id))
        (withdraw id seller amount))
    )
    (step
      (let ( (buyer:string (read-msg "buyer"))
             (buyer-guard:guard (read-msg "buyer-guard")) )
        (with-capability (BUY id seller buyer amount timeout (pact-id))
          (buy id seller buyer buyer-guard amount (pact-id)))))
  )

  (defun offer
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
    (require-capability (SALE_PRIVATE (pact-id)))
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1_DRAFT2}
      , 'token := token }
      (policy::enforce-offer token seller amount (pact-id)))
    (debit id seller amount)
    (credit id (sale-account) (create-pact-guard "SALE") amount)
    (emit-event (TRANSFER id seller (sale-account) amount))
  )

  (defun withdraw
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Withdraw offer by SELLER of AMOUNT of TOKEN before TIMEOUT"
    (require-capability (SALE_PRIVATE (pact-id)))
    (debit id (sale-account) amount)
    (credit-account id seller amount)
    (emit-event (TRANSFER id (sale-account) seller amount))
  )


  (defun buy
    ( id:string
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string
    )
    @doc "Complete sale with transfer."
    (require-capability (SALE_PRIVATE (pact-id)))
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1_DRAFT2}
      , 'token := token }
      (policy::enforce-buy token seller buyer amount sale-id))
    (debit id (sale-account) amount)
    (credit id buyer buyer-guard amount)
    (emit-event (TRANSFER id (sale-account) buyer amount))
  )

  (defun sale-active (timeout:integer)
    @doc "Sale is active until TIMEOUT block height."
    (< (at 'block-height (chain-data)) timeout)
  )

  (defun sale-account:string ()
    (format "sale-{}" [(pact-id)])
  )

  (defun get-ledger-keys ()
    (keys ledger))

  (defun get-ledger-entry (key:string)
    (read ledger key))

  (defun get-ledger ()
    (map (read ledger) (keys ledger)))

)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table ledger)
    (create-table tokens) ])
