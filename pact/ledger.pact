(namespace (read-msg 'ns))

(module ledger GOVERNANCE

  @model
    [
      (defproperty valid-account (account:string)
          (> (length account) 2))
    ]

  (use util.fungible-util)
  (use marmalade.policy-manager)
  (implements kip.poly-fungible-v3)
  (use kip.poly-fungible-v3 [account-details sender-balance-change receiver-balance-change])
  (use kip.token-policy-v2 [token-policies])

  ;;
  ;; Tables/Schemas
  ;;

  (deftable ledger:{account-details})

  (defschema token-schema
    id:string
    uri:string
    precision:integer
    supply:decimal
    policies:object{token-policies}
  )

  (defschema token-details
    uri:string
    precision:integer
    policies:object{token-policies}
  )

  (deftable tokens:{token-schema})

  ;;
  ;; Capabilities
  ;;

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin)))

  ;;
  ;; poly-fungible-v3 caps
  ;;

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defcap XTRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      target-chain:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce false "cross chain not supported")
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
    @doc " Emitted when supply is updated, if supported."
    @event true
  )

  (defcap TOKEN:bool (id:string precision:integer supply:decimal policies:object{token-policies} uri:string)
    @event
    true
  )

  (defcap RECONCILE:bool
    ( token-id:string
      amount:decimal
      sender:object{sender-balance-change}
      receiver:object{receiver-balance-change}
    )
    @doc " For accounting via events. \
         \ sender = {account: '', previous: 0.0, current: 0.0} for mint \
         \ receiver = {account: '', previous: 0.0, current: 0.0} for burn"
    @event
    true
  )

  (defcap ACCOUNT_GUARD:bool (id:string account:string guard:guard)
    @doc " Emitted when ACCOUNT guard is updated."
    @event
    true
  )


  ;; dependent on marmalade
  (defcap ROTATE_POLICY (token-id:string account:string)
    @event
    (enforce (= (get-balance token-id account) (total-supply token-id)) "Account doesn't own token")
    (enforce-guard (account-guard token-id account)))

  (defun rotate-adjustable-policy
    ( token-id:string
      account:string
      rotate-policies:[module{kip.token-policy-v2}] )
    (with-capability (ROTATE_POLICY token-id account) ;; needs sigs from token owner
      (with-read tokens token-id {
        "policies":= old-policies
        }
        (let* ((new-policies:object{token-policies} (+
                  {'adjustable-policies: rotate-policies}
                  old-policies
              )))
        (update tokens token-id {
          "policies": new-policies
        })
    ))
  ))


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
    (with-read ledger (key id account) { 'guard := g } g)
  )

  (defcap CREDIT (id:string receiver:string) true)

  (defcap UPDATE_SUPPLY ()
    "private cap for update-supply"
    true)

  (defcap MINT (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (CREDIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap BURN (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (DEBIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap LEDGER:bool ()
    @doc "Ledger module guard for policies to be able to validate access to policy operations."
    true
  )

  (defun ledger-guard:guard ()
    (create-capability-guard (LEDGER))
  )

  ;  Transform token-schema object to token-info object
  (defun get-token-info:object{kip.token-policy-v2.token-info} (id:string)
    (with-read tokens id
     { 'policies := policies:object{token-policies}
     , 'supply := supply
     , 'precision := precision
     , 'uri := uri
     }
     {
       'id: id
       , 'supply: supply
       , 'precision: precision
       , 'uri: uri
       , 'policies: policies
     } )
  )

  (defun create-account:bool
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
    (emit-event (ACCOUNT_GUARD id account guard))
  )

  (defun total-supply:decimal (id:string)
    (with-default-read tokens id
      { 'supply : 0.0 }
      { 'supply := s }
      s)
  )

  (defun create-token-id:string (token-details:object{token-details})
    (format "t:{}" [(hash token-details)])
  )

  (defun create-token:bool
    ( id:string
      precision:integer
      uri:string
      policies:object{token-policies}
    )
    (with-capability (LEDGER)
      (let ((token-details { 'uri: uri, 'precision: precision, 'policies: policies }))
       (enforce-token-reserved id token-details)
      )
      ;; maps policy list and calls policy::enforce-init
      (marmalade.policy-manager.enforce-init
        { 'id: id, 'supply: 0.0, 'precision: precision, 'uri: uri,  'policies: policies})

      (insert tokens id {
        "id": id,
        "uri": uri,
        "precision": precision,
        "supply": 0.0,
        "policies": policies
      })
      (emit-event (TOKEN id precision 0.0 policies uri)))
  )

  (defun enforce-token-reserved:bool (token-id:string token-details:object{token-details})
    @doc "Enforce reserved token-id name protocols."
    (let ((r (check-reserved token-id)))
      (if (= "" r) true
        (if (= "t" r)
          (enforce
            (= token-id
               (create-token-id token-details))
            "Token protocol violation")
          (enforce false
            (format "Unrecognized reserved protocol: {}" [r]) )))))

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

  (defun rotate:bool (id:string account:string new-guard:guard)
    (with-capability (ROTATE id account)
      (enforce-transfer-policy id account account 0.0)
      (with-read ledger (key id account)
        { "guard" := old-guard }

        (enforce-guard old-guard)
        (update ledger (key id account)
          { "guard" : new-guard })
        (emit-event (ACCOUNT_GUARD id account new-guard)))))

  (defun transfer:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (with-capability (LEDGER)
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-transfer sender receiver (precision id) amount)
      (with-capability (TRANSFER id sender receiver amount)
        (enforce-transfer-policy id sender receiver amount)
        (with-read ledger (key id receiver)
          { "guard" := g }
          (let
            ( (sender (debit id sender amount))
              (receiver (credit id receiver g amount))
            )
            (emit-event (RECONCILE id amount sender receiver))
          )
        )
      ))
  )

  (defun enforce-transfer-policy
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (let ((token (get-token-info id)))
      (marmalade.policy-manager.enforce-transfer token sender (account-guard id sender) receiver amount))
  )

  (defun transfer-create:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    (with-capability (LEDGER)
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-transfer sender receiver (precision id) amount)

      (with-capability (TRANSFER id sender receiver amount)
        (enforce-transfer-policy id sender receiver amount)
        (let
          (
            (sender (debit id sender amount))
            (receiver (credit id receiver receiver-guard amount))
          )
          (emit-event (RECONCILE id amount sender receiver))
        )))
  )

  (defun mint:bool
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    (with-capability (LEDGER)
      (with-capability (MINT id account amount)
        (let ((token (get-token-info id)))
          (marmalade.policy-manager.enforce-mint token account guard amount))
        (let
          (
            (receiver (credit id account guard amount))
            (sender:object{sender-balance-change}
              {'account: "", 'previous: 0.0, 'current: 0.0})
          )
          (emit-event (RECONCILE id amount sender receiver))
          (update-supply id amount)
        )))
  )

  (defun burn:bool
    ( id:string
      account:string
      amount:decimal
    )
    (with-capability (LEDGER)
      (with-capability (BURN id account amount)
        (let ((token (get-token-info id)))
          (marmalade.policy-manager.enforce-burn token account amount))
        (let
          (
            (sender (debit id account amount))
            (receiver:object{receiver-balance-change}
              {'account: "", 'previous: 0.0, 'current: 0.0})
          )
          (emit-event (RECONCILE id amount sender receiver))
          (update-supply id (- amount))
        )))
  )

  (defun debit:object{sender-balance-change}
    ( id:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT id account))

    (enforce-unit id amount)

    (with-read ledger (key id account)
      { "balance" := old-bal }

      (enforce (<= amount old-bal) "Insufficient funds")

      (let ((new-bal (- old-bal amount)))
        (update ledger (key id account)
          { "balance" : new-bal }
          )
        {'account: account, 'previous: old-bal, 'current: new-bal}
      ))
  )

  (defun credit:object{receiver-balance-change}
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
      { "balance" := old-bal, "guard" := retg }
      (enforce (= retg guard)
        "account guards do not match")

      (let* ((is-new
               (if (= old-bal -1.0)
                   (enforce-reserved account guard)
                 false))
              (new-bal (if is-new amount (+ old-bal amount)))
            )

      (write ledger (key id account)
        { "balance" : new-bal
        , "guard"   : retg
        , "id"   : id
        , "account" : account
        })
        (if is-new (emit-event (ACCOUNT_GUARD id account retg)) true)
        {'account: account, 'previous: (if is-new 0.0 old-bal), 'current: new-bal}
      ))
  )

  (defun credit-account:object{receiver-balance-change}
    ( id:string
      account:string
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT"
    (credit id account (account-guard id account) amount)
  )

  (defun update-supply:bool (id:string amount:decimal)
    (require-capability (UPDATE_SUPPLY))
    (with-default-read tokens id
      { 'supply: 0.0 }
      { 'supply := s }
      (let ((new-supply (+ s amount)))
        (update tokens id {'supply: new-supply })
        (emit-event (SUPPLY id new-supply))))
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

  (defpact transfer-crosschain:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (format "{}" [(enforce false "cross chain not supported")]) false))

  ;;
  ;; ACCESSORS
  ;;

  (defun key:string ( id:string account:string )
    @doc "DB key for ledger account"
    (format "{}:{}" [id account])
  )

  (defun get-uri:string (id:string)
    (at 'uri (read tokens id)))

  ;;
  ;; sale
  ;;

  (defcap SALE:bool
    (id:string seller:string amount:decimal timeout:time sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (LEDGER))
    (compose-capability (OFFER id seller amount timeout))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap OFFER:bool
    (id:string seller:string amount:decimal timeout:time)
    @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
    @managed
    (enforce (sale-active timeout) "SALE: invalid timeout")
    (compose-capability (DEBIT id seller))
    (compose-capability (CREDIT id (sale-account)))
  )

  (defcap WITHDRAW:bool
    (id:string seller:string amount:decimal timeout:time sale-id:string)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after timeout."
    @event
    (enforce (not (sale-active timeout)) "WITHDRAW: still active")
    (compose-capability (LEDGER))
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id seller))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap BUY:bool
    (id:string seller:string buyer:string amount:decimal timeout:time sale-id:string)
    @doc "Completes sale OFFER to BUYER."
    @managed
    (enforce (sale-active timeout) "BUY: expired")
    (compose-capability (LEDGER))
    (compose-capability (SALE_PRIVATE sale-id))
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id buyer))
  )

  (defcap SALE_PRIVATE:bool (sale-id:string) true)

  (defpact sale:bool
    ( id:string
      seller:string
      amount:decimal
      timeout:time
    )
    (step-with-rollback
      (with-capability (SALE id seller amount timeout (pact-id))
        (offer id seller amount))
      (with-capability (WITHDRAW id seller amount timeout (pact-id))
        (withdraw id seller amount)))
    (step
      (let ( (buyer:string (read-msg "buyer"))
             (buyer-guard:guard (read-msg "buyer-guard")) )
        (with-capability (BUY id seller buyer amount timeout (pact-id))
          (buy id seller buyer buyer-guard amount (pact-id))))))


  (defun offer:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
    (require-capability (SALE_PRIVATE (pact-id)))
    (let ((token (get-token-info id)))
      (marmalade.policy-manager.enforce-offer token seller amount (pact-id)))
    (let
      (
        (sender (debit id seller amount))
        (receiver (credit id (sale-account) (create-capability-pact-guard (SALE_PRIVATE (pact-id))) amount))
      )
      (emit-event (TRANSFER id seller (sale-account) amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun withdraw:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Withdraw offer by SELLER of AMOUNT of TOKEN"
    (require-capability (SALE_PRIVATE (pact-id)))
    (let ((token (get-token-info id)))
      (marmalade.policy-manager.enforce-withdraw token seller amount (pact-id)))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit-account id seller amount))
      )
      (emit-event (TRANSFER id (sale-account) seller amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )


  (defun buy:bool
    ( id:string
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string
    )
    @doc "Complete sale with transfer."
    (require-capability (SALE_PRIVATE (pact-id)))
    (let ((token (get-token-info id)))
      (marmalade.policy-manager.enforce-buy token seller buyer buyer-guard amount sale-id))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit id buyer buyer-guard amount))
      )
      (emit-event (TRANSFER id (sale-account) buyer amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun sale-active:bool (timeout:time)
    @doc "Sale is active until TIMEOUT block height."
    (< (at 'block-time (chain-data)) timeout)
  )

  (defun sale-account:string ()
    (create-principal (create-capability-pact-guard (SALE_PRIVATE (pact-id))))
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table ledger)
    (create-table tokens) ])
