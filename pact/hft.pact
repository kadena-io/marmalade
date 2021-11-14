(namespace (read-msg 'ns))

(module hft GOVERNANCE

  @model
    [
      (defproperty valid-account (account:string)
          (> (length account) 2))
    ]

  (use util.fungible-util)
  (use kip.token-manifest)

  (implements kip.poly-fungible-v2)
  (use kip.poly-fungible-v2 [account-details])

  ;;
  ;; Tables/Schemas
  ;;

  (deftable ledger:{account-details})

  (defschema token-schema
    id:string
    manifest:object{manifest}
    precision:integer
    supply:decimal
    policy:module{token-policy-v1}
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
    (enforce-guard
      (at 'guard
        (read ledger (key id sender)))))

  (defcap CREDIT (id:string receiver:string) true)

  (defcap UPDATE_SUPPLY ()
    "private cap for update-supply"
    true)

  (defcap MINT (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (with-read tokens id
      { 'policy := policy:module{token-policy-v1}
      , 'supply := supply
      , 'precision := precision
      }
      (policy::enforce-mint
        { 'id: id, 'supply: supply, 'precision: precision }
        account amount))
  )

  (defcap BURN (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (with-read tokens id
      { 'policy := policy:module{token-policy-v1}
      , 'supply := supply
      , 'precision := precision
      }
      (policy::enforce-burn
        { 'id: id, 'supply: supply, 'precision: precision }
        account amount))
  )



  (defun create-account:string
    ( id:string
      account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger account
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
      policy:module{token-policy-v1}
    )
    (policy::enforce-init id)
    (enforce-verify-manifest manifest)
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
      (debit id sender amount)
      (with-read ledger (key id receiver)
        { "guard" := g }
        (credit id receiver g amount))
    )
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
      (with-capability (CREDIT id account)
        (credit id account guard amount)))
  )

  (defun burn:string
    ( id:string
      account:string
      amount:decimal
    )
    (with-capability (BURN id account amount)
      (with-capability (DEBIT id account)
        (debit id account amount)))
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
    (with-capability (UPDATE_SUPPLY)
      (update-supply id (- amount)))
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
        })
      (with-capability (UPDATE_SUPPLY)
        (update-supply id amount))
      ))
  )

  (defun update-supply (id:string amount:decimal)
    (require-capability (UPDATE_SUPPLY))
    (with-default-read tokens id
      { 'supply: 0.0 }
      { 'supply := s }
      (update tokens id {'supply: (+ s amount)}))
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

  (defun get-tokens:[string] ()
    "Get all token identifiers"
    (keys tokens))

  (defun get-token:object{token-schema} (id:string)
    "Read token"
    (read tokens id)
  )

  (defun get-ledger-keys ()
    (keys ledger))

  (defun get-ledger ()
    (map (read ledger) (keys ledger)))

)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table ledger)
    (create-table tokens) ])
