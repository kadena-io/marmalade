(namespace (read-msg 'ns))

(module hft GOVERNANCE

  @model
    [
     ;; prop-ledger-write-guard
     (property
      (forall (key:string)
       (when (row-written ledger key)
         (row-enforced ledger 'guard key)))  ;; owner write
      { 'except:
        [ transfer-crosschain ;; VACUOUS
          debit               ;; PRIVATE
          credit              ;; PRIVATE
          create-account      ;; prop-ledger-conserves-mass, prop-supply-conserves-mass
          transfer            ;; prop-ledger-conserves-mass, prop-supply-conserves-mass
          transfer-create     ;; prop-ledger-conserves-mass, prop-supply-conserves-mass
        ] } )


     ;; prop-ledger-conserves-mass
     (property
      (= (column-delta ledger 'balance) 0.0)
      { 'except:
         [ transfer-crosschain ;; VACUOUS
           debit               ;; PRIVATE
           credit              ;; PRIVATE
           burn                ;; prop-ledger-write-guard
           mint                ;; prop-ledger-write-guard
         ] } )

      ;; prop-supply-conserves-mass
      (property
       (= (column-delta tokens 'supply) 0.0)
       { 'except:
        [ transfer-crosschain ;; VACUOUS
          debit               ;; PRIVATE
          credit              ;; PRIVATE
          update-supply       ;; PRIVATE
          burn                ;; prop-ledger-write-guard
          mint                ;; prop-ledger-write-guard
       ] } )

      (defproperty enforce-valid-account (account:string)
          (> (length account) 2))
    ]

  (use fungible-util)
  (use token-manifest)

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'hft-admin)))

  (defschema entry
    token:string
    account:string
    balance:decimal
    guard:guard
    )

  (deftable ledger:{entry})

  (defun view-ledger-keys ()
    (keys ledger))

  (defun view-ledger ()
    (map (read ledger) (keys ledger)))

  (defschema token
    token:string
    manifest:object{manifest}
    minimum-precision:integer
    supply:decimal
    policy:module{token-policy-v1}
  )

  (deftable tokens:{token})

  (defun view-tokens-keys ()
    (keys tokens))

  (defun view-tokens ()
    (map (read tokens) (keys tokens)))

  (defun key ( token:string account:string )
    (format "{}:{}" [token account])
  )

  (defcap TRANSFER:bool
    ( token:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit token amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT token sender))
    (compose-capability (CREDIT token receiver))
    (with-read tokens token
      { 'policy := policy:module{token-policy-v1}
      , 'supply := supply
      , 'minimum-precision := precision
      , 'manifest := manifest
      }
      (policy::enforce-transfer
        { 'token: token, 'supply: supply, 'precision: precision, 'manifest: manifest }
        sender receiver amount))
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

  (defcap ROTATE (token:string account:string)
    @doc "Autonomously managed capability for guard rotation"
    @managed
    true)

  (defcap DEBIT (token:string sender:string)
    (enforce-guard (account-guard token sender))
  )

  (defun account-guard:guard (token:string account:string)
    (with-read ledger (key token account) { 'guard := guard } guard)
  )

  (defcap CREDIT (token:string receiver:string) true)

  (defcap UPDATE_SUPPLY ()
    "private cap for update-supply"
    true)

  (defcap MINT (token:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (with-read tokens token
      { 'policy := policy:module{token-policy-v1}
      , 'supply := supply
      , 'minimum-precision := precision
      , 'manifest := manifest
      }
      (policy::enforce-mint
        { 'token: token, 'supply: supply, 'precision: precision, 'manifest: manifest }
        account amount))
  )

  (defcap BURN (token:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (with-read tokens token
      { 'policy := policy:module{token-policy-v1}
      , 'supply := supply
      , 'minimum-precision := precision
      , 'manifest := manifest
      }
      (policy::enforce-burn
        { 'token: token, 'supply: supply, 'precision: precision, 'manifest: manifest }
        account amount))
  )

  (defcap CREATE_TOKEN (token:string)
    true
  )


  (defun create-account:string
    ( token:string
      account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger account
      { "balance" : 0.0
      , "guard"   : guard
      , "token" : token
      , "account" : account
      })
    )

    (defun total-supply:decimal (token:string)
      (with-default-read tokens token
        { 'supply : 0.0 }
        { 'supply := s }
        s)
    )

    (defun create-token
      ( token:string
        precision:integer
        manifest:object{manifest}
        policy:module{token-policy-v1}
      )
      (policy::enforce-init
        { 'token: token, 'supply: 0.0, 'precision: precision, 'manifest: manifest })
      (enforce-verify-manifest manifest)
      (with-capability (CREATE_TOKEN token)
        (insert tokens token {
          "token": token,
          "minimum-precision": precision,
          "manifest": manifest,
          "supply": 0.0,
          "policy": policy
          }))
    )

    (defun truncate:decimal (token:string amount:decimal)
      (floor amount (precision token))
    )

    (defun get-balance:decimal (token:string account:string)
      (at 'balance (read ledger (key token account)))
      )

    (defun details
      ( token:string account:string )
      (read ledger (key token account))
      )

    (defun rotate:string (token:string account:string new-guard:guard)
      (with-capability (ROTATE token account)
        (with-read ledger (key token account)
          { "guard" := old-guard }

          (enforce-guard old-guard)
          (update ledger (key token account)
            { "guard" : new-guard }))))

    (defun transfer:string
      ( token:string
        sender:string
        receiver:string
        amount:decimal
      )
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-transfer sender receiver (precision token) amount)

      (with-capability (TRANSFER token sender receiver amount)
        (debit token sender amount)
        (with-read ledger (key token receiver)
          { "guard" := g }
          (credit token receiver g amount))
      )
    )

    (defun transfer-create:string
      ( token:string
        sender:string
        receiver:string
        receiver-guard:guard
        amount:decimal
      )
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-transfer sender receiver (precision token) amount)

      (with-capability (TRANSFER token sender receiver amount)
        (debit token sender amount)
        (credit token receiver receiver-guard amount))
      )

    (defun mint:string
      ( token:string
        account:string
        guard:guard
        amount:decimal
      )
      (with-capability (MINT token account amount)
        (with-capability (CREDIT token account)
          (credit token account guard amount)))
    )

    (defun burn:string
      ( token:string
        account:string
        amount:decimal
      )
      (with-capability (BURN token account amount)
        (with-capability (DEBIT token account)
          (debit token account amount)))
    )

    (defun debit:string
      ( token:string
        account:string
        amount:decimal
      )

      (require-capability (DEBIT token account))

      (enforce-unit token amount)

      (with-read ledger (key token account)
        { "balance" := balance }

        (enforce (<= amount balance) "Insufficient funds")

        (update ledger (key token account)
          { "balance" : (- balance amount) }
          ))
      (with-capability (UPDATE_SUPPLY)
        (update-supply token (- amount)))
    )

    (defun credit-account:string
      ( token:string
        account:string
        amount:decimal
      )
      @doc "Credit AMOUNT to ACCOUNT"
      (credit token account (account-guard token account) amount)
    )

    (defun credit:string
      ( token:string
        account:string
        guard:guard
        amount:decimal
      )
      @doc "Credit AMOUNT of TOKEN to ACCOUNT/GUARD"

      @model [ (property (> amount 0.0))
               (property (enforce-valid-account account))
             ]
      (enforce-valid-account account)
      (enforce-unit token amount)

      (require-capability (CREDIT token account))

      (with-default-read ledger (key token account)
        { "balance" : -1.0, "guard" : guard }
        { "balance" := balance, "guard" := retg }
        (enforce (= retg guard)
          "account guards do not match")

        (let ((is-new
               (if (= balance -1.0)
                   (enforce-reserved account guard)
                 false)))

        (write ledger (key token account)
          { "balance" : (if is-new amount (+ balance amount))
          , "guard"   : retg
          , "token"   : token
          , "account" : account
          })
        (with-capability (UPDATE_SUPPLY)
          (update-supply token amount))
        )))

    (defun update-supply (token:string amount:decimal)
      (require-capability (UPDATE_SUPPLY))
      (with-default-read tokens token
        { 'supply: 0.0 }
        { 'supply := s }
        (update tokens token {'supply: (+ s amount)}))
    )

  (defun enforce-unit:bool (token:string amount:decimal)
    (let ((p (precision token)))
    (enforce
      (= (floor amount p)
         amount)
      "precision violation"))
  )

  (defun precision:integer (token:string)
    (at 'minimum-precision (read tokens token))
  )

  (defpact transfer-crosschain:string
    ( token:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (format "{}" [(enforce false "cross chain not supported")]))
    )

  (defun get-tokens:[string] ()
    "Get all token identifiers"
    (keys tokens))

  (defun get-token:object{token} (token:string)
    "Read token"
    (read tokens token)
  )

  ;;
  ;; sale
  ;;

  (defcap SALE
    (token:string seller:string amount:decimal timeout:integer sale:string)
    @doc "Wrapper cap/event of SALE of TOKEN by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (compose-capability (OFFER token seller amount timeout))
    (compose-capability (SALE_PRIVATE sale))
  )

  (defcap OFFER
    (token:string seller:string amount:decimal timeout:integer)
    @doc "SELLER offers AMOUNT of TOKEN until TIMEOUT."
    @managed
    (enforce (sale-active timeout) "SALE: invalid timeout")
    (compose-capability (DEBIT token seller))
    (compose-capability (CREDIT token (sale-account)))
  )

  (defcap WITHDRAW
    (token:string seller:string amount:decimal timeout:integer sale:string)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of TOKEN after timeout."
    @event
    (enforce (not (sale-active timeout)) "WITHDRAW: still active")
    (compose-capability (DEBIT token (sale-account)))
    (compose-capability (CREDIT token seller))
    (compose-capability (SALE_PRIVATE sale))
  )

  (defcap BUY
    (token:string seller:string buyer:string amount:decimal timeout:integer sale:string)
    @doc "Completes sale OFFER to BUYER."
    @managed
    (enforce (sale-active timeout) "BUY: expired")
    (with-read tokens token
      { 'policy := policy:module{token-policy-v1}
      , 'supply := supply
      , 'minimum-precision := precision
      , 'manifest := manifest
      }
      (policy::enforce-sale
        { 'token: token, 'supply: supply, 'precision: precision, 'manifest: manifest }
        seller buyer amount sale))
    (compose-capability (DEBIT token (sale-account)))
    (compose-capability (CREDIT token buyer))
    (compose-capability (SALE_PRIVATE sale))
  )

  (defcap SALE_PRIVATE (sale:string) true)


  (defpact sale
    ( token:string
      seller:string
      amount:decimal
      timeout:integer
    )
    (step-with-rollback
      (with-capability (SALE token seller amount timeout (pact-id))
        (offer token seller amount timeout))
      (with-capability (WITHDRAW token seller amount timeout (pact-id))
        (withdraw token seller amount))
    )
    (step
      (let ( (buyer:string (read-msg "buyer"))
             (buyer-guard:guard (read-msg "buyer-guard")) )
        (with-capability (BUY token seller buyer amount timeout (pact-id))
          (buy token seller buyer buyer-guard amount))))
  )

  (defun offer
    ( token:string
      seller:string
      amount:decimal
    )
    @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
    (require-capability (SALE_PRIVATE (pact-id)))
    (debit token seller amount)
    (credit token (sale-account) (create-pact-guard "SALE") amount)
  )

  (defun withdraw
    ( token:string
      seller:string
      amount:decimal
    )
    @doc "Withdraw offer by SELLER of AMOUNT of TOKEN before TIMEOUT"
    (require-capability (SALE_PRIVATE (pact-id)))
    (debit token (sale-account) amount)
    (credit-account token seller amount)
  )


  (defun buy
    ( token:string
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
    )
    @doc "Complete sale with transfer."
    (require-capability (SALE_PRIVATE (pact-id)))
    (debit token (sale-account) amount)
    (credit token buyer buyer-guard amount)
    (emit-event (TRANSFER token seller buyer amount))
  )

  (defun sale-active (timeout:integer)
    @doc "Sale is active until TIMEOUT block height."
    (< (at 'block-height (chain-data)) timeout)
  )

  (defun sale-account:string ()
    (format "p:{}" [(pact-id)])
  )



)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table ledger)
    (create-table tokens) ])
