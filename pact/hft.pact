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
    (enforce-guard
      (at 'guard
        (read ledger (key token sender)))))

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
      }
      (policy::enforce-mint
        { 'token: token, 'supply: supply, 'precision: precision }
        account amount))
  )

  (defcap BURN (token:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (with-read tokens token
      { 'policy := policy:module{token-policy-v1}
      , 'supply := supply
      , 'minimum-precision := precision
      }
      (policy::enforce-burn
        { 'token: token, 'supply: supply, 'precision: precision }
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
      (policy::enforce-init token)
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

    (defun credit:string
      ( token:string
        account:string
        guard:guard
        amount:decimal
      )
      @doc "Credit AMOUNT to ACCOUNT balance"

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
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table ledger)
    (create-table tokens) ])
