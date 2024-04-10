(namespace (read-msg 'ns))

(module proof-of-us GOVERNANCE

  (defconst ADMIN-KS:string "n_31cd1d224d06ca2b327f1b03f06763e305099250.pou-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)
  (use marmalade-v2.collection-policy-v1)
  (use marmalade-v2.util-v1 [curr-time])
  (use n_eef68e581f767dd66c4d4c39ed922be944ede505.webauthn-wallet)

  (defconst POLICY:string (format "{}" [proof-of-us]))
  (defconst TOKEN-POLICIES [marmalade-v2.collection-policy-v1 proof-of-us])

  (defcap EVENT (collection-id:string event-id:string name:string uri:string)
    @doc "Used for new event discovery"
    @event
    (enforce-guard ADMIN-KS)
    true
  )

  (defcap EVENT_UPDATE (event-id:string)
    @doc "Used for updated event discovery"
    @event
    (with-read events event-id {
      'starts-at := starts-at
      ,'ends-at := ends-at
    }
      (validate-event-time starts-at ends-at)
      (enforce (> starts-at (curr-time)) "Event can't be updated after it has started")
      (enforce-guard ADMIN-KS)
    )
    true
  )

  (defcap COLLECTION_CREATION ()
    @doc "Used to guard collection creation"
    (enforce-guard ADMIN-KS)
  )

  (defcap CONNECT (event-id:string uri:string)
    @doc "Used to guarante signature of all connecting parties"
    @event
    true
  )

  (defcap ATTEND (event-id:string)
    @doc "Used to guarantee signature of attendant"
    @event
    true
  )

  (defcap INTERNAL:bool (token-id:string)
    @doc "Used to guarantee function is called only by the contract itself"
    true
  )

  (defcap COLLECTION_OPERATOR ()
    @doc "Used to guard adding tokens to a collection"
    true
  )

  (defconst EVENT-ID-MSG-KEY:string "event_id")
  (defconst ATTENDANCE-SUPPLY-KEY:string "attendance_supply")

  (defconst MIN-SIGNATURE-THRESHOLD:integer 4)

  (defun has-collection-policy:bool (policies)
    (> (length (filter (lambda (policy) (= policy marmalade-v2.collection-policy-v1)) policies)) 0))

  (defschema event-schema
    name:string
    uri:string
    collection-id:string
    token-id:string
    starts-at:integer
    ends-at:integer
  )

  (defschema supply-schema
    max-supply:integer
  )

  (deftable events:{event-schema})
  (deftable supplies:{supply-schema})

  (defun collection-guard:guard ()
    (create-capability-guard (COLLECTION_OPERATOR))
  )

  (defun create-event-collection:string (collection-name:string)
    (let ((collection-id:string (create-collection-id collection-name (collection-guard))))
      (with-capability (COLLECTION_CREATION)
        (with-capability (COLLECTION_OPERATOR)
          (install-capability (COLLECTION collection-id collection-name 0 (collection-guard) (create-principal (collection-guard))))
          (create-collection collection-name 0 (collection-guard) (create-principal (collection-guard)))
          collection-id
        )
      )
    )
  )

  (defun create-event (collection-id:string name:string uri:string starts-at:integer ends-at:integer)
    (let* ((event-id:string (create-event-id name starts-at ends-at))
        (creator-guard:guard (create-capability-guard (EVENT collection-id event-id name uri)))
        (token-id (marmalade-v2.ledger.create-token-id { 'uri: uri, 'precision: 0, 'policies: TOKEN-POLICIES } creator-guard)) )

      (validate-event-time starts-at ends-at)

      (with-capability (EVENT collection-id event-id name uri)
        (insert events event-id {
          'name: name
          ,'uri: uri
          ,'collection-id: collection-id
          ,'token-id: token-id
          ,'starts-at: starts-at
          ,'ends-at: ends-at
        })

        (with-capability (COLLECTION_OPERATOR)
          (with-capability (INTERNAL token-id)
            ; Create the attendance token
            (marmalade-v2.ledger.create-token
              token-id
              0
              uri
              TOKEN-POLICIES
              creator-guard
            )
          )
          ; Set attendance supply
          (let ((attendance-supply:integer (try 0 (read-integer ATTENDANCE-SUPPLY-KEY))))
            (write supplies token-id { "max-supply": attendance-supply })
          )
        )
        event-id
      )
    )
  )

  (defun update-event (event-id:string name:string uri:string starts-at:integer ends-at:integer)
    (with-capability (EVENT_UPDATE event-id)
      (update events event-id {
        'name: name
        ,'uri: uri
        ,'starts-at: starts-at
        ,'ends-at: ends-at
      })
    )
  )

  (defun mint-attendance-token (event-id:string attendant:string attendant-guard:guard)
    (enforce (validate-principal attendant-guard attendant) "Incorrect account guard, only principal accounts allowed")

    (validate-event event-id)

    (let* (
      (event:object{event-schema} (get-event event-id))
      (token-id:string (at 'token-id event)) )

      (with-capability (ATTEND event-id)
        (enforce-pou-guard attendant-guard)
        (install-capability (marmalade-v2.ledger.MINT token-id attendant 1.0))

        (with-capability (INTERNAL token-id)
          (marmalade-v2.ledger.mint token-id attendant attendant-guard 1.0)
        )
        token-id
      )
    )
  )

  (defun retrieve-connection-token-id:string (event-id:string uri:string)
    (marmalade-v2.ledger.create-token-id { 'uri: uri, 'precision: 0, 'policies: TOKEN-POLICIES } (create-capability-guard (CONNECT event-id uri)))
  )

  (defun create-and-mint-connection-token:string (uri:string connection-guards:[guard])
    (enforce (>= (length connection-guards) 2) "At least 2 connections are required to mint a connection token")

    (let* (
      (event-id:string (try "" (read-msg EVENT-ID-MSG-KEY)))
      (creator-guard:guard (create-capability-guard (CONNECT event-id uri)))
      (token-id (marmalade-v2.ledger.create-token-id { 'uri: uri, 'precision: 0, 'policies: TOKEN-POLICIES } creator-guard)))

      (if (= event-id "") true [
          (validate-event-collection event-id (read-msg COLLECTION-ID-MSG-KEY))
          (validate-event event-id)
        ]
      )
      (with-capability (CONNECT event-id uri)
        (enforce-guard-count connection-guards (get-guard-threshold connection-guards))
        (with-capability (INTERNAL token-id)
          (with-capability (COLLECTION_OPERATOR)
            ; Create the connection token
            (marmalade-v2.ledger.create-token
              token-id
              0
              uri
              TOKEN-POLICIES
              creator-guard
            )
            ; Limited supply to number of participants
            (write supplies token-id { "max-supply": (length connection-guards) })
          )

          (map (lambda (connection-guard:guard)

            (install-capability (marmalade-v2.ledger.MINT token-id (create-principal connection-guard) 1.0))

            (marmalade-v2.ledger.mint token-id (create-principal connection-guard) connection-guard 1.0)

          ) connection-guards)
          token-id
        )
      )
    )
  )

  ;;POLICY IMPLEMENTATION

  (defun enforce-init:bool
    ( token:object{token-info})
    @doc "The function is run at `create-token` step of marmalade-v2.ledger.create-token"

    (require-capability (marmalade-v2.policy-manager.INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) POLICY))
    (require-capability (INTERNAL (at "id" token)))

    (enforce (= (at 'precision token) 0) "Precision must be 0 for proof-of-us tokens")

    (enforce (has-collection-policy (at 'policies token)) "Collection policy is required for proof-of-us tokens")
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (marmalade-v2.policy-manager.MINT-CALL (at "id" token) account amount POLICY))
    (require-capability (INTERNAL (at "id" token)))

    (enforce (= amount 1.0) "Amount must be 1.0 for proof-of-us tokens")
    (validate-supply token amount account)
    (let ((balance:decimal (try 0.0 (marmalade-v2.ledger.get-balance (at "id" token) account))))
      (enforce (= 0.0 balance) "Account already has a token")
    )
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn is not allowed!")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer is not allowed!")
  )

  ;;UTILITY FUNCTIONS

  (defun get-guard-threshold:integer (guards:[guard])
    (let ((count:integer (length guards)))
      (if (> count MIN-SIGNATURE-THRESHOLD)
        (let ((threshold:integer (+ 1 (/ count 2))))
          (if (< threshold MIN-SIGNATURE-THRESHOLD)
            MIN-SIGNATURE-THRESHOLD
            threshold
          )
        )
        count
      )
    )
  )

  (defun enforce-guard-count:bool (guards:[guard] threshold:integer)
    "Will succeed if at least the threshold of guards is successfully enforced."
    (let* (
      (attemps:[bool] (map (try-enforce-guard) guards))
      (filtered:[bool] (filter (= true) attemps))
      (len:integer (length filtered))
    )
      (enforce (<= threshold len) "Guard threshold not met")
    )
  )

  (defun try-enforce-guard (guard:guard)
    (try false (enforce-pou-guard guard))
  )

  (defun enforce-pou-guard:bool (guard:guard)
    (enforce-one "Neither keyset or capability guard passed" [
      (enforce-guard guard)
      (enforce-authenticated (create-principal guard))
    ])
  )

  (defun validate-event-time:bool (starts-at:integer ends-at:integer)
    (enforce (> ends-at (curr-time)) "Event end time must be in the future")
    (enforce (> starts-at (curr-time)) "Event start time must be in the future")
    (enforce (> ends-at starts-at) "Event must end after it starts")

    true
  )

  (defun validate-event:bool (event-id:string)
    (let* (
      (event:object{event-schema} (get-event event-id))
      (starts-at:integer (at 'starts-at event))
      (ends-at:integer (at 'ends-at event)) )

      (enforce (and (>= (curr-time) starts-at) (<= (curr-time) ends-at)) "Minting is not allowed outside of event time")
    )
  )

  (defun validate-event-collection (event-id:string collection-id:string)
    (let* ((event:object{event-schema} (get-event event-id)))
      (enforce (= collection-id (at 'collection-id event)) "Event does not belong to this collection")
    )
  )

  (defun validate-supply:bool (token:object{token-info} amount:decimal account:string)
    (with-read supplies (at "id" token) {
        "max-supply" := max-supply
      }
      (if (> max-supply 0)
        (enforce (<= (+ amount (at 'supply token)) (* max-supply 1.0)) "Exceeds max supply")
        true
      )
    )
  )

  (defun create-event-id:string (name:string starts-at:integer ends-at:integer)
    (format "proof-of-us:{}" [(hash [name starts-at ends-at])])
  )

  (defun get-event:object{event-schema} (event-id:string)
    (read events event-id)
  )

  (defun has-minted-attendance-token (event-id:string attendant:string)
    (with-read events event-id { 'token-id := token-id }
      (> (marmalade-v2.ledger.get-balance token-id attendant) 0.0)
    )
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [
    (create-table events)
    (create-table supplies)
  ]
)

(enforce-guard ADMIN-KS)
