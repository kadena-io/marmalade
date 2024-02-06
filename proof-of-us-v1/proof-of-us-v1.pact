(namespace (read-msg 'ns))

(module proof-of-us-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (use marmalade-v2.ledger)
  (use marmalade-v2.util-v1)

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
      (enforce (> starts-at (curr-time)) "Event can't be updated if it already started")
      (enforce (> ends-at (curr-time)) "Event can't be updated if it ended")
      (enforce-guard ADMIN-KS)
    )
    true
  )

  (defcap TOKEN_CREATION (event-id:string)
    @doc "Used to validate token creation"
    @event

    (util.guards1.guard-any [
      (enforce-guard ADMIN-KS)
      (create-user-guard (validate-event event-id))
    ])
  )

  (defcap CONNECT (event-id:string uri:string connection-guards:[guard])
    @doc "Used to guarante signature of all connecting parties"
    @event
    (util.guards1.enforce-guard-all connection-guards)
  )

  (defschema event
    name:string
    uri:string
    collection-id:string
    token-id:string
    starts-at:integer
    ends-at:integer
  )

  (deftable events:{event})

  (defun create-event (collection-id:string name:string uri:string starts-at:integer ends-at:integer)
    (let* ((event-id:string (create-event-id collection-id name))
        (creator-guard:guard (create-capability-guard (EVENT collection-id event-id name uri)))
        (policies [marmalade-v2.collection-policy-v1 proof-of-us-policy-v1])
        (token-id (create-token-id { 'uri: uri, 'precision: 0, 'policies: policies } creator-guard)) )
      
      (validate-event-time starts-at ends-at)

      (create-token
        token-id
        0 
        uri 
        policies
        creator-guard
      )

      (with-capability (EVENT collection-id event-id name uri)
        (insert events event-id {
          'name: name
          ,'uri: uri
          ,'collection-id: collection-id
          ,'token-id: token-id
          ,'starts-at: starts-at
          ,'ends-at: ends-at
        })
        true
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
      true
    )
  )

  (defun mint-attendance-token (event-id:string attendant:string attendant-guard:guard)
    (enforce (validate-principal attendant-guard attendant) "Incorrect account guard, only principal accounts allowed")

    (let* (
      (event:{event} (get-event event-id))
      (token-id:integer (at 'token-id event)) )

      (install-capability (MINT token-id attendant 1.0))
      (mint token-id attendant attendant-guard 1.0)
    )
  )

  (defun create-and-mint-connection-token (event-id:string uri:string connection-guards:[guard])
    (util.guards1.enforce-guard-all connection-guards)

    (validate-event event-id)

    (let* ((creator-guard:guard (util.guards1.guard-all connection-guards))
      (policies [marmalade-v2.collection-policy-v1 proof-of-us-policy-v1])
      (token-id (create-token-id { 'uri: uri, 'precision: 0, 'policies: policies } creator-guard)))

      (create-token
        token-id
        0
        uri 
        policies
        creator-guard
      )

      (map (lambda (connection-guard:guard)

        (install-capability (MINT token-id (create-principal connection-guard) 1.0))
        (mint token-id (create-principal connection-guard) connection-guard 1.0)

      ) connection-guards)

    )
  )

  ;;UTILITY FUNCTIONS

  (defun validate-event-time:bool (starts-at:integer ends-at:integer)
    (enforce (> ends-at (curr-time)) "Event end time must be in the future")
    (enforce (> starts-at (curr-time)) "Event start time must be in the future")
    (enforce (> ends-at starts-at) "Event must end after it starts")

    true
  )

  (defun validate-event:{event} (event-id:string)
    (let* (
      (event:{event} (get-event event-id))
      (starts-at:integer (at 'starts-at event))
      (ends-at:integer (at 'ends-at event)) )

      (enforce (and (>= (curr-time) starts-at) (<= (curr-time) ends-at)) "Minting is not allowed outside of event time")

      true
    )
  )

  (defun create-event-id:string (collection-id:string name:string)
    (format "proof-of-us:{}" [(hash [collection-id name])])
  )

  (defun get-event:object{event} (event-id:string)
    (read events event-id)
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [(create-table events)]
)

(enforce-guard ADMIN-KS)
