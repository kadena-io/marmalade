(namespace (read-msg 'ns))

(module proof-of-us-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (use marmalade-v2.util-v1)

  (defcap EVENT (collection-id:string event-id:string name:string uri:string operator-guard:guard)
    @doc "Used for new event discovery"
    @event
    (enforce-guard operator-guard)
    true
  )

  (defcap EVENT_UPDATE (event-id:string)
    @doc "Used for updated event discovery"
    @event
    (with-read events event-id {
      'starts-at := starts-at
      ,'ends-at := ends-at
      ,'operator-guard := operator-guard
    }
      (validate-event-time starts-at ends-at)
      (enforce (> starts-at (curr-time)) "Event can't be updated if it already started")
      (enforce (> ends-at (curr-time)) "Event can't be updated if it ended")
      (enforce-guard operator-guard)
    )
    true
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
    operator-guard:guard
  )

  (deftable events:{event})

  (defun create-event (collection-id:string name:string uri:string starts-at:integer ends-at:integer operator-guard:guard)
    (let ((event-id:string (create-event-id collection-id operator-guard)) )
      (validate-event-time starts-at ends-at)

      ;  TODO: create token and store token-id

      (with-capability (EVENT collection-id event-id name uri operator-guard)
        (insert events event-id {
          'name: name
          ,'uri: uri
          ,'collection-id: collection-id
          ,'starts-at: starts-at
          ,'ends-at: ends-at
          ,'operator-guard: operator-guard
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

  (defun attendance-guard:guard (event-id:string)
    (create-user-guard (attendance-mint-guard event-id))
  )

  (defun attendance-mint-guard:guard (event-id:string)
    (with-read events event-id {
      'starts-at: starts-at
      ,'ends-at: ends-at
    }
      (validate-event-time starts-at ends-at)
    )
  )

  (defun mint-attendance-token (event-id:string attendant:string attendant-guard:guard)
  ; validate principal account
  ; read token from event table
  ; mint token to attendant (taking into account the mint-guard)
  )

  (defun create-and-mint-connection-token (event-id:string uri:string connection-guards:[guard])
    ; Capability that needs to be signed by all connection-guards, util.guards1.enforce-guard-all
    ; set supply to number of guards
    ; mint token to principal accounts that belong to the guards
  )


  ;;UTILITY FUNCTIONS

  (defun validate-event-time:bool (starts-at:integer ends-at:integer)
    (enforce (> ends-at (curr-time)) "Event end time must be in the future")
    (enforce (> starts-at (curr-time)) "Event start time must be in the future")
    (enforce (> ends-at starts-at) "Event must end after it starts")

    true
  )

  (defun create-event-id:string (event-name:string operator-guard:guard)
    (format "proof-of-us:{}" [(hash [event-name operator-guard])])
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
