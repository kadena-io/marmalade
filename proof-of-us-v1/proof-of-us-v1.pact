(namespace (read-msg 'ns))

(module proof-of-us-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
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

  (defschema event
    name:string
    uri:string
    collection-id:string
    starts-at:integer
    ends-at:integer
    operator-guard:guard
  )

  (deftable events:{event})

  (defun create-event (collection-id:string name:string uri:string starts-at:integer ends-at:integer operator-guard:guard)
    (let ((event-id:string (create-event-id collection-id operator-guard)) )
      (validate-event-time starts-at ends-at)

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
  
  (defun enforce-init:bool
    ( token:object{token-info}
    )
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    true
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string
    )
    (enforce false "Sale not allowed")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce false "Sale not allowed")
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (enforce false "Sale not allowed")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer not allowed")
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
