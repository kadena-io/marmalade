(namespace (read-msg 'ns))

(module proof-of-us-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.util-v1)

  (defcap EVENT (event-id:string name:string operator-guard:guard)
    @doc "Used for new event discovery"
    @event
    (enforce-guard operator-guard)
    true
  )

  (defcap UPDATE_EVENT (event-id:string)
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
    starts-at:integer
    ends-at:integer
    operator-guard:guard
  )

  (deftable events:{event})

  (defconst EVENT-NAME-MSG-KEY:string "name")
  (defconst EVENT-URI-MSG-KEY:string "uri")
  (defconst EVENT-STARTS-AT-MSG-KEY:string "starts_at")
  (defconst EVENT-ENDS-AT-MSG-KEY:string "ends_at")
  (defconst EVENT-OPERATOR-GUARD-MSG-KEY:string "operator_guard")

  (defun create-event ()
    (let* (
      (name:string (read-msg EVENT-NAME-MSG-KEY))
      (uri:string (read-msg EVENT-URI-MSG-KEY))
      (starts-at:integer (read-integer EVENT-STARTS-AT-MSG-KEY))
      (ends-at:integer (read-integer EVENT-ENDS-AT-MSG-KEY))
      (operator-guard:guard (read-keyset EVENT-OPERATOR-GUARD-MSG-KEY))
      (event-id:string (create-event-id name operator-guard)) )
    
      (validate-event-time starts-at ends-at)

      (with-capability (EVENT event-id name operator-guard)
        (insert events event-id {
          'name: name
          ,'uri: uri
          ,'starts-at: starts-at
          ,'ends-at: ends-at
          ,'operator-guard: operator-guard
        }) 
        true
      )
    )
  )

  (defun update-event (event-id:string)
    (let* (
      (uri:string (read-msg EVENT-URI-MSG-KEY))
      (starts-at:integer (read-integer EVENT-STARTS-AT-MSG-KEY))
      (ends-at:integer (read-integer EVENT-ENDS-AT-MSG-KEY)) )
      
      (with-capability (UPDATE_EVENT event-id)
        (update events event-id {
          'uri: uri
          ,'starts-at: starts-at
          ,'ends-at: ends-at
        })
        true
      )
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
