(namespace (read-msg 'ns))

(module creator-royalty-policy GOVERNANCE

  @doc "Policy for issuance of royalty in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema policy-schema
    fungible:module{fungible-v2}
    creator:string ;; with k: you can transfer to k:creator account
    creator-guard:guard
    royalty-rate:decimal
  )

  (deftable policies:{policy-schema})

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal )
     true
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal )
    true
  )

  (defun enforce-init:bool
    (
      token:object{token-info}
    )
    (enforce-ledger)
    (let* (
      (spec:object{policy-schema} (read-msg TOKEN_SPEC))
      (fungible:module{fungible-v2} (at 'fungible spec))
      (creator:string (at 'creator spec))
      (creator-guard:guard (at 'creator-guard spec))
      (royalty-rate:decimal (at 'royalty-rate spec))
      (creator-details:object (fungible::details creator )) )

      (enforce (=
        (take 2 creator) "k:") "Only k:accounts are supported")
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")

      (insert policies (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'royalty-rate: royalty-rate }))
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    @doc "Capture spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    ;;enforce fq-p.enforce-offer is already run = sale-id on line 92 wouldnt be available of not run
    (bind (get-policy token)
      {
        'fungible := fungible:module{fungible-v2}
      }
      (let* (
          (quote:object{fixed-quote-policy.quote-schema} (fixed-quote-policy.get-quote sale-id))
          (spec:object{fixed-quote-policy.quote-spec} (at 'spec quote)))

        (enforce (= (at 'fungible spec) fungible) "Mismatching creator requested fungible") )
    )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      {
        'fungible := fungible:module{fungible-v2},
        'creator:= creator:string,
        'royalty-rate:= royalty-rate:decimal
      }
      (let* (
        (quote:object{fixed-quote-policy.quote-schema} (fixed-quote-policy.get-quote sale-id))
        (spec:object{fixed-quote-policy.quote-spec} (at 'spec quote))
        (qtoken:string (at 'id quote)) )
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          {
            'price := price:decimal
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal (floor (* sale-price royalty-rate) (fungible::precision))) )
            (if
              (> royalty-payout 0.0)
              (fungible::transfer buyer creator royalty-payout)) ;; extra payment + sale-price (this is fundementally wrong :) see google-docs)
              ;; should be subtracted from the sale-price from previous policie buys
            true
        ))
  )))

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table policies) ])
