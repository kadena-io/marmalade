(namespace (read-msg 'ns))

(module proof-of-us-gas-station GOVERNANCE

  (defconst ADMIN-KS:string "n_31cd1d224d06ca2b327f1b03f06763e305099250.pou-admin")

  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KS)
  )

  (implements gas-payer-v1)

  (use coin)

  (defun chain-gas-price ()
    (at 'gas-price (chain-data))
  )

  (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
    (enforce (<= (chain-gas-price) gasPrice)
      (format "Gas Price must be smaller than or equal to {}" [gasPrice]))
  )

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Can only be used inside an exec")
    (enforce (= 1 (length (at "exec-code" (read-msg)))) "Can only be used to call one pact function")
    (enforce
      (= "(n_31cd1d224d06ca2b327f1b03f06763e305099250.proof-of-us." (take 56 (at 0 (at "exec-code" (read-msg)))))
      "Only proof-of-us module calls are allowed"
    )
    (enforce-below-or-at-gas-price 0.000001)
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )

  (defconst GAS_STATION_ACCOUNT "proof-of-us-gas-station")

  (defun init ()
    (coin.create-account GAS_STATION_ACCOUNT (create-gas-payer-guard))
  )
)

(if (read-msg "init")
  [(init)]
  ["upgrade"]
)

(enforce-guard ADMIN-KS)
