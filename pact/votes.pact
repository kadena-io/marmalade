(module govtest COUNT_VOTES
  "Demonstrate programmable governance showing votes for upgrade transaction hashes"
  (use user.hft)

  (defschema vote
    vote-hash:string)

  (deftable votes:{vote})

  (defcap COUNT_VOTES ()
    ;;todo
    true)

  (defun token-guard ()
    (require-capability (COUNT_VOTES))
  )

  (defun create-token-guard:guard ()
    (create-user-guard (token-guard))
  )

  (defun transfer (
    token:string
    sender:string
    receiver:string
    amount:decimal
   )
    (with-capability (COUNT_VOTES)
      (transfer token sender receiver amount))
  )

  (defun transfer-create (
    token:string
    sender:string
    receiver:string
    receiver-g:guard
    amount:decimal
   )
    (with-capability (COUNT_VOTES)
      (transfer-create token sender receiver receiver-g amount))
  )

  (defun vote-for (user)
    ;;todo
    true
    )

  (defun do-count (hsh tally u)
    ;;todo
    true
    )

)
(create-table votes)
