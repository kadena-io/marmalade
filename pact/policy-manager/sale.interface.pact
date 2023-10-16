(namespace (read-msg 'ns))

(interface sale-v1
  (defun sale-guard:guard (sale-id:string price:decimal)
    @doc "Guard that is enforced to update quote price at enforce-buy"
  )
)
