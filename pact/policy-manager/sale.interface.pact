(namespace (read-msg 'ns))

(interface sale-v1
  (defun enforce-quote-update:bool (sale-id:string price:decimal)
    @doc "Read-only Function that is enforced to update quote price at enforce-buy"
  )
)
