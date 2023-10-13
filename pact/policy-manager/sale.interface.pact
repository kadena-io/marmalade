(namespace (read-msg 'ns))

(interface sale-v1
  (defun sale-guard:guard (sale-id:string price:decimal)
    @doc "Capability securing the modref call for enforce-init "
  )
)
