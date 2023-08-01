(let ((admin-ks (read-keyset 'marmalade-admin)))
  (ns.write-registry (read-msg 'ns) admin-ks true)
  (define-namespace
    (read-msg 'ns)
    admin-ks
    admin-ks)

  (namespace (read-msg 'ns))
  (define-keyset (+ (read-msg 'ns) ".marmalade-admin")
                 admin-ks))
