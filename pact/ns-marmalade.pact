(define-keyset 'marmalade-ns-user)
(define-keyset 'marmalade-ns-admin)
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'marmalade-ns-admin) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'marmalade-ns-user )
  (keyset-ref-guard 'marmalade-ns-admin )
)
