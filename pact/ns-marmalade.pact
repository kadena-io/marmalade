(define-keyset "marmalade-admin")
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'marmalade-admin) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'marmalade-admin )
  (keyset-ref-guard 'marmalade-admin )
)
(namespace (read-msg 'ns))
(define-keyset  (+ (read-msg 'ns) ".marmalade-admin"))
(enforce-keyset (+ (read-msg 'ns) ".marmalade-admin"))
(enforce-keyset 'marmalade-admin)
