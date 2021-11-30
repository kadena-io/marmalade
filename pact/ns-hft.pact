(define-keyset 'hft-ns-user)
(define-keyset 'hft-ns-admin)
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'hft-ns-admin) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'hft-ns-user )
  (keyset-ref-guard 'hft-ns-admin )
)
