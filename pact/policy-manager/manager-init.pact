(use marmalade-v2.policy-manager [NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY])

(marmalade-v2.policy-manager.init marmalade-v2.ledger)
(marmalade-v2.quote-manager.init marmalade-v2.policy-manager)
(marmalade-v2.policy-manager.write-concrete-policy NON_FUNGIBLE_POLICY marmalade-v2.non-fungible-policy-v1)
(marmalade-v2.policy-manager.write-concrete-policy ROYALTY_POLICY marmalade-v2.royalty-policy-v1)
(marmalade-v2.policy-manager.write-concrete-policy COLLECTION_POLICY marmalade-v2.collection-policy-v1)
(marmalade-v2.policy-manager.write-concrete-policy GUARD_POLICY marmalade-v2.guard-policy-v1)
