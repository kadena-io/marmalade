(namespace (read-msg 'ns))

(module crafted-policy G
  (defcap G() true)
  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.abc)

  (defun enforce-init:bool (token:object{token-info}) true)

  (defun enforce-mint:bool (token:object{token-info} account:string guard:guard amount:decimal) true)

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (marmalade-v2.quote-manager.add-quote "attack-sale-id" "t:Pwo01UasZppPu1k7hmk7W16w4RK7C3qaVdkc1-T_ew8" (read-msg 'quote )))

  (defun enforce-offer:bool (token:object{token-info} seller:string amount:decimal sale-id:string) true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string guard:guard receiver:string amount:decimal) true )

  (defun enforce-withdraw:bool (token:object{token-info} seller:string amount:decimal sale-id:string) true)

  (defun enforce-buy:bool (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string) true)
)
