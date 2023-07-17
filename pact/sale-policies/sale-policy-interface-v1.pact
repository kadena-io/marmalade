(namespace (read-msg 'ns))

(interface sale-policy-interface-v1

    @doc "Interface for sale-policies"

    (defschema token-info
        id:string
        supply:decimal
        precision:integer
        uri:string
        policies:[module{token-policy-v2}])


    (defun enforce-offer:bool
        ( token:object{token-info}
            seller:string
            amount:decimal
            sale-id:string )
        @doc "Offer policy of sale SALE-ID by SELLER of AMOUNT of TOKEN."
    )
    
    (defun enforce-buy:bool
        ( token:object{token-info}
            seller:string
            buyer:string
            buyer-guard:guard
            amount:decimal
            sale-id:string )
        @doc "Buy policy on SALE-ID by SELLER to BUYER AMOUNT of TOKEN."
    )
    
    (defun enforce-withdraw:bool
        ( token:object{token-info}
            seller:string
            amount:decimal
            sale-id:string )
        @doc "Withdraw policy on SALE-ID by SELLER of AMOUNT of TOKEN"
    )
)
