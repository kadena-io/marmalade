# multi-policy tutorial
multi-policy is a policy that  defines a multi-policy token implementation. The contract manages a policy-table table and uses the kip.token-policy-v1 module for policy implementation.

#The contract defines several functions, including:

create-multi-policy: 
a function that creates a new token with an associated policies.

add-policy: 
a function that adds a new policy to an existing token.

remove-policy: 
a function that removes a policy from an existing token.

get-policies:
a function that retrieves the policies associated with a given token.

enforce-init:
a function that enforces a one-off policy required for token initialization.

enforce-mint:
a function that enforces policies for minting new tokens.

enforce-burn:
a function that enforces policies for burning tokens.

enforce-offer:
a function that enforces policies for making an offer to sell tokens.

enforce-buy:
a function that enforces policies for buying tokens.

The contract also defines a policy-list schema and a policies schema.