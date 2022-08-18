(interface collectionByIndex

  (defcap CREATE_COLLECTION (collection-id:string))

  (defcap CREATE_TOKEN (collection-id:string token-id:string))

  (defcap COLLECTION (collection-id:string total-unique-tokens:integer)
    @event)

  (defcap TOKEN (token-id:string collection-id:string supply:decimal)
    @event)

  (defschema account
    account:string
    guard:guard
    tokens:[string]
  )

  (defschema token
    id:string
    collection-id:string
  )

  (defschema collection
    id:string
    tokens:[string]
  )

  (defschema account
    total-tokens-owned:integer)

  (defun key:string (collection-id:string index:integer) )

  (defun init-collection:bool (id:string))

  (defun get-collection-token-count:integer (collection-id:string))
  (defun get-account-token-count:integer (account-id:string))
  (defun get-collection-token (index:integer))
  (defun get-account-token (index:integer))
)
