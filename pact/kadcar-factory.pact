(namespace (read-msg 'ns))

(module kadcar-factory GOVERNANCE

(use kip.token-manifest)


;;;  token id
;;;  MAKE : MODEL : VIN_NUMBER
;;;
;;;


;;;;;;;;;;;;;;  CONTST ;;;;;;;;;;;;;;

(defconst URL "https://bafybeictqzkgpzfwwdno3upe3lrm5z5euo2oe6xohmup2ckoy3qk7xi2zi.ipfs.infura-ipfs.io/?filename=b-1.png")

;;;;;;;;;;;;;; SCHEMAS ;;;;;;;;;;;;;;

  (defschema mutable-state-schema
    @doc "regular nft stats indexed by IDs"
    components:[object:{component}]
  )

  (defschema vehicle-information
    @doc "vehicle identification certificate"
    vin:string
    make:string
    model:string
    )
  (defschema immutable-state-schema
      @doc "immutable-state data such as id and creation information"
      vehicle-information:object{vehicle-information}
      mint-time:time
  )

(defschema component
        name:string
        schema-uri:object{mf-uri}
        stats:[object:{keyval}]
)

(defschema keyval
      @doc "Open data model to allow for any typed stat"
      key:string
      val:string
)

(defschema view-references
        @doc "stores references to NFT 3d files"
        genesis-3d-asset:object{mf-uri}
        final-3d-asset:object{mf-uri}
)

;;;;;;;;;;;;;; Capabilities ;;;;;;;;;;;;;;

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin)))


;;;;;;;;;;;;;;;;;;;;;;;; MARM WRAPPERS ;;;;;;;;;;;;;;;;;;;;;;;;



;;vin:string make:string model:string

  (defun create-k2 (vin-number:string)

    (let*
      (
        (vehicleSpec:object{vehicle-information} (read-msg "vehicle_spec"))
        (make:string (at 'make vehicleSpec))
        (model:string (at 'model vehicleSpec))
        (vin:string (at 'vin vehicleSpec))
        (token-id (get-kadcar-token-id vin make model))
      )
      (marmalade.ledger.create-token token-id 1
      (get-k2-manifest vin-number) marmalade.fixed-quote-royalty-policy)
      (format "created token {}" [token-id])
    )

  )


  (defun mint-k2 (token-id:string account:string account-guard:guard)
    (marmalade.ledger.mint token-id account account-guard 1.0)
  )


    ;;;;;;;;;;;;;; main entry to retrieve k1 manifest ;;;;;;;;;;;;;;

  (defun get-k2-manifest(vin-number:string)

    (let*
      (
        (vehicleSpec:object{vehicle-information} (read-msg "vehicle_spec"))
        (mut:object{mutable-state-schema} (get-mutable-state))
        (immut:object{immutable-state-schema} (get-immutable-state vehicleSpec))
        (view-ref:object{view-references} (get-view-refs))

        (mutable (create-datum (uri "pact:schema" "mutable-state-data") mut))
        (immutable (create-datum (uri "pact:schema" "immutable-state-data") immut))
        (view (create-datum (uri "pact:schema" "view-references") view-ref))
      )
      (create-manifest (uri "ipfs" URL) [mutable immutable view])
    )
  )


;; FOR TESTING ONLY
  (defun build-k2-manifest-with-id(id:string)

    (let*

      (
        (mut:object{mutable-state-schema} (get-mutable-state))
        (immut:object{immutable-state-schema} (get-immutable-state {
          'vin:"any",
          'make: "any",
          'model:"any"
        }))
        (view-ref:object{view-references} (get-view-refs))

        (mutable (create-datum (uri "pact:schema" "mutable-state-data") mut))
        (immutable (create-datum (uri "pact:schema" "immutable-state-data") immut))
        (view (create-datum (uri "pact:schema" "view-references") view-ref))
      )
      (create-manifest (uri "ipfs" URL) [mutable immutable view])
    )
  )


  ;;;;;;;;;;;;;; top-level state getters ;;;;;;;;;;;;;;

  (defun get-mutable-state:object{mutable-state-schema} ()
      {'components : [(get-body) (get-engine)]}

  )
  (defun get-immutable-state:object{immutable-state-schema} (
      vehicle-info:object{vehicle-information}
    )
      {
        "vehicle-information":
        {
          'vin:(at 'vin vehicle-info),
          'make: (at 'make vehicle-info),
          'model:(at 'model vehicle-info)
        }
        ,'mint-time: (at "block-time" (chain-data))
      }

  )

  (defun get-view-refs:object{view-references} ()
    {
      'genesis-3d-asset: (uri "ipfs" "https://bafybeiblo3baxrxskpsxbgjp2nb7em2img666jo6ssyv4zjbw7ae7lu6q4.ipfs.infura-ipfs.io/?filename=new_embedded.gltf"),
      'final-3d-asset: (uri "pact:schema" "https://bafybeiblo3baxrxskpsxbgjp2nb7em2img666jo6ssyv4zjbw7ae7lu6q4.ipfs.infura-ipfs.io/?filename=new_embedded.gltf")
    }
  )

  ;;;;;;;;;;;;;; Component builders ;;;;;;;;;;;;;;

  (defun get-body:object{component} ()
    {
      'name:"body",
      'schema-uri: (uri "pact:schema" "component"),
      'stats:[{'key:"aerodynamic-factor", 'val:"0.24"} {'key:"weight", 'val:"230"}]
    }
  )

  (defun get-engine:object{component} ()
    {
      'name:"engine",
      'schema-uri: (uri "pact:schema" "component"),
      'stats:[{'top-speed:"", 'val:"145"} {'key:"torque", 'val:"124"} {'key:"horse-power", 'val:"320"}]
    }
  )

  (defun get-kadcar-token-id (vin:string make:string model:string)
    (format "{}#{}:{}" [make model (time-stamp vin)])
  )

  (defun time-stamp:string (any-data)
    (hash (+ (hash (at 'block-time (chain-data))) (hash any-data)))
  )


)
