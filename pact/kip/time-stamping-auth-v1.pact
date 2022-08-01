(namespace 'kip)

(module time-stamping-auth-v1 GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (defschema mf-uri
    scheme:string
    data:string
  )

  (defschema mf-datum
    uri:object{mf-uri}
    hash:string
    datum:object
  )

  (defschema manifest
    uri:object{mf-uri}
    hash:string
    data:[object{mf-datum}]
  )

  (defschema token-certificates
    certificate-ids:[object{time-stamped-certificate}]
  )

  (defschema time-stamped-certificate
      token-ids:object{mf-uri}
      cx-minus-one-h:string
      mxh:string
      dxh:string
      time-stamp:time
  )


  ;         d1 t1 -> c1 = (h(d1+t1))
  ;       d2 t2 -> c2 (h(h(d2+t2) + h(m1) + c1))
  ;       d3 t3 -> c3 (h(h(d3+t3) + h(m2) + c2))
  ;       d4 t4 -> c4 (h(h(d4+t4) + h(m3) + c3))

  ; to verify d3 existed at t3. hash d3 + t3 and then combine with hashes from above
  ;
  ;



;;;;;;;;ENTRY;;;;;;
  (defun broadcast-change (token-id:string manifest:object{manifest} block-time:time)

    ;;create certificate for id
    ;;add certificate to id

  )
  (defun create-manifest:object{manifest}
    ( uri:object{mf-uri}
      data:[object{mf-datum}]
    )
    { 'uri: uri
    , 'hash: (hash-contents uri (map (at 'hash ) data))
    , 'data: data
    }
  )

  (defun create-datum:object{mf-datum}
    ( uri:object{mf-uri}
      datum:object
    )
    { 'uri: uri
    , 'hash: (hash-contents uri [(hash datum)])
    , 'datum: datum
    }
  )

  (defun verify-manifest:bool
    ( manifest:object{manifest}
    )
    (bind manifest
      { "uri":= uri
      , "data":= data
      }
      (= (create-manifest uri data) manifest)
    )
  )

  (defun enforce-verify-manifest:bool
    ( manifest:object{manifest}
    )
    (enforce
      (verify-manifest manifest)
      "Manifest is not valid")
  )

  (defun uri (scheme:string data:string)
    {'scheme: scheme, 'data: data }
  )

)
