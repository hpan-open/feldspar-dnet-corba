(in-package :user)

(defsystem :clorb (:default-package :clorb
                      #|:default-pathname "devl:"|#)
  
  (:definitions (:serial "clorb-pkgdcl" "clorb-macros") 
      (:serial base services))

  (:module-group base
                 (:serial
                  "clorb-options"
                  "clorb-supp"
                  "socket"
                  "clorb-basetypes"
                  "clorb-exceptions"
                  "clorb-object"
                  "clorb-typecode"
                  "clorb-any"
                  "clorb-struct"
                  "clorb-union"
                  "clorb-buffer"
                  (:parallel
                   "clorb-marshal"
                   "clorb-unmarshal"
                   "clorb-opdef"
                   "clorb-client")
                  "clorb-orb"
                  "clorb-request"
                  "clorb-iir"
                  "clorb-objkey"
                  "clorb-srvreq"
                  "clorb-servant"
                  "clorb-trie"
                  "clorb-poamgr"
                  "clorb-poa"
                  "clorb-srv"
                  "clorb-util"
                  "local-ir"
                  "idef-read"
                  "idef-write"
                  "idef-macros"
                  "test-suite"))
  
  (:module-group services
                 (:serial 
                  "ns-server" 
                  "ec-server"
                  ;; "omniwrapper"
                  "dumpir"))
  
  (:module-group examples
                 (:serial
                  "hello-idl" "hello-client" "hello-server" "hello-dynserver"
                  "dsi-example" "dii-example"))
  
)
