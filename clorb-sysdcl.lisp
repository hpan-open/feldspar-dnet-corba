(in-package :user)

(defsystem :clorb (:default-package :clorb
                      #|:default-pathname "devl:"|#)
  
  (:definitions (:serial "clorb-pkgdcl" "clorb-macros") 
      (:serial base 
               server
               x-files))

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
                  "clorb-util"
                  "dumpir"))
  
  (:module-group server
                 (:serial
                  ;; server 
                  "clorb-objkey"
                  "clorb-srvreq"
                  "clorb-servant"
                  "clorb-trie"
                  "clorb-poamgr"
                  "clorb-poa"
                  "clorb-srv"))
  
  (:module-group x-files
                 (:serial 
                  "test-suite"
                  "orb-export"
                  "orb-structs"
                  "local-ir"
                  "idef-read"
                  "idef-write"
                  "idef-macros"
                  ;; Services
                  "cosnaming-idl"
                  "cosnaming-stub"
                  "cosnaming-skel"
                  "pns-server"
                  "ec-server"
                  ;; Example
                  "hello-idl"
                  "hello-client" 
                  "hello-server"))
  
  (:module-group examples
                 (:serial
                  "hello-dynserver"
                  "dsi-example" 
                  "dii-example")))
