(in-package :clorb)

(idef-definitions
 (define-module "Locator" ()
   (define-type "POA_name" (sequence string))
   (define-type "ObjectID" (sequence octet))
   (define-interface "Factory" ()
     (define-operation "create_reference"
         ((:in "poa" "POA_name")
          (:in "oid" "ObjectID")
          (:in "intf" string))
       :result-type object))
   (define-interface "Locator" ()
     (define-operation "register" 
         ((:in "server_name" string)
          (:in "server" "Factory"))
       :result-type "Factory"))))

