(in-package :clorb)

(idef-definitions
 (define-module "Hello" ()
   (define-interface "World" ()
     (define-operation "greet" ()
       :result-type string))))

