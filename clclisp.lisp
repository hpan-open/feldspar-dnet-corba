(in-package :cl-user)

(pushnew :clorb-dev *features*)
(pushnew :use-my-idlparser *features*)
(pushnew :no-idlcomp *features*)



(setf (logical-pathname-translations "clorb")
  '(("SRC;**;*.*"  "/Users/lenst/src/clorb/**/*.*")
    ("IDL;**;*.*"  "/Users/lenst/src/clorb/idl/**/*.*")
    ("**;*.*"      "/Users/lenst/src/clorb/**/*.*" )))

(setf (logical-pathname-translations "PHOME")
      '(("**;*.*"  "home:**;*.*")))


;;(setq CLOS::*GF-WARN-ON-REPLACING-METHOD*       nil)
;;(setq CLOS::*WARN-IF-GF-ALREADY-CALLED*         nil)

#+use-my-idlparser
(packer:require-package :net.cddr.redpas)

(load "clorb-files")
(net.cddr.clorb.system:reload)


(setq clorb:*host* "localhost")
(setq clorb::*host-translations* nil)

(defvar *orb*
  (CORBA:ORB_init
   (list
    ;;"-ORBInitRef NameService=corbaloc::quad.local./NameService"
    "-ORBInitRef NameService=corbaloc::/NameService"
    )
   ""))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))

(load (translate-logical-pathname "clorb:examples;hello;auto.lisp"))
