(in-package :cl-user)

(setf (logical-pathname-translations "clorb")
  '(("SRC;*.*"  "/Users/lenst/src/clorb/*.*")
    ("*.*"      "CLORB:SRC;*.*" )))

(setf (logical-pathname-translations "PHOME")
      '(("**;*.*"  "home:**;*.*")))


(setq CLOS::*GF-WARN-ON-REPLACING-METHOD*       nil)
(setq CLOS::*WARN-IF-GF-ALREADY-CALLED*         nil)

(load "clorb-pkgdcl")
(load "clorb-files")
(clorb:reload)
(clorb:load-ir)

(setq clorb:*host* "localhost")
;;(setq clorb::*name-service* "corbaloc::1.2@localhost:2001/NameService")
(setq clorb::*name-service* "file:///tmp/NameService")
(setq clorb::*host-translations* nil)

(defvar *orb* (CORBA:ORB_init))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))
