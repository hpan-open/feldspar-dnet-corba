(in-package :cl-user)

(pushnew :clorb-dev *features*)

(setq custom:*parse-namestring-ansi* t)

(setf (logical-pathname-translations "clorb")
  '(("SRC;**;*.*"  "/Users/lenst/src/clorb/**/*.*")
    ("IDL;**;*.*"  "/Users/lenst/src/clorb/idl/**/*.*")
    ("**;*.*"      "/Users/lenst/src/clorb/**/*.*" )))

(setf (logical-pathname-translations "PHOME")
      '(("**;*.*"  "home:**;*.*")))


(in-package "CLOS")
(setq *gf-warn-on-replacing-method*       nil)
(setq *warn-if-gf-already-called*         nil)
(in-package :cl-user)


(load "~/src/clorb/clorb-files")
(setq net.cddr.clorb.system::*binary-folder* "fasl")
(net.cddr.clorb.system:reload)


(setq clorb:*host* "localhost")
(setq clorb::*host-translations* nil)

(defvar *the-orb*
  (CORBA:ORB_init
   (list
    ;;"-ORBInitRef NameService=corbaloc::quad.local./NameService"
    "-ORBInitRef NameService=corbaloc::/NameService"
    )
   ""))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%"
        (clorb::orb-port clorb::*the-orb*))

(load (logical-pathname "clorb:examples;hello;auto.lisp"))
