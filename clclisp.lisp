
;;(require :make "~/src/lisp/defsystem")
;;(mk:oos :clorb :compile) 
(load "clorb-pkgdcl")
(load "clorb-files")
(clorb::reload)
(clorb::load-ir)

(defvar *orb* (CORBA:ORB_init))
(setf clorb::*running-orb* t)
(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
