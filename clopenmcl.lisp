(in-package :cl-user)

(pushnew :use-idlcomp *features*)

(setf (logical-pathname-translations "CLORB")
      '(("SRC;**;*.*"  "home:src;clorb;**;*.*")
        ("**;*.*"      "CLORB:SRC;**;*.*" )))

(setf (logical-pathname-translations "PHOME")
      '(("**;*.*"  "home:**;*.*")))

(require 'acl-socket)
(load "CLORB:SRC;clorb-pkgdcl")
(load "CLORB:SRC;clorb-files")
(clorb:reload)

(setq clorb:*host* "localhost")
;;(setq clorb::*name-service* "corbaloc::1.2@localhost:2001/NameService")
(setq clorb::*name-service* "file:///tmp/NameService")
(setq clorb::*host-translations* '())


(defvar *orb* (CORBA:ORB_init))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))
(net.cddr.clorb.persistent-naming:setup-pns)

(setq
 *print-length* 7
 *print-depth* 3
 *print-array* nil
 )

(load "clorb:examples;hello;auto")

;; (load "clorb:examples;combat-account;account")
