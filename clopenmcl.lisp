(in-package :cl-user)

(pushnew :clorb-dev *features*)
(pushnew :use-my-idlparser *features*)
(pushnew :no-idlcomp *features*)


(let ((clorb-home #p"home:src;clorb;")
      (clorb-fasl #p"home:src;clorb;fasl;"))
  (ensure-directories-exist clorb-fasl)
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.dfsl" ,(merge-pathnames ";**;*.*" clorb-fasl))
          ("SRC;**;*.*"  ,(merge-pathnames ";**;*.*" clorb-home))
          ("**;*.*"      "CLORB:SRC;**;*.*" ))))


(setf (logical-pathname-translations "PHOME")
      '(("**;*.*"  "home:**;*.*")))

(require 'acl-socket)
#+use-my-idlparser (packer:require-package :net.cddr.redpas)

(load "CLORB:SRC;clorb-files")
(net.cddr.clorb.system:reload)

(setq clorb:*host* "localhost")
;;(setq clorb::*name-service* "corbaloc::1.2@localhost:2001/NameService")
;;(setq clorb::*name-service* "file:///tmp/NameService")
(setq clorb::*name-service* "corbaloc::localhost:4711/NameService")

(setq clorb::*host-translations* '())


(defvar *the-orb* (CORBA:ORB_init))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))
(setq persistent-naming:*naming-ior-file* "/tmp/openmcl-naming.ior")
(persistent-naming:setup-pns :export t)

(setq
 *print-length* 7
 *print-depth* 3
 *print-array* nil
 )

(load "clorb:examples;hello;auto")

;; (load "clorb:examples;combat-account;account")
