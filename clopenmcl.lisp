(in-package :cl-user)

(pushnew :clorb-dev *features*)


(let ((clorb-home #p"home:src;clorb;"))
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.*"  ,(merge-pathnames ";**;*.*" clorb-home))
          ("**;*.*"      ,(merge-pathnames ";**;*.*" clorb-home)) )))


(load "CLORB:SRC;clorb-files")
(setq net.cddr.clorb.system::*binary-folder* "fasl")
(net.cddr.clorb.system:reload)

(setq clorb:*host* "localhost")
(setq clorb::*host-translations* '())

(defvar *the-orb*
  (CORBA:ORB_init (list "-ORBInitRef NameService=corbaloc::/NameService")))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))
(setq persistent-naming:*naming-ior-file* "/tmp/openmcl-naming.ior")
(persistent-naming:setup-pns :export t)

(setq
 *print-length* 10
 *print-depth* 4
 *print-array* nil
 )

(load "clorb:examples;hello;auto")

;; (load "clorb:examples;combat-account;account")
