
(in-package :cl-user)

(pushnew :clorb-dev *features*)

(setf (logical-pathname-translations "clorb")
  '(("src;**;*.*"  "~/src/clorb/**/*.*")
    ("**;*.*"      "~/src/clorb/**/*.*" )))

(load "clorb:src;clorb-files")
(net.cddr.clorb.system:reload)

(setq clorb:*host* "localhost")

(defvar *orb* 
    (CORBA:ORB_init
     (list "-ORBPort" "5111"
           "-ORBInitRef" "NameService=corbaloc::/NameService")))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listning on port ~A~%" (clorb::orb-port *orb*))

(defun run ()
  (op:run *orb*))
