(in-package :cl-user)

(pushnew :use-my-idlparser *features*)
(net.cddr.packer:require-package :net.cddr.redpas)

(load "clorb-pkgdcl")
(load "clorb-files")
(clorb:reload)

(setq clorb:*port* 5711)
(setq clorb:*host* "localhost")

(defvar *orb* (CORBA:ORB_init))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))

(defun hh ()
  (setup-hello :file "hello.ior")
  (hello-client :file "hello.ior"))
