
(in-package :cl-user)

(load "clorb-pkgdcl")

(import '(setup-pns hello-client setup-hello run-hello)
        :clorb)

(load "clorb-files")
(clorb:reload)
(clorb::load-ir)

(defvar *orb* (CORBA:ORB_init))
(setf clorb::*running-orb* t)

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listning on port ~A~%" (clorb::orb-port clorb::*the-orb*))

;;(op:object_to_string *orb* cl-user::root)

(defun hh ()
  (setup-hello :file "hello.ior")
  (hello-client :file "hello.ior"))

(defun hhn ()
  (cl-user::setup-hello :name "hello")
  (cl-user::hello-client :name "hello"))

(export '(hh hhn))
(import '(hh hhn) :clorb)
