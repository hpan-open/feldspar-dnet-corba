(load "clorb:src;clorb-pkgdcl")
(load "clorb:src;clorb-files")

(pushnew :dummy-tcp *features*)

(clorb::reload)
(clorb::load-ir)

(defvar *orb* (CORBA:ORB_init))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listning on port ~A~%" (clorb::orb-port clorb::*the-orb*))

(defun hh ()
  (cl-user::setup-hello :file "hello.ior")
  (cl-user::hello-client :file "hello.ior"))

(defun hhn ()
  (cl-user::setup-hello :name "hello")
  (cl-user::hello-client :name "hello"))



(import '(cl-user::setup-pns
          cl-user::hello-client
          cl-user::setup-hello
          cl-user::run-hello)
        :clorb)





