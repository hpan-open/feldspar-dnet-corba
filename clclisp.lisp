
(setq CLOS::*GF-WARN-ON-REPLACING-METHOD*       nil)
(setq CLOS::*WARN-IF-GF-ALREADY-CALLED*         nil)

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
(format t "~&;;; ORB listning on port ~A~%" (clorb::orb-port clorb::*the-orb*))

;;(op:object_to_string *orb* cl-user::root)

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





