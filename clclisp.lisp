(in-package :cl-user)

(setq CLOS::*GF-WARN-ON-REPLACING-METHOD*       nil)
(setq CLOS::*WARN-IF-GF-ALREADY-CALLED*         nil)

;;(require :make "~/src/lisp/defsystem")
;;(mk:oos :clorb :compile) 
(load "clorb-pkgdcl")
(load "clorb-files")
(clorb:reload)
(clorb:load-ir)

(setq clorb:*host* "localhost")
(setq clorb::*name-service* "corbaloc::1.2@localhost:2001/NameService")

(defvar *orb* (CORBA:ORB_init))
(setf clorb::*running-orb* t)

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))

(defun hh ()
  (setup-hello :file "hello.ior")
  (hello-client :file "hello.ior"))

(defun hhn ()
  (setup-hello :name "hello")
  (hello-client :name "hello"))



(import '(cl-user::setup-pns
          cl-user::hello-client
          cl-user::setup-hello
          cl-user::run-hello)
        :clorb)


(defparameter *openorb-ns-ior*
  "corbaloc::1.2@localhost:2001/NameService")

(defvar *openorb-ns*
  (op:string_to_object *orb* *openorb-ns-ior*))

;; (clorb::locate *openorb-ns*)
;; (clorb::object-narrow *openorb-ns* +naming-context-id+)
