
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

(defun run ()
  (handler-case
      (progn
        (clorb::close-connections)
        (op:list (clorb::get-ns) 100))
   (condition (c)
     (format t "~&Condition: ~A~%" c))))

(in-package :clorb)
(export 'clients)
(defun clients ()
  (client-streams-raw (adaptor *the-orb*)))

(import '(cl-user::setup-pns
          cl-user::hello-client
          cl-user::setup-hello
          cl-user::run-hello))

