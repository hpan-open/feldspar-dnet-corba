
(in-package :cl-user)

(load "clorb-pkgdcl")

(import '(setup-pns hello-client setup-hello run-hello run)
        :clorb)

(load "clorb-files")
(clorb:reload)
(clorb::load-ir)

(setq clorb:*host* "10.0.1.251")
(setq clorb:*port* 5111)

(defvar *orb* (CORBA:ORB_init))
(setf clorb::*running-orb* t)

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listning on port ~A~%" (clorb::orb-port clorb::*the-orb*))

(defun run ()
  (op:run *orb*))

;;(op:object_to_string *orb* cl-user::root)

(defun hh ()
  (setup-hello :file "hello.ior")
  (hello-client :file "hello.ior"))

(defun hhn ()
  (cl-user::setup-hello :name "hello")
  (cl-user::hello-client :name "hello"))

(export '(hh hhn))
(import '(hh hhn) :clorb)

(defun describe-repo (r)
  (let ((l (coerce (op:contents r :dk_all t) 'list))
        (k (op:def_kind r)))
    (pprint-logical-block (*standard-output* l :per-line-prefix "| ")
      (loop for x in l
          for f = nil then t
          do (when f (pprint-newline (if (eq k :dk_Interface)
                                         :fill :mandatory)))
             (format *standard-output*
                     "~A ~S  "
                      (op:name x) (op:def_kind x))
             (when (typep x 'clorb::container)
               (pprint-newline :mandatory)
               (describe-repo x))))))

(setf (tpl:alias "rep") (lambda ()
                          (describe-repo clorb::*idef-repository*)))
