
(in-package :cl-user)

(pushnew :no-idlcomp *features*)
(pushnew :clorb-dev *features*)
(pushnew :use-my-idlparser *features*)
(net.cddr.packer:require-package :net.cddr.redpas)

(setf (logical-pathname-translations "clorb")
  '(("src;**;*.fasl"  "~/src/clorb/fasl/**/*.*")
    ("src;**;*.*"  "~/src/clorb/**/*.*")
    ("**;*.*"      "~/src/clorb/**/*.*" )))

(setf (logical-pathname-translations "phome")
      '(("**;*.*"  "~/**/*.*")))


(load "clorb:src;clorb-files")
(net.cddr.clorb.system:reload)

(setq clorb:*host* "localhost")
(setq clorb:*port* 5111)

(defvar *orb* (CORBA:ORB_init))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listning on port ~A~%" (clorb::orb-port *orb*))

(defun run ()
  (op:run *orb*))

;;(op:object_to_string *orb* cl-user::root)


(defun describe-repo (r)
  (let ((l (coerce (op:contents r :dk_all t) 'list))
        (k (op:def_kind r)))
    (pprint-logical-block (*standard-output* l :per-line-prefix "| ")
      (loop for x in l
          for f = nil then t
          do (when f (pprint-newline (if (eq k :dk_interface)
                                         :fill :mandatory)))
             (format *standard-output*
                     "~A ~S  "
                      (op:name x) (op:def_kind x))
             (when (typep x 'clorb::container)
               (pprint-newline :mandatory)
               (describe-repo x))))))

(setf (tpl:alias "rep") (lambda ()
                          (describe-repo clorb::*idef-repository*)))
