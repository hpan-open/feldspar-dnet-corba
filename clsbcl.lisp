(in-package :cl-user)

;;(require :db-sockets)
(require :asdf)
(require :sb-bsd-sockets)

(pushnew :clorb-dev *features*)

(let ((clorb-home #p"home:src;clorb;")
      (clorb-fasl #p"home:src;clorb;fasl;"))
  (ensure-directories-exist clorb-fasl)
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.fasl" ,(merge-pathnames ";**;*.*" clorb-fasl))
          ("SRC;**;*.*"  ,(merge-pathnames ";**;*.*" clorb-home))
          ("**;*.*"      "CLORB:SRC;**;*.*" ))))

(load "clorb:src;clorb-files")
(net.cddr.clorb.system:reload)

;;(clorb::load-ir)

(load "clorb:src;examples;hello;auto")

(defvar *orb* (CORBA:ORB_init))
(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))


;; Local variables:
;; inferior-lisp-program: "sbcl"
;; End:
