(in-package :cl-user)

(format t "~&;;;; Loading CLORB~%")

(pushnew :clorb-dev *features*)

(require :sb-bsd-sockets)

(let ((clorb-home (merge-pathnames "src/clorb/" (user-homedir-pathname))))
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.*"    ,(merge-pathnames "**/*.*" clorb-home))
          ("**;*.*"        ,(merge-pathnames "**/*.*" clorb-home)) )))

(load "clorb:src;clorb-files")
(setq net.cddr.clorb.system::*binary-folder* "fasl")
(net.cddr.clorb.system:reload)

(load "clorb:src;examples;hello;auto")

(defvar *the-orb* 
  (CORBA:ORB_init
   (list ;;"-ORBPort" "4712"
         "-ORBInitRef" "NameService=corbaloc::/NameService"
         #| "-ORBInitRef" "InterfaceRepository=" |#)))
(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port *the-orb*))


;; Local variables:
;; inferior-lisp-program: "sbcl"
;; End:
