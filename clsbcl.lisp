(in-package :cl-user)

(format t "~&;;;; Loading CLORB~%")

(pushnew :use-my-idlparser *features*)
(pushnew :no-idlcomp *features*)
(pushnew :clorb-dev *features*)


;;(require :db-sockets)
(require :asdf)
(require :sb-bsd-sockets)

#+use-my-idlparser
(packer:require-package "NET.CDDR.REDPAS")

(let* ((clorb-home (merge-pathnames "src/clorb/" (user-homedir-pathname)))
       (clorb-fasl (merge-pathnames "fasl/" clorb-home)))
  (ensure-directories-exist clorb-fasl)
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.fasl" ,(merge-pathnames "**/*.fasl" clorb-fasl))
          ("SRC;**;*.*"    ,(merge-pathnames "**/*.*" clorb-home))
          ("**;*.*"        "CLORB:SRC;**;*.*" ))))


(load "clorb:src;clorb-files")
;;(setq net.cddr.clorb.system:*source-pathname-defaults* (pathname "clorb:src;"))
(net.cddr.clorb.system:reload)

(load "clorb:src;examples;hello;auto")

(defvar *orb* 
  (CORBA:ORB_init
   (list ;;"-ORBPort" "4712"
         "-ORBInitRef" "NameService=corbaloc::127.0.0.1:4744/NameService"
         #| "-ORBInitRef" "InterfaceRepository=" |#)))
(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))


;; Local variables:
;; inferior-lisp-program: "sbcl"
;; End:
