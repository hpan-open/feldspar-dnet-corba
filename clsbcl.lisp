(in-package :cl-user)

(format t "~&;;;; Loading CLORB~%")

(pushnew :clorb-dev *features*)


;;(require :db-sockets)
;;(require :asdf)
(require :sb-bsd-sockets)

(let* ((clorb-home (merge-pathnames "src/clorb/" (user-homedir-pathname)))
       (clorb-fasl (merge-pathnames "fasl/" clorb-home)))
  (ensure-directories-exist clorb-fasl)
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.fasl" ,(merge-pathnames "**/*.fasl" clorb-fasl))
          ("SRC;**;*.*"    ,(merge-pathnames "**/*.*" clorb-home))
          ("**;*.*"        "CLORB:SRC;**;*.*" ))))


(load "clorb:src;clorb-files")
;;(setq net.cddr.clorb.system::*source-pathname-defaults* (pathname "clorb:src;"))
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
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))


;; Local variables:
;; inferior-lisp-program: "sbcl"
;; End:
