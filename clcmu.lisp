(in-package :cl-user)


(pushnew :clorb-dev *features*)


(let* ((clorb-home (merge-pathnames "src/clorb/" (user-homedir-pathname))))
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.*"    ,(merge-pathnames "**/*.*" clorb-home))
          ("**;*.*"        "CLORB:SRC;**;*.*" ))))


(load "clorb-files")
(setq net.cddr.clorb.system::*binary-folder* "fasl")
(net.cddr.clorb.system:reload)

(setq clorb:*host* "localhost")

(defvar *the-orb*
  (CORBA:ORB_init
   (list "-ORBPort 5711"
         "-ORBInitRef NameService=corbaloc::/NameService"
         "-ORBInitRef InterfaceRepository=http://localhost/InterfaceRepository")
   ""))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))

