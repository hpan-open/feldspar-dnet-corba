(in-package :cl-user)


(pushnew :clorb-dev *features*)
(pushnew :no-idlcomp *features*)
(pushnew :use-my-idlparser *features*)
(net.cddr.packer:require-package :net.cddr.redpas)


(let* ((clorb-home (merge-pathnames "src/clorb/" (user-homedir-pathname)))
       (clorb-fasl (merge-pathnames "fasl/" clorb-home)))
  (ensure-directories-exist clorb-fasl)
  (setf (logical-pathname-translations "CLORB")
        `(("SRC;**;*.fasl" ,(merge-pathnames "**/*.fasl" clorb-fasl))
          ("SRC;**;*.*"    ,(merge-pathnames "**/*.*" clorb-home))
          ("**;*.*"        "CLORB:SRC;**;*.*" ))))


(load "clorb-files")
(net.cddr.clorb.system:reload)

(setq clorb:*host* "localhost")

(defvar *the-orb*
  (CORBA:ORB_init
   (list "-ORBPort 5711"
         "-ORBInitRef NameService=http://localhost/NameService"
         "-ORBInitRef InterfaceRepository=http://localhost/InterfaceRepository")
   ""))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listening on port ~A~%" (clorb::orb-port clorb::*the-orb*))

