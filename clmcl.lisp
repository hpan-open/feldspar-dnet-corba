(in-package :cl-user)

;;(pushnew :use-acl-socket *features*)
;;(pushnew :use-my-idlparser *features*)
;;(pushnew :no-idlcomp *features*)
(pushnew :clorb-dev *features*)


#+:use-acl-socket (require 'acl-socket)
(net.cddr.packer:require-package "BSD")
#+:use-my-idlparser
(net.cddr.packer:require-package "NET.CDDR.REDPAS")

(defun clean-fasl ()
  (mapc #'delete-file (directory "clorb:*.cfsl")))

(load "CLORB:SRC;CLORB-FILES")
(net.cddr.clorb.system:reload)


(ignore-errors
 (setq clorb:*host*
       (let ((local-ip (ccl::local-interface-ip-address)))
         (if (zerop local-ip)
           "localhost"
           (ccl::tcp-addr-to-str local-ip)))))

(setq net.cddr.clorb.persistent-naming:*naming-base-path*
  (make-pathname :directory '(:relative "naming")
                 :name "foo"
                 :type "obj"))

(setq net.cddr.clorb.persistent-naming:*naming-ior-file* nil)

(ensure-directories-exist persistent-naming:*naming-base-path* :verbose t)


#|
(unless (find-class 'log-window nil)
  (load "home:contrib;log-window"))
(when (eq clorb::*log-output* t)
  (setq clorb::*log-output*
        (make-instance 'log-window :window-title "Log")))
|#

(defvar *orb*
  (CORBA:ORB_init
   (list "-ORBPort" "4711"
         "-ORBInitRef" "NameService=corbaloc::127.0.0.1:4711/NameService"
         #| "-ORBInitRef" "InterfaceRepository=" |#)))


(format t "~&;;; Activating the POA~%")
(progn 'ignore-errors
 (op:activate (op:the_poamanager (clorb::root-poa))))
;;(clorb:load-ir)
(persistent-naming:setup-pns :export t)


(defun use-pentax-ifr ()
  (clorb::set-initial-reference *orb* "InterfaceRepository" 
                                "http://10.0.1.251/InterfaceRepository"))

(defun use-pentax-ns ()
  (clorb::set-initial-reference *orb* "NameService" 
                                "http://10.0.1.251/NameService"))



(defun run ()
  (persistent-naming:setup-pns :export t)
  (op:run *orb*))



(setq clorb::*log-level* 3)

(defun clorb::xir ()
  (map 'list #'op:name (op:contents (clorb::get-ir) :dk_all t)))



#+(or)
(let ((ns (clorb::get-ns)))
  (defun pfoo (&optional (n 100))
    (dotimes (i n)
      (op:list ns 100))))


#|
(corba:idl "clorb:idl;CosNaming.idl" :eval nil)
|#

(load "clorb:examples;hello;auto")

#|

(corba:idl "clorb:idl;TypeCodeFactory.idl"
           :eval nil :print t 
           :only '("CORBA::TypeCodeFactory") :skeleton nil
           :exclude nil )

(gen-stub-file (lookup-name-in r "CORBA::TypeCodeFactory") "clorb:y-typecodefactory.lisp"  :package-def t)

(corba:idl "clorb:idl;pi.idl" 
           :eval nil :print t )


(corba:idl "clorb:idl;orb.idl" :eval nil :print t :skeleton nil)

(corba:idl "clorb:idl;interface_repository.idl" :eval nil :print t :skeleton nil
           :exclude '("::CORBA::TypeCode"))

(with-open-file (o "clorb:y-ifr-idl.lisp" :direction :output :if-exists :supersede) (write (idef-write common-lisp-user::irr :default-prefix "omg.org") :stream o :pretty t))


(op:string_to_object *orb* "corbaloc::10.0.1.2:4711/NameService")

|#

(defparameter *random-ior*
  "IOR:000000000000000f49444c3a52616e646f6d3a312e3000000000000100000000000000500001000000000016706c616e7874792e6473672e63732e7463642e69650006220000002c3a5c706c616e7874792e6473672e63732e7463642e69653a52616e646f6d3a303a3a49523a52616e646f6d00")

(import '(@ @@ @@@) "CLORB")
(define-symbol-macro inpc (in-package :clorb))
(define-symbol-macro clorb::inpu (in-package :cl-user))

#|
(setq ccl::*search-default* (format nil "~A:" (package-name (find-package :corba)))
      ccl::*replace-default* "corba:")
(setq ccl::*search-default* (format nil "~A:" (package-name (find-package :op)))
      ccl::*replace-default* "op:")

|#

(load "clorb:src;all-test")
