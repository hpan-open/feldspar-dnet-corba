(in-package :cl-user)

(defun clean-fasl ()
  (mapc #'delete-file (directory "clorb:fasl;**;*.cfsl")))

(load "clorb:src;devel")

#|
(net.cddr.clorb.system:set-load-opts
 :server t  :idlcomp nil  :my-idlparser t
 :portable-interceptor t  :support-test t)
(net.cddr.clorb.system:reload)
|#

#+(or)
(ignore-errors
 (setq clorb:*host*
       (let ((local-ip (ccl::local-interface-ip-address)))
         (if (zerop local-ip)
           "localhost"
           (ccl::tcp-addr-to-str local-ip)))))


(ensure-directories-exist persistent-naming:*naming-base-path* :verbose t)

;;(clorb::io-system-switch 'clorb::io-system-multiprocess)
(persistent-naming:setup-pns :export t)


(defun use-pentax-ifr ()
  (CORBA:ORB_init '("-ORBInitRef InterfaceRepository=http://10.0.1.251/InterfaceRepository")))

(defun use-pentax-ns ()
  (CORBA:ORB_init '("-ORBInitRef NameService=http://10.0.1.251/NameService")))

(defun use-mcl-ns ()
  (CORBA:ORB_init '("-ORBInitRef NameService=corbaloc::127.0.0.1:4711/NameService")))

(defun use-quad-mcl-ns ()
  (CORBA:ORB_init '("-ORBInitRef NameService=corbaloc::quad.local.:4711/NameService")))

(defun use-jacorb ()
  (CORBA:ORB_init '("-ORBInitRef NameService=http://localhost/~lenst/NS_ref")))



(defun run ()
  (persistent-naming:setup-pns :export t)
  (op:run *the-orb*))

(setq clorb::*log-level* 3)

;; http://www.random.org/Random.ior
(defparameter *random-ior*
  "IOR:000000000000000f49444c3a52616e646f6d3a312e3000000000000100000000000000500001000000000016706c616e7874792e6473672e63732e7463642e69650006220000002c3a5c706c616e7874792e6473672e63732e7463642e69653a52616e646f6d3a303a3a49523a52616e646f6d00")



#|

(defun clorb::xir ()
  (map 'list #'op:name (op:contents (clorb::get-ir) :dk_all t)))

(corba:idl "clorb:idl;TypeCodeFactory.idl"
           :eval nil :output "clorb:y-typecodefactory.lisp"
           :only '("CORBA::TypeCodeFactory") :exclude nil :skeleton nil)

(defclass local-target (clorb::stub-target) ()
  (:default-initargs :struct-marshal nil))
(corba:idl "clorb:idl;pi.idl" 
           :eval nil :output "clorb:y-pi-base.lisp"
           :exclude '("::CORBA" "::IOP" "::GIOP" "::IIOP")
           :package-decl t
           :target 'local-target)



(corba:idl "clorb:idl;orb.idl" :eval nil :print t :skeleton nil
           :output "clorb:src;y-orb.lisp")


(corba:idl "clorb:idl;interface_repository.idl" :eval nil :print t :skeleton nil
           :exclude '("::CORBA::TypeCode"))

(with-open-file (o "clorb:y-ifr-idl.lisp" :direction :output :if-exists :supersede) (write (idef-write common-lisp-user::irr :default-prefix "omg.org") :stream o :pretty t))


(op:string_to_object *the-orb* "corbaloc::10.0.1.2:4711/NameService")

|#

(defvar *export-ifr* nil)

(defun export-iors ()
  (with-open-file (out "SaturnX:private:tmp:NameService" 
                       :direction :output :if-exists :supersede)
    (princ (op:object_to_string *the-orb* (clorb::get-ns)) out))
  (unless *export-ifr*
    (setq *export-ifr* (corba:idl "clorb:idl;CosNaming.idl" :eval nil))
    (corba:idl "clorb:idl;interface-repository.idl" :repository *export-ifr* :eval nil))
  (with-open-file (out "SaturnX:private:tmp:InterfaceRepository"
                       :direction :output :if-exists :supersede)
    (princ (op:object_to_string *the-orb* *export-ifr*) out)))

(defun home-volume-pathname ()
    (let ((home (user-homedir-pathname)))
      (make-pathname :host (pathname-host home)
                     :directory (subseq (pathname-directory home) 0 2)
                     :device (pathname-device home))))


(defun obj (str &optional type)
  (let ((proxy (op:string_to_object *the-orb* str)))
    (if type
      (net.cddr.clorb::nobject-narrow proxy type)
      proxy)))

;; Federate
;;(op:bind_context (net.cddr.clorb::get-ns) (net.cddr.clorb::ns-name "main") (obj "corbaloc::/NameService" 'cosnaming:namingcontextext))
;;(op:bind_context (net.cddr.clorb::get-ns) (net.cddr.clorb::ns-name "saturn") (obj "corbaloc::saturn.local./NameService" 'cosnaming:namingcontextext))



;;;; Bg driver
(in-package :clorb)
(defvar *work-pending* nil)
(defun bg-driver ()
  (loop
    (process-wait "wating for main thread"
                  (lambda () (not *work-pending*)))
    (io-driver nil)
    (when (io-work-pending-p)
      (setq *work-pending* t)
      (ccl:eval-enqueue (lambda ()
                          (orb-work *the-orb* t t)
                          (setq *work-pending* nil))))))
  
;;(ccl:process-run-function "orb" #'bg-driver)
