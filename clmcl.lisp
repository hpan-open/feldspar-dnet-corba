(in-package :cl-user)

(pushnew :use-acl-socket *features*)
(pushnew :use-my-idlparser *features*)
(pushnew :use-idlcomp *features*)

#+:use-acl-socket (require 'acl-socket)
(net.cddr.packer:require-package "BSD")
#+:use-my-idlparser
(net.cddr.packer:require-package "NET.CDDR.REDPAS")

(load "CLORB:SRC;CLORB-PKGDCL")
(load "CLORB:SRC;CLORB-FILES")
(clorb:reload)

(setq clorb:*port*  4711)

(ignore-errors
 (setq clorb:*host*
       (ccl::tcp-addr-to-str (ccl::local-interface-ip-address))))

(setq net.cddr.clorb.persistent-naming:*naming-base-path*
  (make-pathname :directory '(:relative "naming")
                 :name "foo"
                 :type "obj"))

(setq net.cddr.clorb.persistent-naming:*naming-ior-file*
      #P"ccl:NameService")

(setq clorb::*name-service*
      (let ((file net.cddr.clorb.persistent-naming:*naming-ior-file*))
        (format nil "file:~{/~A~}/~A"
                (cdr (pathname-directory (full-pathname file)))
                (pathname-name file))))

(ensure-directories-exist persistent-naming:*naming-base-path* :verbose t)

#|
(unless (find-class 'log-window nil)
  (load "home:contrib;log-window"))
(when (eq clorb::*log-output* t)
  (setq clorb::*log-output*
        (make-instance 'log-window :window-title "Log")))
|#

(defvar *orb* (CORBA:ORB_init))
(format t "~&;;; Activating the POA~%")
(progn 'ignore-errors
 (op:activate (op:the_poamanager (clorb::root-poa))))
;;(clorb:load-ir)
(net.cddr.clorb.persistent-naming:setup-pns)


(defun pentax-get (name)
  (clorb::http-get-ior "pentax.cddr.net" 80 name ))

(defun use-pentax-ifr ()
  (let ((ior (pentax-get "InterfaceRepository")))
    (when ior
      (setq clorb::*interface-repository* ior)
      (clorb::set-initial-reference *orb* "InterfaceRepository" ior))))

(defun use-pentax-ns ()
  (let ((ior (pentax-get "NameService")))
    (when ior 
      (clorb::set-initial-reference *orb* "NameService" ior))))

#+pentax-has-ifr
(use-pentax-ifr)

#+pentax-has-ifr
(use-pentax-ns)


(defun run ()
  (persistent-naming:setup-pns)
  (op:run *orb*))

;; 


(defun vsns-get (name &key stringified)
  (with-open-stream (s (ccl::open-tcp-stream "172.17.17.18" 5111))
    (ccl::telnet-write-line s name)
    (let ((ior (read-line s)))
      (clorb::mess 2 "vsns IOR: ~A" ior)
      (when (and (stringp ior)
                 (clorb::string-starts-with ior "IOR:"))
        (setq ior (string-trim #.(vector #\Linefeed #\Return #\Space) ior))
        (if stringified
          ior
          (op:string_to_object *orb* ior))))))

#+t2-has-ifr
(let ((ior (vsns-get "InterfaceRepository" :stringified t)))
  (when ior
    (setq clorb::*interface-repository* ior)
    (clorb::set-initial-reference *orb* "InterfaceRepository" ior) ))

(export 'vsns-get)
(import 'vsns-get "CLORB")

(setq clorb::*log-level* 3)

(defun clorb::xir ()
  (map 'list #'op:name (op:contents (clorb::get-ir) :dk_all t)))



#+(or)
(let ((ns (clorb::get-ns)))
  (defun pfoo (&optional (n 100))
    (dotimes (i n)
      (op:list ns 100))))


#|
(corba:idl #P"QuadX:Users:lenst:src:corba:interfaces:CosNaming.idl" :eval nil)
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


(setf (gethash "NameService" clorb::*boot-objects*)
      (clorb::get-ns))

(op:string_to_object *orb* "corbaloc::10.0.1.2:4711/NameService")

|#