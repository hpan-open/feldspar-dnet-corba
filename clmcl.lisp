(in-package :cl-user)

(pushnew :use-acl-socket *features*)
(require 'acl-socket)
(net.cddr.packer:require-package "BSD")

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

(defun hh ()
  (setup-hello :file "hello.ior")
  (hello-client :file "hello.ior"))

(defun hhn ()
  (setup-hello :name "hello")
  (hello-client :name "hello"))


(defun run ()
  (persistent-naming:setup-pns)
  (op:run *orb*))

(defvar *calc-ior* "file:///Mac_OS_X/tmp/ObjectID")
(defvar *calc*)
(defun do-calc ()
  (setq *calc* (op:string_to_object *orb* *calc-ior*))
  (corba:funcall "add" *calc* 12.5 5.8)
  (corba:funcall "div" *calc* 9.9 3))


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


(setq clorb::*host-translations*
      '(("saturn" . "172.17.17.42")
        ("quad.lst" . "172.17.17.17")))

#+(or)
(let ((ns (clorb::get-ns)))
  (defun pfoo (&optional (n 100))
    (dotimes (i n)
      (op:list ns 100))))


(defparameter +calculator.idl+
  #P"Macintosh HD:Users:lenst:src:corba:interfaces:Calculator.idl")