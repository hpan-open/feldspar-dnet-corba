(in-package :cl-user)

(pushnew :use-acl-socket *features*)
(require 'acl-socket)


(load "CLORB:SRC;CLORB-PKGDCL")
(import '(§ §§§ %) :clorb)
(load "CLORB:SRC;CLORB-FILES")
(clorb:reload)

(setq clorb:*port*  4711)
(ignore-errors
 (setq clorb:*host*
       (ccl::tcp-addr-to-str (ccl::local-interface-ip-address))))
(setq *naming-base-path*
  (make-pathname :directory '(:relative "naming")
                 :name "foo"
                 :type "obj"))
(setq *naming-ior-file* #P"ccl:NameService")
(setq clorb::*name-service*
      #+file-url-does-the-right-thing
      (format nil "file:~{/~A~}/~A"
              (cdr (pathname-directory (full-pathname *naming-ior-file*)))
              (pathname-name *naming-ior-file*))
      (format nil "file:~A"
              (namestring *naming-ior-file*)))

(ensure-directories-exist *naming-base-path* :verbose t)

(define-symbol-macro §§         (in-package :clorb))
(define-symbol-macro clorb::§§  (in-package :cl-user))

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
(clorb:load-ir)




(defun pentax-get (name)
  (with-open-stream (s (ccl::open-tcp-stream "pentax.cddr.net" 80))
    (let ((crlf (coerce (vector #\Return #\Linefeed) 'string)))
      (format s "GET /~A~A" name crlf)
      (let ((ior (read-line s)))
        (cond 
         ((and (stringp ior)
               (> (length ior) 4)
               (string= "IOR:" ior :end2 4))
          (setq ior (string-right-trim crlf ior)))
         (t
          (clorb::mess 4 "non ior=~A" ior)
          nil))))))

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
  (setup-pns)
  (op:run *orb*))

(defvar *calc-ior* "file:Mac_OS_X:tmp:ObjectID")
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
          (omg.org/features:string_to_object *orb* ior))))))

#+t2-has-ifr
(let ((ior (vsns-get "InterfaceRepository" :stringified t)))
  (when ior
    (setq clorb::*interface-repository* ior)
    (clorb::set-initial-reference *orb* "InterfaceRepository" ior) ))

(export 'vsns-get)
(import 'vsns-get "CLORB")

#|
|#
(setq clorb::*log-level* 3)

(defun clorb::xir ()
  (map 'list #'op:name (op:contents (clorb::get-ir) :dk_all t)))


(setq clorb::*host-translations*
      '(("saturn" . "172.17.17.42")))
