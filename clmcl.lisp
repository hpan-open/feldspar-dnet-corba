(in-package :cl-user)

(load "CLORB:SRC;CLORB-PKGDCL")

(defparameter clorb:*port*  4711)

(defparameter *naming-base-path*
  (make-pathname :directory '(:relative "naming")
                 :name "foo"
                 :type "obj"))

(defparameter *naming-ior-file* #P"ccl:NameService")

(setq clorb::*name-service*
      #+file-url-does-the-right-thing
      (format nil "file:~{/~A~}/~A"
              (cdr (pathname-directory (full-pathname *naming-ior-file*)))
              (pathname-name *naming-ior-file*))
      (format nil "file:~A"
              (namestring *naming-ior-file*)))

;;(clorb::set-initial-reference clorb::*the-orb* "NameService" clorb::*name-service*)

(ensure-directories-exist *naming-base-path* :verbose t)


;;(make:oos :clorb :load)

(import 'cl-user::§ :clorb)
(import 'cl-user::§§§ :clorb)
(import 'cl-user::% :clorb)

(define-symbol-macro cl-user::§§
  (in-package :clorb))

(define-symbol-macro clorb::§§
  (in-package :cl-user))

(load "CLORB:SRC;CLORB-FILES")
(clorb:reload)

#|
(unless (find-class 'log-window nil)
  (load "home:contrib;log-window"))
(when (eq clorb::*log-output* t)
  (setq clorb::*log-output*
        (make-instance 'log-window :window-title "Log")))
|#

(defvar *orb* (CORBA:ORB_init))
(format t "~&;;; Activating the POA~%")
(ignore-errors
 (op:activate (op:the_poamanager (clorb::root-poa))))
(clorb:load-ir)

(ignore-errors
 (setq clorb:*host*
       (ccl::tcp-addr-to-str (ccl::local-interface-ip-address))))

#+t2-has-ifr
(with-open-stream (s (ccl::open-tcp-stream "172.17.17.18" 5111))
  (ccl::telnet-write-line s "InterfaceRepository")
  (let ((ior (read-line s)))
    (format t "IOR: ~A~%" ior)
    (when (and (stringp ior)
               (> (length ior) 4)
               (string= "IOR:" ior :end2 4))
      (when (eql #\Linefeed (char ior (1- (length ior))))
        (setq ior (subseq ior 0 (1- (length ior)))))
      (setq clorb::*interface-repository* ior)
      (clorb::set-initial-reference *orb* "InterfaceRepository" ior)
      ior)))

#+pentax-has-ifr
(with-open-stream (s (ccl::open-tcp-stream "172.17.17.1" 80))
  (let ((crlf (coerce (vector #\Return #\Linefeed) 'string)))
    (format s "GET /InterfaceRepository~A" crlf)
    (let ((ior (read-line s)))
      (cond 
       ((and (stringp ior)
                 (> (length ior) 4)
                 (string= "IOR:" ior :end2 4))
        (setq ior (string-right-trim crlf ior))
        (setq clorb::*interface-repository* ior)
        (clorb::set-initial-reference *orb* "InterfaceRepository" ior))
       (t
        (clorb::mess 4 "non ior=~A" ior))))))


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


(defun vsns-get (name)
  (with-open-stream (s (ccl::open-tcp-stream "172.17.17.18" 5111))
    (ccl::telnet-write-line s name)
    (let ((ior (read-line s)))
      (clorb::mess 2 "vsns IOR: ~A" ior)
      (when (and (stringp ior)
                 (clorb::string-starts-with ior "IOR:"))
        (setq ior (string-trim #.(vector #\Linefeed #\Return #\Space) ior))
        (omg.org/features:string_to_object *orb* ior)))))

(export 'vsns-get)
(import 'vsns-get "CLORB")

#|
(setq clorb::*log-level* 3)
|#
