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

(setf clorb::*running-orb* (CORBA:ORB_init))
(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(clorb:load-ir)

#|
(setq clorb::*host*
      (clorb::listner-host 
       (clorb::listner-socket (clorb::adaptor clorb::*the-orb*))))
|#

(setq clorb:*host*
      (ccl::tcp-addr-to-str (ccl::local-interface-ip-address)))

#+t2-has-ifr
(with-open-stream (s (ccl::open-tcp-stream "172.17.17.18" 5111))
  (let ((ior (read-line s)))
    (when (and (stringp ior)
               (> (length ior) 4)
               (string= "IOR:" ior :end2 4))
      (when (eql #\Linefeed (char ior (1- (length ior))))
        (setq ior (subseq ior 0 (1- (length ior)))))
      (setq clorb::*interface-repository* ior))))

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
        (setq clorb::*interface-repository* ior))
       (t
        (clorb::mess 4 "non ior=~A" ior))))))


(defun hh ()
  (cl-user::setup-hello :file "hello.ior")
  (cl-user::hello-client :file "hello.ior"))

(defun hhn ()
  (cl-user::setup-hello :name "hello")
  (cl-user::hello-client :name "hello"))


(defun run ()
  (setup-pns)
  (op:run clorb::*the-orb*))
