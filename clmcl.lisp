(in-package :cl-user)

(load "clorb:clorb.system")
(load "clorb:clorb-pkgdcl")
(load "clorb:mcl-patches")

(make:oos :clorb :load)

(import 'cl-user::§ :clorb)
(import 'cl-user::§§ :clorb)
(import 'cl-user::% :clorb)


(define-symbol-macro cl-user::§§§
  (in-package :clorb))

(define-symbol-macro clorb::§§§
  (in-package :cl-user))


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
(clorb::load-ir)

#|
(setq clorb::*host*
      (clorb::listner-host 
       (clorb::listner-socket (clorb::adaptor clorb::*the-orb*))))
|#

(setq clorb::*host*
      (ccl::tcp-addr-to-str (ccl::local-interface-ip-address)))
