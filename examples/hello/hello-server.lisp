(in-package :cl-user)

(defclass HELLO-WORLD (hello:world-servant)
  ((motd :initform "Hello World")))

(corba:define-method greet ((self hello-world))
  (slot-value self 'motd))

(defvar *hello-servant* nil)

(defun setup-hello (&key file name)
  (unless *hello-servant*
    (setq *hello-servant* 
      (make-instance 'hello-world)))
  (let ((orb (CORBA:ORB_init)))
    ;; Register the object servant with the ORB
    (let ((poa (op:resolve_initial_references orb "RootPOA")))
      ;; Optionally activate object (RootPOA has implicit activation)
      ;;(op:activate_object poa *hello-servant*)
      ;; Activate the POAManager to allow the POA to accept requests
      (op:activate (op:the_poamanager poa)))
    ;; Store object reference in file or in naming service
    (when file
      (with-open-file (wr file :direction :output
                       :if-exists :supersede)
        (format wr "~A~%" (op:object_to_string orb *hello-servant*))))
    (when name
      ;; Clorb utility function to access naming service
      (clorb:rebind *hello-servant* name))
    *hello-servant*))

(defun run-hello (&rest args)
  (let ((orb (CORBA:ORB_init)))
    (when args (apply #'setup-hello args))
    (op:run orb)))
