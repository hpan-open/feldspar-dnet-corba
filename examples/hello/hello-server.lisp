(in-package :cl-user)

(defclass HELLO-WORLD (hello:world-servant)
  ((motd :initform 
         (format nil "Hello World from ~A" (lisp-implementation-type)))))

(corba:define-method greet ((self hello-world))
  (slot-value self 'motd))

(corba:define-method read ((self hello-world) name)
  (cond ((equal name "") nil)
        (t (make-instance 'hello:fox :name name :value (length name)))))

(corba:define-method write ((self hello-world) box)
  (clorb::mess 5 "~A" box))

(corba:define-method repr ((self hello-world) box)
  (princ-to-string box))

(corba:define-method write2 ((self hello-world) box1 box2)
  (cond ((eql box1 box2) "They are the same")
        (t (format nil "~A and ~A" box1 box2))))


(defmethod print-object ((fox hello:fox) stream)
  (print-unreadable-object (fox stream :type t :identity t)
    (format stream "~A #~A" (op:name fox) (op:value fox))))


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
