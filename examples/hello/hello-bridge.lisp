(in-package :cl-user)

(defclass hello-world-bridge (hello:world-servant)
  ((other :initarg :other :accessor other)))


(corba:define-method greet ((self hello-world-bridge))
  (format nil "Bridge: '~A'" (op:greet (other self))))

;;(defvar *hello-servant* nil)

(setq *hello-servant*
      (make-instance 'hello-world-bridge
        :other (clorb:resolve "hello")))


