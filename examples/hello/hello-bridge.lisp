(in-package :cl-user)

(defclass hello-world-bridge (hello:world-servant)
  ((other :initarg :other :accessor other)))


(corba:define-method greet ((self hello-world-bridge))
  (format nil "Bridge: '~A'" (op:greet (other self))))

(corba:define-method read ((self hello-world-bridge) name)
  (let ((fox (op:read (other self) name)))
    (when fox
      (incf (op:value fox)))
    fox))

(corba:define-method write ((self hello-world-bridge) box)
  (op:write (other self) box))

(corba:define-method repr ((self hello-world-bridge) box)
  (op:repr (other self) box))

(corba:define-method write2 ((self hello-world-bridge) box1 box2)
  (op:write2 (other self) box1 box2))


;;(defvar *hello-servant* nil)

(setq *hello-servant*
      (make-instance 'hello-world-bridge
        :other (clorb:resolve "hello")))


