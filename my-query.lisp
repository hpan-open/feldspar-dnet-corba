(in-package :clorb)

(defclass QNODE (servant)
  ()
  (:default-initargs
      :interface-id "IDL:MyQuery/Node:1.0"))


(define-method op::list ((n qnode) filter)
  (describe filter)
  nil)


(define-method op::default_filter ((n qnode) group)
  (declare (ignore group))
  (corba:union 
   :union-discriminator 3
   :union-value "hello"))


(defparameter *qnode* (make-instance 'qnode))

(rebind *qnode* "node")
