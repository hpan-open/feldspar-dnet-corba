;; $Id: clorb-union.lisp,v 1.1 2000/11/14 22:50:30 lenst Exp $

(in-package :clorb)

(defclass corba:union () 
  ((discriminator :initarg :discriminator)
   (value :initarg :value)))


(defun corba:union (&key union-discriminator union-value)
  (make-instance 'corba:union
    :discriminator union-discriminator
    :value union-value))

(defmethod union-discriminator ((obj corba:union))
  (slot-value obj 'discriminator))

(defmethod union-value ((obj corba:union))
  (slot-value obj 'value))


;;; clorb-union.lisp ends here
