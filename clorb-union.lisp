;;;; clorb-union.lisp -- CORBA Union support

(in-package :clorb)

(defclass CORBA:UNION () 
  ((discriminator 
    :initarg :discriminator
    :accessor union-discriminator)
   (value
    :initarg :value
    :accessor union-value )))


(defun create-union-tc (id name discriminator-type members)
  "Create a TypeCode for union type.
members = ( (label name typecode)* )
where label = symbol clorb:default or value"
  (check-type id string)
  (check-type name string)
  (check-type discriminator-type corba:typecode)
  (setq members (coerce members 'list))
  (let* ((default-index -1)
         (massaged-members 
          (loop for (label name typecode) in members
                for i from 0
                do (when (eq label 'default)
                     (setq default-index i)
                     (setq label (arbritary-value discriminator-type)))
                collect (list label name typecode))))
    (make-typecode :tk_union id name
                   discriminator-type default-index 
                   (coerce massaged-members 'vector))))


(defmethod any-typecode ((obj corba:union))
  (symbol-typecode (class-name (class-of obj))))

(defmethod any-value ((obj corba:union))
  obj)

(defun corba:union (&key union-discriminator union-value
                           id typecode)
  (let ((id (or id (and typecode (op:id typecode)))))
    (let ((name (ifr-id-symbol id)))
      (if name
        (funcall name 
                 :discriminator union-discriminator
                 :value union-value)
        (make-instance 'corba:union
          :discriminator union-discriminator
          :value union-value)))))

(define-method default ((obj corba:union)) (union-value obj))
(define-method (setf default) (value (obj corba:union)) 
  (setf (union-value obj) value))


(defun typecode-values-do (function typecode) 
  (case (op:kind typecode)
    (:tk_char 
     (loop for code from 0 below char-code-limit
           for char = (code-char code)
           when char do (funcall function char)))
    (:tk_boolean (funcall function nil) (funcall function t)) 
    (:tk_enum (doseq (sym (tc-keywords typecode))
                (funcall function sym)))
    (otherwise (loop for i from 0 do (funcall function i)))))


;;; clorb-union.lisp ends here
