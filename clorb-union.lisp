;;;; clorb-union.lisp -- CORBA Union support
;; $Id: clorb-union.lisp,v 1.3 2002/10/05 13:51:00 lenst Exp $

(in-package :clorb)

(defclass CORBA:union () 
  ((discriminator 
    :initarg :discriminator
    :accessor union-discriminator)
   (value
    :initarg :value
    :accessor union-value )))


(defun create-union-tc (id name discriminator-type members)
  (check-type id string)
  (check-type name string)
  (check-type discriminator-type corba:typecode)
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

(defvar *union-registry*
  (make-hash-table :test #'equal))




(defun corba:union (&key union-discriminator union-value
                           id typecode)
  (let ((id (or id (and typecode (op:id typecode)))))
    (let ((name (gethash id *union-registry*)))
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
    (:tk_enum (doseq (sym (tcp-member-symbols (typecode-params typecode)))
                (funcall function sym)))
    (otherwise (loop for i from 0 do (funcall function i)))))


(defmacro define-union (symbol &key id (name "") discriminator-type members)
  "members = (label type &key creator name default)*
     where labels = () is default member"
  (let ((used-names '())
        (code '())
        (tc-members '()) )
    (dolist (m members)
      (destructuring-bind (label type &key creator (name "") default
                                 (accessor (string-upcase name))) m
        (push `(list ,(if default ''default label) ,name ,type) 
              tc-members)
        (unless (member name used-names :test #'equal)
          (push name used-names)
          (push `(progn 
                   (define-method ,accessor ((obj ,symbol))
                     (union-value obj))
                   (define-method (setf ,accessor) (value (obj ,symbol))
                     (setf (union-discriminator obj) ,label)
                     (setf (union-value obj) value))
                   ,(if creator
                      `(defun ,creator (value)
                         (,symbol :union-value value :union-discriminator ,label))))
                code)
          (when default
            (push `(progn 
                     (define-method default ((obj ,symbol)) (union-value obj))
                     (define-method (setf default) (value (obj ,symbol))
                       (setf (union-discriminator obj) ,label)
                       (setf (union-value obj) value))) code)))))
    
    `(progn
       (defclass ,symbol (corba:union) ())
       (setf (gethash ,id *union-registry*) ',symbol)
       (defun ,symbol (&key union-value union-discriminator)
         (make-instance ',symbol 
           :value union-value
           :discriminator union-discriminator))
       (set-symbol-ifr-id ',symbol ,id)
       (set-symbol-typecode ',symbol
                            (lambda ()
                              (create-union-tc ,id ,name
                                               ,discriminator-type
                                               (list ,@(nreverse tc-members)))))
       ,@code)))


#|
(define-union omg.org/root::filter :name "filter" :id "idl:filter.1.0"
  :discriminator-type corba:tc_long
  :members ((0 corba:tc_string :name "foo" :creator filter/foo))
)
|#
;;; clorb-union.lisp ends here
