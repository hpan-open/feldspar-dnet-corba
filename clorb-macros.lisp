;;; clorb-macros.lisp -- Macros for CLORB
;;; $Id: clorb-macros.lisp,v 1.3 2001/02/13 22:21:57 lenst Exp $

(in-package :clorb)

;;;; Sysdep

#-clisp
(defmacro doseq ((var seq) &body forms)
  `(map nil (lambda (,var) ,@forms) ,seq))
#+clisp
(import 'lisp:doseq)

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'format))
		

;;;; Fix incomplete format in CLISP

#+clisp
(defun format (stream fmtstr &rest args)
  (apply #'cl:format stream (clisp-clean-format-string fmtstr) args))

#+clisp
(defun clisp-clean-format-string (string)
  (loop for pos = (search "~_" string)
        while pos
        do (setq string (concatenate 'string
                                     (subseq string 0 pos)
                                     (subseq string (+ pos 2)))))
  string)

;;;; MCL Has old style (?) make-load-form
#+MCL
(require "ANSI-MAKE-LOAD-FORM")

(defmacro define-slot-dumper (class)
  `(defmethod make-load-form ((obj ,class) &optional env)
     (make-load-form-saving-slots obj :environment env)))


;;;; Operations

(defmacro define-method (name args &body body)
  (let ((opkg  "OMG.ORG/FEATURES")
        doc-string
        setf-form)
    (when (consp name)
      (assert (eq (first name) 'setf))
      (setq setf-form t)
      (setq name (second name)))
    (when (and body (cdr body) (stringp (car body)))
      (setq doc-string (list (pop body))))
    (let ((opsym (intern (string name) opkg)))
      `(progn
         (eval-when (:execute :compile-toplevel :load-toplevel)
           (export ',opsym ,opkg))
         ,(if setf-form
              `(defmethod (setf ,opsym) ,args
                          ,@doc-string ,@body)
            `(defmethod ,opsym (,(first args) &rest -args-)
               ,@doc-string
               (destructuring-bind ,(rest args) -args- 
                 ,@body)))))))


#||
(define-method foo ((x symbol) y &key z)
  (list x y z))
(define-method (setf zep) (val (obj cons))
  (setf (cdr obj) val))
||#

;;;; Define Corba Class

(defstruct attspec  
  name readonly virtual
  slotopts)

(defun attspec-parse (attspec)
  (let ((name (pop attspec))
        (readonly nil)
        (slotopts nil)
        (virtual nil))
    (when (not (keywordp (first attspec)))
        (setf slotopts `(:initform ,(pop attspec))))
    (when (eq :readonly (first attspec))
        (setf readonly t)
        (pop attspec))
    (when (eq :virtual (first attspec))
      (pop attspec)
      (setf virtual (pop attspec)))
    (make-attspec :name name :readonly readonly :virtual virtual
                  :slotopts (nconc slotopts attspec))))

(defun attribute-ops (attspec class)
  (let* ((name
          (attspec-name attspec))
         (opname
          (intern (symbol-name name) :op))
         (giop-get-name
          (intern (format nil "_GET_~a" (symbol-name name)) :op))
         (giop-set-name
          (intern (format nil "_SET_~a" (symbol-name name)) :op))
         (read-form
          (if (attspec-virtual attspec)
              `(,(attspec-virtual attspec) obj)
            `(slot-value obj ',name))))
    `(progn
       (define-method ,opname ((obj ,class)) ,read-form)
       ,@(if (not (attspec-readonly attspec))
             `((defmethod (setf ,opname) (value (obj ,class))
                 (setf ,read-form value))))
       (define-method ,giop-get-name ((obj ,class))
         (,opname obj))
       ,@(if (not (attspec-readonly attspec))
             `((define-method ,giop-set-name ((obj ,class) value)
                 (setf (,opname obj) value)))))))


(defmacro define-corba-class (name superclasses 
                              &key attributes slots defaults)
  (let ((asl (mapcar #'attspec-parse attributes)))
    `(progn
       (defclass ,name ,superclasses
         (,@(remove nil
                    (mapcar 
                     (lambda (attspec)
                       (if (attspec-virtual attspec)
                           nil
                         (list* 
                          (attspec-name attspec)
                          :initarg 
                          (intern (string (attspec-name attspec)) :keyword)
                          (attspec-slotopts attspec))))
                     asl))
            ,@slots)
         (:default-initargs 
           ,@defaults))
       ,@(mapcar #'(lambda (attspec) (attribute-ops attspec name))
                 asl))))


;;; clorb-macros.lisp ends here
