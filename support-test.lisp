;;;; support-test.lisp -- support code for testing CLORB

(in-package :clorb)

(defun ensure-typecode (obj type)
  (ensure-typep obj 'CORBA:TypeCode)
  (typecase type
    (symbol
     (ensure (eql (op:kind obj) type)
             "typecode ~A should be of kind ~A" obj type))
    (CORBA:Typecode
     (ensure (op:equal obj type)
             "Typecode ~A equal to ~A" obj type))))


(defmethod match ((pattern corba:typecode) object)
  (boolean-match pattern object (op:equal object pattern)))



;;;; IR Definition Pattern

(defclass DEF-PATTERN (pattern)
  ((kind :initarg :kind :accessor def-kind)))

(defun def-pattern (kind &rest args)
  (make-instance 'def-pattern :kind kind :args args))

(defmethod match ((pattern def-pattern) def)
  (unless def
    (fail-match def "Missing"))
  (unless (eq (def-kind pattern)
              (op:def_kind def))
    (fail-match def "Wrong definition kind"))
  (call-next-method))


;;;; IR Repository Pattern 

(defclass REPOSITORY-PATTERN (pattern)
  ())

(defun repository-pattern (&rest args)
  (make-instance 'repository-pattern :args args))

(defmethod match ((pattern repository-pattern) object)
  (loop for (name pattern) on (pattern-args pattern) by #'cddr
        do (let ((def (op:lookup object name)))
             (cond ((null def) (fail-match object "has no definition named ~S" name))
                   (t
                    (handler-case 
                      (match pattern def)
                      (match-fail (condition)
                                  (fail-match object "~S ~A" name (match-fail-message condition)))))))))



;;;; Struct Pattern

(defclass STRUCT-PATTERN (pattern)
  ())

(defun struct-pattern (&rest args)
  (make-instance 'struct-pattern :args args))

(defmethod match ((pattern struct-pattern) object)
  (unless (typep object 'CORBA:struct)
    (fail-match object "Not a struct"))
  (call-next-method))

(defun struct-class-name (struct)
  (class-name (class-of struct)))


;;;; ISA Pattern

(defclass isa-pattern (pattern)
  ())

(defun isa (type)
  (make-instance 'isa-pattern :args type))

(defmethod print-object ((p isa-pattern) stream)
  (print-unreadable-object (p stream :type t)
    (prin1 (pattern-args p) stream)))

(defmethod match ((pattern isa-pattern) object)
  (boolean-match pattern object (typep object (pattern-args pattern))))


;;;; Additional ensure operators

(defmacro ensure-exception (code exception &rest pattern)
  `(handler-case
     (progn ,code
            (net.cddr.luna::tc-report "~S should raise exception" ',code))
     (,exception (exc)
                 (ensure-pattern* exc ,@pattern))
     (t (exc)
        (net.cddr.luna::tc-report "Should raise ~A. Got: ~A" ',exception exc))))

(defun std-minor (minor)
  (logior omg.org/corba::omgvmcid minor))

(defmacro ensure-repository (&rest args)
  `(ensure-pattern repository (repository-pattern ,@args)))
