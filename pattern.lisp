;;;; test-idlcomp.lisp

(in-package :clorb)

(defvar *temporary-directory*
  (let ((base (truename "ccl:")))
    (make-pathname
     :directory (list :absolute (second (pathname-directory base)) "tmp")
     :defaults base)))

(defun repository-from-string (string)
  (let ((repository (make-instance 'repository))
        (temp-file (merge-pathnames "working.idl" *temporary-directory*)))
    (with-open-file (out temp-file :direction :output :if-exists :supersede)
      (princ string out)
      (terpri out))
    (load-repository *default-idl-compiler* repository temp-file)
    repository))


(define-condition match-fail (warning)
                  ((object :initarg :object :reader match-fail-object)
                   (message :initarg :message :reader match-fail-message))
  (:report print-match-fail))

(defun print-match-fail (condition stream)
  (format stream "IR object ~S : ~A" (match-fail-object condition)
          (match-fail-message condition)))

(defvar *failed-match-warning* t)

(defun fail-match (object format &rest args)
  (funcall (if *failed-match-warning* #'warn #'signal)
           'match-fail
           :object object
           :message (apply #'cl:format nil format args)))



(defgeneric match (pattern object))

(defun boolean-match (pattern object boolean)
  (unless boolean
    (fail-match object "~S does not match ~S" object pattern)))

(defmethod match ((pattern t) object)
  (boolean-match pattern object (equalp pattern object)))

(defmethod match ((pattern CORBA:TypeCode) object)
  (boolean-match pattern object (op:equal object pattern)))


(defclass pattern ()
  ((args :initarg :args :accessor pattern-args)))

(defun pattern (&rest args)
  (make-instance 'pattern :args args))

(defmethod match ((pattern pattern) object)
  (loop for (key value) on (pattern-args pattern) by #'cddr
        do (let ((attval (funcall key object)))
             (handler-case 
               (match value attval)
               (match-fail (condition)
                           (fail-match object "~S ~A" key (match-fail-message condition)))))))


(defclass def-pattern (pattern)
  ((kind :initarg :kind :accessor def-kind)))

(defun def-pattern (kind &rest args)
  (make-instance 'def-pattern :kind kind :args args))

(defmethod match ((pattern def-pattern) def)
  (unless (eq (def-kind pattern)
              (omg.org/features:def_kind def))
    (fail-match def "Wrong definition kind"))
  (call-next-method))


(defclass repository-pattern (pattern)
  ())

(defmethod match ((pattern repository-pattern) object)
  (loop for (name pattern) on (pattern-args pattern) by #'cddr
        do (let ((def (op:lookup object name)))
             (cond ((null def) (fail-match object "has no definition named ~S" name))
                   (t
                    (handler-case 
                      (match pattern def)
                      (match-fail (condition)
                                  (fail-match object "~S ~A" name (match-fail-message condition)))))))))


(defclass sequence-pattern (pattern)
  ())

(defun seq-pattern (&rest args)
  (make-instance 'sequence-pattern :args args))

(defmethod match ((pattern sequence-pattern) object)
  (unless (typep object 'sequence)
    (fail-match object "Not a sequence"))
  (let ((args (pattern-args pattern))
        (len (length object)))
    (unless (= len (length args))
      (fail-match object "Wrong length ~D /= ~D" len (length args)))
    (let ((i 0))
      (handler-case
        (map nil (lambda (pattern-element object-element)
                   (match pattern-element object-element)
                   (incf i))
             args object)
        (match-fail (condition)
                    (fail-match object "[~d] ~A" i (match-fail-message condition)))))))


;;;; Struct Pattern

(defclass struct-pattern (pattern)
  ())

(defun struct-pattern (&rest args)
  (make-instance 'struct-pattern :args args))

(defmethod match ((pattern struct-pattern) object)
  (unless (typep object 'CORBA:struct)
    (fail-match object "Not a struct"))
  (call-next-method))

(defun struct-class-name (struct)
  (class-name (class-of struct)))

#|
(defvar *def* (repository-from-string "typedef string foo;"))

(match (make-instance 'repository-pattern
         :args (list "foo" (def-pattern :dk_alias
                             'op:name "foxo"
                             'op:absolute_name "::foo"
                             'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_string))))
 
       *def*)

|#



(defmacro define-idl-test (name idl &rest pattern)
  `(define-test ,name 
     (let ((repository (repository-from-string ,idl)))
       (handler-case 
         (match (make-instance 'repository-pattern :args (list ,@pattern))
                repository)
         (match-fail (c) (tc-report "~A" (match-fail-message c)))))))


#|
(repository-from-string
 "union u switch(boolean) {
   case true: long x;
   case false: unsigned long y;
};"

)
|#