;;;; pattern.lisp -- pattern matcher for testing 

(in-package :clorb)

;;;; match-fail condition

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


;;;; Basic pattern

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


;;;; IR Definition Pattern

(defclass def-pattern (pattern)
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

(defclass repository-pattern (pattern)
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


;;;; Sequence Pattern

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



;;;; Sexp pattern

(defclass sexp-pattern (pattern)
  ())

(defun sexp-pattern (sexp)
  (make-instance 'sexp-pattern :args sexp))

(defmethod match ((pattern sexp-pattern) object)
  (match-sexp (pattern-args pattern) object))

(defun match-sexp (pattern object)
  (if (consp pattern)
    (progn
      (unless (consp object)
        (fail-match object "not a cons"))
      (case (car pattern)
        (&key (match-keys (cdr pattern) object))
        (&any (match-sexp (cdr pattern) (cdr object)))
        (&rest (map nil (lambda (obj) (match-sexp (second pattern) obj)) object))
        (&any-rest)
        (otherwise
         (progn (match-sexp (car pattern) (car object))
                (match-sexp (cdr pattern) (cdr object))))))
    (match pattern object)))
  
(defun match-keys (keys list)
  (let ((seen '()))
    (when (oddp (length list))
      (fail-match list "odd key-value list"))
    (loop for (key value) on list by #'cddr
          do (let ((spec (assoc key keys)))
               (unless spec (fail-match key "illegal key"))
               (when (member key seen) (fail-match key "duplicated key"))
               (push key seen)
               (when (cddr spec)
                 (match-sexp (caddr spec) value))))
    (loop for (key option) in keys
          when (eq option :required)
          do (unless (member key seen)
               (fail-match list "missing required key ~S" key)))))


;;;; Eval-To pattern

(defclass eval-to-pattern (pattern)
  ())

(defun eval-to (form)
  (make-instance 'eval-to-pattern :args form))

(defmethod match ((pattern eval-to-pattern) object)
  (match (pattern-args pattern)
         (eval object)))





#|
(defvar *def* (repository-from-string "typedef string foo;"))
(match (make-instance 'repository-pattern
         :args (list "foo" (def-pattern :dk_alias
                             'op:name "foxo"
                             'op:absolute_name "::foo"
                             'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_string))))
       *def*)
|#
