(in-package :clorb)

(defvar *test-suite-debug* nil)

#|
(setq *test-suite-debug* t)
|#

(defgeneric add-error (self)
  (:method ((self null))))

(defgeneric start-test-case (self &optional name)
  (:method ((self null) &optional name) (declare (ignore name))))

(defgeneric end-test-case (self)
  (:method ((self null))))

(defclass test-result ()
  ((suite :initarg :suite
          :reader result-suite)
   (current :initform nil
            :accessor current-test-case)
   (count :initform 0
          :accessor result-count)
   (errors :initform 0
           :accessor result-errors)
   (parent :initarg :parent
           :reader result-parent)))

(defvar *test-suite-result* nil)

(defmethod add-error ((self test-result))
  (incf (result-errors self))
  (add-error (result-parent self)))

(defmethod start-test-case ((self test-result) &optional name)
  (incf (result-count self))
  (when name
    (setf (current-test-case self) name))
  (start-test-case (result-parent self)))

(defmethod end-test-case ((self test-result)))

(defmethod print-result ((self test-result))
  (format t "~&;;; ------------- Suite '~A' result -----------------~%"
          (result-suite self))
  (format t ";;; ~D tests executed with ~D errors~%"
          (result-count self)
          (result-errors self)))


(defmacro define-test-suite (name &body body)
  (let ((vars nil))
    (when (and (consp (first body))
               (eq 'variables (car (first body))))
      (setq vars (cdr (pop body))))
    `(eval-when (:load-toplevel :execute)
       (let ((*test-suite-result* (make-instance 'test-result
                                    :suite ,name
                                    :parent *test-suite-result*))
             (tc-current nil))
         (labels
           ((tc-report (msg &rest args)
              (add-error *test-suite-result*)
              (format t "~&;;; In test case ~A~%;;;! ~A~%" 
                      tc-current
                      (apply #'cl:format nil msg args))
                (when *test-suite-debug*
                  (break "Failed ensure")))
            (ensure-eql (is shouldbe)
              (unless (eql is shouldbe)
                (tc-report "~S~_ should be~_ ~S"
                           is shouldbe)))
            (ensure-equalp (is shouldbe)
              (unless (equalp is shouldbe)
                (tc-report "~S~_ should be~_ ~S"
                           is shouldbe)))
            (ensure-typep (obj type)
              (unless (typep obj type)
                (tc-report "~S shoubd be of type ~S, but is ~S"
                           obj type (type-of obj))))
            (ensure (bool &optional description)
              (unless bool
                (tc-report "~A fail"
                           (or description "ensure"))))
            (ensure-pattern (obj pattern)
              (handler-case 
                (match pattern obj)
                (match-fail (c) (tc-report "~A" (match-fail-message c)))))
            (ensure-typecode (obj type)
              (unless (typep obj 'CORBA:TypeCode)
                (tc-report "~S should be a TypeCode" obj))
              (typecase type
                (symbol
                 (unless (eql (op:kind obj) type)
                   (tc-report "Wrong kind of typecode ~A. Should be ~A."
                              obj type)))
                (CORBA:Typecode
                 (unless (op:equal obj type)
                   (tc-report "Typecode ~A should be equal to ~A."
                              obj type))))))
           
           
           (macrolet ((define-test (name &body body)
                        `(let* ,',vars
                           (declare (ignorable . ,',(mapcar #'car vars)))
                           (block nil
                             (handler-bind ((error (lambda (exc) 
                                                     (tc-report "Exception ~A" exc)
                                                     (unless *test-suite-debug*
                                                       (return)))))
                               (progn
                                 (setq tc-current ,(string name))
                                 (start-test-case *test-suite-result* ,(string name))
                                 ,@body
                                 (end-test-case *test-suite-result*)))))))
             ,@body
             (print-result *test-suite-result*)))))))


(defvar *temporary-suite-variables* nil)

(defmacro define-test (name &body body)
  `(define-test-suite "Temporary Suit"
     (variables ,@ *temporary-suite-variables*)
     (define-test ,name ,@body)))
  