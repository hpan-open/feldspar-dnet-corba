;;;; clorb-struct.lisp -- CORBA Structure support

(in-package :clorb)

(defclass CORBA:STRUCT ()
  ())


(define-typecode struct-typecode
  :kind :tk_struct
  :cdr-syntax (complex :tk_string :tk_string
                       (sequence (:tk_string :tk_typecode)))
  :params (id name :members)
  :member-params (member_name member_type)
  :share named-typecode :shared-params 2
  :extra-slots (member-types feature-symbols))


(defun create-struct-tc (id name members)
  (check-type id string)
  (check-type name string)
  (check-type members sequence)
  (doseq (m members)
    (check-type m (cons string (cons CORBA:TypeCode null))))
  (make-typecode :tk_struct id name (coerce members 'vector)))


(defmethod any-typecode ((struct corba:struct))
  (symbol-typecode (class-name (class-of struct))))

(defmethod any-value ((struct corba:struct))
  struct)


(defgeneric type-id (struct)
  (:method ((struct CORBA:struct))
           (symbol-ifr-id (class-name (class-of struct)))))

(defgeneric fields (struct))

(defmethod raw-fields ((struct corba:struct))
  (loop for (key . val) in (fields struct)
        collect key collect val))

(defmethod struct-get ((struct corba:struct) (field symbol))
  (funcall (feature (symbol-name field)) struct))

(defmethod struct-get ((struct corba:struct) (field string))
  (funcall (feature field) struct))


;;;; Generic struct


(defclass GENERIC-STRUCT (corba:struct)
  ((typecode :initarg :typecode :reader generic-struct-typecode)
   (fields  :initarg :fields  :accessor raw-fields)))

(defmethod fields ((struct GENERIC-STRUCT))
  (loop for (key val) on (raw-fields struct) by #'cddr
        collect (cons key val)))


(defmethod type-id ((struct generic-struct))
  (op:id (generic-struct-typecode struct)))

(defmethod any-typecode ((struct generic-struct))
  (generic-struct-typecode struct))


(defun make-generic-struct (typecode fields)
  (make-instance 'generic-struct
    :typecode typecode :fields fields))


(defmethod struct-get ((struct generic-struct) (field symbol))
  (getf (raw-fields struct) field))

(defmethod struct-get ((struct generic-struct) (field string))
  (struct-get struct (key field)))



;;;; Struct creation and printing


(defun make-struct (typecode &rest nv-pairs)
  "Make a CORBA structure of type.
NV-PAIRS is a list field names and field values.
If ID is nil, then all fields must be supplied. Otherwise some types
of fields can be defaulted (numbers and strings)."
  (let* ((id (op:id typecode))
         (class (ifr-id-symbol id)))
    (if class
        (apply #'make-instance class nv-pairs)
        (make-generic-struct typecode nv-pairs))))


(defmethod print-object ((obj corba:struct) stream)
  (cond (*print-readably*
         (format stream "#.(~S~{ ~S '~S~})"
                 (class-name (class-of obj))
                 (raw-fields obj)))
        (*print-pretty*
         (let ((fields (raw-fields obj)))
           (pprint-logical-block (stream fields
                                         :prefix "#<" :suffix ">")
             (pprint-indent :block 4)
             (typecase obj
               (generic-struct (princ (type-id obj) stream))
               (t (princ (type-of obj) stream)))
             (format stream "~{ ~_~W ~W~}" fields) )))
        (t
         (print-unreadable-object (obj stream :type t)
           (format stream "~{~S~^ ~}" (raw-fields obj))))))


(defun map-struct (fn struct)
  (let* ((tc (any-typecode struct))
         (keys (tc-keywords tc)))
    (apply #'make-struct tc
           (loop for key across keys
                 collect key
                 collect (funcall fn (struct-get struct key))))))



;;;; Struct marshalling


(defgeneric struct-read (symbol buffer))

(defgeneric struct-write (obj symbol buffer))


(defmethod struct-read ((symbol symbol) buffer)
  (unmarshal-struct-2 symbol (symbol-typecode symbol) buffer))


(defmethod unmarshal ((tc struct-typecode) buffer)
  (let* ((symbol (typecode-symbol tc)))
    (unmarshal-struct-2 symbol tc buffer)))


(defun unmarshal-struct-2 (symbol tc buffer)
  (let* ((constructor
          (if symbol 
            symbol
            (lambda (&rest fields) (make-generic-struct tc fields)))))
    (apply constructor
           (loop for key across (tc-keywords tc) 
                 for member-tc in (tc-member-types tc)
                 collect key
                 collect (unmarshal member-tc buffer)))))


(defmethod compute-unmarshal-function ((tc struct-typecode))
  (let ((constructor (typecode-symbol tc)))
    (if (null constructor)
      (lambda (buffer) (unmarshal tc buffer))
      (let ((keys (tc-keywords tc))
            (unmarshallers
             (loop for i from 0 below (op:member_count tc)
                   collect (unmarshal-function (op:member_type tc i)))))
        (case (length keys)
          (2 (let ((k1 (elt keys 0))
                   (k2 (elt keys 1))
                   (m1 (first unmarshallers))
                   (m2 (second unmarshallers)))
               (lambda (buffer)
                 (funcall constructor
                          k1 (funcall m1 buffer)
                          k2 (funcall m2 buffer)))))
          (t (lambda (buffer)
               (apply constructor
                      (loop for key across keys and fun in unmarshallers
                            collect key collect (funcall fun buffer))))))))))


(defmethod struct-write (obj (symbol symbol) buffer)
  (marshal obj (symbol-typecode symbol) buffer))


(defmethod marshal (struct (typecode struct-typecode) buffer)
  (loop for tc in (tc-member-types typecode)
        for feature in (tc-feature-symbols typecode)
        do (marshal (slot-value struct feature) tc buffer)))

(defmethod marshal ((struct generic-struct) (typecode struct-typecode) buffer)
  (loop for tc in (tc-member-types typecode)
        for name across (tc-keywords typecode)
        do (marshal (struct-get struct name) tc buffer)))


(defmethod compute-marshal-function ((tc struct-typecode))
  (let ((features nil)
        (marshallers nil))
    (loop for i from 0 below (op:member_count tc)
          do (push (feature (op:member_name tc i)) features)
          (push (marshal-function (op:member_type tc i)) marshallers))
    (setf features (mapcar #'fdefinition (nreverse features))
          marshallers (nreverse marshallers))
    (case (length features)
      (2 (let ((f1 (first features))
               (f2 (second features))
               (m1 (first marshallers))
               (m2 (second marshallers)))
           (lambda (struct buffer)
             (funcall m1 (funcall f1 struct) buffer)
             (funcall m2 (funcall f2 struct) buffer))))
      (t (lambda (struct buffer)
           (loop for accssor in features and marshal in marshallers
                 do (funcall marshal (funcall accssor struct) buffer)))))))


;;; clorb-struct.lisp ends here
