;;;; clorb-struct.lisp -- CORBA Structure support

(in-package :clorb)

(defclass CORBA:struct ()
  ())

(defun create-struct-tc (id name members)
  (check-type id string)
  (check-type name string)
  (make-typecode :tk_struct id name
                 (coerce members 'vector)))

(defmethod any-typecode ((struct CORBA:struct))
  (symbol-typecode (class-name (class-of struct))))

(defmethod any-value ((struct CORBA:struct))
  struct)


(defgeneric type-id (struct))
(defgeneric fields (struct))


;; old:
#+unused-defuns
(defun struct-typecode (id name &rest fields)
  (make-typecode :tk_struct
                 id
                 (string (or name ""))
                 (coerce (loop for (name type) on fields by #'cddr
                             collect (list (string name) type))
                         'vector)))

;;;; Generic struct

(defclass generic-struct (CORBA:struct)
  ((typecode :initarg :typecode :reader generic-struct-typecode)
   (fields  :initarg :fields  :accessor fields)))

(defmethod type-id ((struct generic-struct))
  (op:id (generic-struct-typecode struct)))

(defmethod any-typecode ((struct generic-struct))
  (generic-struct-typecode struct))


(defmethod print-object ((obj CORBA:struct) stream)
  (cond (*print-readably*
         (format stream "#.(~S~:{ ~S '~S~})"
                 (class-name (class-of obj))
                 (mapcar (lambda (x) (list (car x) (cdr x)))
                         (fields obj))))
        (*print-pretty*
         (let ((fields (map 'list (lambda (pair) (list (car pair) (cdr pair)))
                            (fields obj))))
           (pprint-logical-block (stream fields
                                         :prefix "#<" :suffix ">")
             (pprint-indent :block 4)
             (typecase obj
               (generic-struct (princ (type-id obj) stream))
               (t (princ (type-of obj) stream)))
             (format stream "~{ ~_~{~W ~W~}~}" fields) )))
        (t
         (print-unreadable-object (obj stream :type t)
           (format stream "~{~S ~S~^ ~}"
                   (loop for (k . v) in (fields obj)
                         collect k collect v))))))


(defun make-generic-struct (typecode fields)
  (make-instance 'generic-struct
    :typecode typecode
    :fields (loop for (key val) on fields by #'cddr collect (cons key val))))


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


(defun map-struct (fn struct)
  (let* ((tc (any-typecode struct))
         (keys (tc-keywords tc)))
    (apply #'make-struct tc
           (loop for key across keys
                 collect key
                 collect (funcall fn (struct-get struct key))))))


(defmethod struct-get ((struct generic-struct) (field symbol))
  (cdr (assoc field (fields struct))))

(defmethod struct-get ((struct CORBA:struct) (field string))
  (struct-get struct (key field)))

#+unused-functions
(defun default-from-type (typecode)
  ;; FIXME: similary to arbritary-value
  (ecase (typecode-kind typecode)
    ((:tk_ushort :tk_short :tk_ulong :tk_long :tk_float :tk_double
      :tk_octet :tk_longlong :tk_ulonglong :tk_longdouble)
     0)
    ((:tk_boolean) nil)
    ((:tk_char) #\Space)
    ((:tk_string) "")
    ((:tk_sequence) nil)
    ((:tk_objref) nil)))


;; more marshalling support

(defgeneric struct-read (symbol buffer))
(defgeneric struct-write (obj symbol buffer))

(defmethod struct-read ((symbol symbol) buffer)
  (unmarshal-struct-2 symbol (symbol-typecode symbol) buffer))

(defun unmarshal-struct (tc buffer)
  (let* ((id (op:id tc))
         (symbol (ifr-id-symbol id)))
    (if symbol 
      (struct-read symbol buffer)
      (unmarshal-struct-2 symbol tc buffer))))

(defun unmarshal-struct-2 (symbol tc buffer)
  (let* ((constructor
          (if symbol 
            symbol
            (lambda (&rest fields) (make-generic-struct tc fields)))))
    (apply constructor
           (loop for key across (tc-keywords tc) 
                 for (nil tc) across (tc-members tc)
                 collect key
                 collect (unmarshal tc buffer)))))


(defmethod struct-write (obj (symbol symbol) buffer)
  (marshal-struct obj (symbol-typecode symbol) buffer))

(defun marshal-struct (struct typecode buffer)
  (loop for (nil tc) across (tc-members typecode)
        for name across (tc-keywords typecode)
        do (marshal (struct-get struct name) tc buffer)))



;;; clorb-struct.lisp ends here
