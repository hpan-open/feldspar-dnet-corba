;;;; clorb-struct.lisp -- CORBA Structure support
;; $Id: clorb-struct.lisp,v 1.9 2002/05/30 06:11:11 lenst Exp $

(in-package :clorb)

(defvar *specialized-structs*
  (make-hash-table :test #'equal))

(defclass CORBA:struct ()
  ())

;; This is a bit dubious, but just for testing...
(define-slot-dumper CORBA:struct)

(defgeneric type-id (struct))
(defgeneric fields (struct))


;;;; Generic struct

(defclass generic-struct (CORBA:struct)
  ((type-id :initarg :type-id :reader type-id)
   (fields  :initarg :fields  :accessor fields)))

(defmethod print-object ((obj CORBA:struct) stream)
  (if *print-readably*
      (format stream "#.(CLORB::MAKE-STRUCT ~S~:{ ~S '~S~})"
              (type-id obj)
              (mapcar (lambda (x) (list (car x) (cdr x)))
                      (fields obj)))
    (print-unreadable-object (obj stream :type t)
      (format stream "~A~:{~_ ~A=~S~}"
              (type-id obj)
              (map 'list (lambda (pair) (list (car pair) (cdr pair)))
                   (fields obj))))))



;; Interface:
(defun make-struct (id-or-typecode &rest nv-pairs)
  "Make a CORBA structure of type ID.
NV-PAIRS is a list field names and field values.
If ID is nil, then all fields must be supplied. Otherwise some types
of fields can be defaulted (numbers and strings)."
  (let ((class (gethash id-or-typecode *specialized-structs*)))
    (if class
        (apply #'make-instance class nv-pairs)
      (let* ((typecode (if (stringp id-or-typecode)
                           (get-typecode id-or-typecode)
                         id-or-typecode))
             (fields
              (multiple-value-bind (kind params)
                  (type-expand typecode)
                (assert (eq kind :tk_struct))
                (map 'list (lambda (nv)
                             (let* ((fname (lispy-name (first nv)))
                                    (val (getf nv-pairs fname nv)))
                               (cons fname
                                     (if (eq val nv)
                                         (default-from-type (second nv))
                                       val))))
                     (tcp-members params)))))
        (make-instance 'generic-struct
          :type-id (op::id typecode)
          :fields fields)))))


(defun make-struct-internal (id nv-alist &optional nv-plist)
  "Make a CORBA structure of type ID.
NV-PAIRS is a list field names and field values."
  (let ((class (gethash id *specialized-structs*)))
    (if class
        (apply #'make-instance class
               (or nv-plist
                   (mapcan (lambda (x) (list (car x) (cdr x)))
                           nv-alist)))
      (make-instance 'generic-struct
        :type-id id
        :fields (or nv-alist
                    (loop for (name val) on nv-plist by #'cddr
                        collect (cons name val)))))))



;; Interface:
(defmethod struct-get ((struct generic-struct) (field symbol))
  (cdr (assoc field (fields struct))))

(defmethod struct-get ((struct CORBA:struct) (field string))
  (struct-get struct (lispy-name field)))

(defun struct-typecode (id name &rest fields)
  (make-typecode :tk_struct
                 id
                 (string (or name ""))
                 (coerce (loop for (name type) on fields by #'cddr
                             collect (list (string name) type))
                         'vector)))


(defmethod typecode ((obj CORBA:struct))
  (get-typecode (type-id obj)))


(defun default-from-type (typecode)
  (ecase (typecode-kind typecode)
    ((:tk_ushort :tk_short :tk_ulong :tk_long :tk_float :tk_double
      :tk_octet :tk_longlong :tk_ulonglong :tk_longdouble)
     0)
    ((:tk_boolean) nil)
    ((:tk_char) #\Space)
    ((:tk_string) "")
    ((:tk_sequence) nil)
    ((:tk_objref) nil)))


;;;; Specialized structs

;; Define methods for:
;; type-id, fields, struct-get

;;; registry
(defun add-struct-class (class id)
  (unless id
    (mess 3 "Null ID to add-struct-class")
    (setq id (type-id (make-instance class))))
  (setf (gethash id *specialized-structs*) class))


;;; Macrology

(defmacro define-corba-struct (name &key id members)
  (loop
      for member in members
      for slot = (car member)
      for field = (lispy-name (symbol-name slot))
      collect field into names
      collect slot into slots
      collect (list* slot :initarg field :initform (second member)
                     (cddr member))
      into slot-defs
      collect `(defmethod struct-get ((s ,name) (field (eql ,field)))
                 (slot-value s ',slot))
      into getters1
      collect `(define-method ,slot ((s ,name)) (slot-value s ',slot))
      into getters2
      collect `(define-method (setf ,slot) (val (s ,name))
                 (setf (slot-value s ',slot) val))
      into setters
      finally
        (return
          `(progn
             (defclass ,name (CORBA:struct) ,slot-defs)
             (defun ,name (&rest initargs)
               ,(format nil "Construct CORBA struct ~A.~%Slots: ~S" name names)
               (apply #'make-instance ',name initargs))
             (defmethod type-id ((s ,name)) ,id)
             ,@getters1 ,@getters2 ,@setters
             (defmethod fields ((s ,name))
               (loop for f in ',names
                   for n in ',slots
                   when (slot-boundp s n)
                   collect (cons f (slot-value s n))))
             (add-struct-class ',name ,id)))))


;;; clorb-struct.lisp ends here
