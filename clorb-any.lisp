;;;; CORBA:Any handling

(in-package :clorb)

(defclass CORBA:Any ()
  ((typecode :initarg :typecode )
   (value    :initarg :value    )))

(defgeneric any-typecode (obj))

(defgeneric any-value (obj))


(defun CORBA:Any (&key any-typecode any-value)
  (make-instance 'CORBA:Any
   :typecode (or any-typecode (any-typecode any-value))
   :value any-value))


(defmethod initialize-instance :after ((any CORBA:Any) &key &allow-other-keys)
  (unless (slot-boundp any 'typecode)
    (setf (slot-value any 'typecode)
      (any-typecode (slot-value any 'value)))))



(defmethod any-typecode ((obj CORBA:Any))
  (slot-value obj 'typecode))

(defmethod any-value ((obj CORBA:Any))
  (slot-value obj 'value))

(defmethod (setf any-value) (val (obj CORBA:Any))
  (setf (slot-value obj 'value) val))

(defmethod (setf any-typecode) (val (obj CORBA:Any))
  (setf (slot-value obj 'typecode) val))


;;; TypeCode accessor

(defmethod any-typecode ((obj t))
  (etypecase obj
    (corba:char      corba:tc_char   )
    (corba:wchar     corba:tc_wchar)
    (CORBA:short     CORBA:tc_short)
    (CORBA:ushort    CORBA:tc_ushort)
    (CORBA:long      CORBA:tc_long)
    (CORBA:ulong     CORBA:tc_ulong)
    (CORBA:longlong  CORBA:tc_longlong)
    (CORBA:ulonglong CORBA:tc_ulonglong)
    (corba:float     CORBA:tc_float)
    (corba:double    CORBA:tc_double)
    #|(corba:longdouble CORBA:tc_longdouble)|#
    (corba:boolean   corba:tc_boolean)))


(defun MEMBER-TYPECODE (sequence)
  (let ((max-num 0)
        (min-num 0)
        (non-string nil)
        (non-integer nil))
    (doseq (el sequence)
           (setf non-string (or non-string (not (stringp el))))
           (cond ((integerp el)
                  (setf max-num (max max-num el)
                        min-num (min min-num el)))
                 (t (setf non-integer t))))
    (cond ((not non-string)
           corba:tc_string)
          ((not non-integer)
           (if (< min-num 0)
               (any-typecode (- (max (- min-num) max-num)))
             (any-typecode max-num)))
          (t corba:tc_any))))


(defmethod any-typecode ((obj array))
  (create-array-tc (length obj) (member-typecode obj)))

(defmethod any-typecode ((obj list))
  (create-sequence-tc 0 (member-typecode obj)))

(defmethod any-typecode ((obj string))
  CORBA:tc_string)


;;; Value accessor

(defmethod any-value ((obj number))
  obj)

(defmethod any-value ((obj string))
  obj)

(defmethod any-value ((obj character))
  obj)

(defmethod any-value ((obj symbol))
  ;; ENUM
  obj)

(defmethod any-value ((obj sequence))
  obj)

(defmethod any-value ((obj t))
  (error 'CORBA:BAD_PARAM))

