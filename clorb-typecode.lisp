;;;; Type codes

(in-package :clorb)


(defparameter TCKind
  '#(:tk_null :tk_void :tk_short :tk_long :tk_ushort :tk_ulong
     :tk_float :tk_double :tk_boolean :tk_char
     :tk_octet :tk_any :tk_TypeCode :tk_Principal :tk_objref
     :tk_struct :tk_union :tk_enum :tk_string
     :tk_sequence :tk_array :tk_alias :tk_except
     :tk_longlong :tk_ulonglong :tk_longdouble
     :tk_wchar :tk_wstring :tk_fixed :tk_value :tk_value_box
     :tk_native :tk_abstract_interface) 
  "The symbols for the TCKind enum")

(deftype CORBA:TCKind ()
  (cons 'member (coerce TCKind 'list)))


(defclass CORBA:TypeCode ()
  ((kind :initarg :kind)
   (params :initarg :params)))

(define-slot-dumper CORBA:TypeCode)

(defun print-typecode (typecode &optional (stream *standard-output*))
  (print-unreadable-object (typecode stream :type t :identity t)
    (format stream "~S~{~_ ~S~}" 
            (slot-value typecode 'kind) (slot-value typecode 'params))))

(defmethod print-object ((tc CORBA:TypeCode) stream)
  (cond
   (*print-readably*
    (format stream "#.(CLORB::MAKE-TYPECODE '~S~{ '~S~})"
            (slot-value tc 'kind) (slot-value tc 'params)))
   (t
    (print-typecode tc stream))))


(eval-when (load eval)
  (loop for i from 0 below (length TCKind)
      do (setf (get (elt TCKind i) 'tk-value) i)))

(defmacro put (sym key val)
  `(setf (get ,sym ,key) ,val))




(put ':tk_fixed 'tk-params '(:tk_ushort :tk_short))

(put ':tk_objref 'tk-params '(complex :tk_string :tk_string))

(put ':tk_struct 'tk-params 
     '(complex :tk_string :tk_string
       (sequence (anon-struct :tk_string :tk_TypeCode))))

(put ':tk_union 'tk-params 
     '(complex 
       :tk_string                       ; Repository id
       :tk_string                       ; name
       :tk_TypeCode                     ; discriminant-type
       :tk_long                         ; default index
       #| Members: (label name typecode)* |#))

(put ':tk_enum 'tk-params 
     '(complex
       :tk_string                       ; Repository id
       :tk_string                       ; name
       (sequence                        ; members
        :tk_string)))

(put ':tk_sequence 'tk-params '(complex :tk_TypeCode :tk_ulong))

(put ':tk_string 'tk-params '(:tk_ulong))

(put ':tk_wstring 'tk-params '(:tk_ulong))

(put ':tk_array 'tk-params '(complex :tk_TypeCode :tk_ulong))

(put ':tk_alias 'tk-params '(complex :tk_string :tk_string :tk_TypeCode))

(put ':tk_except 'tk-params '(complex string string
                              (sequence (anon-struct string :tk_TypeCode))))


(put ':tk_value 'tk-params 
     '(complex :tk_string :tk_string :tk_short :tk_TypeCode 
       (sequence (anon-struct :tk_string :tk_TypeCode :tk_short))))

(put ':tk_value_box 'tk-params '(complex :tk_string :tk_string :tk_TypeCode))

(put ':tk_native 'tk-params '(complex :tk_string :tk_string))

(put ':tk_abstract_interface 'tk-params '(complex :tk_string :tk_string))



;;; TypeCode Parameters
;; Kind         p0              p1              p2              p3
;; :tk_objref   id              name      
;; :tk_struct   id              name            members
;; :tk_except   id              name            members
;; :tk_union    id              name            :tk_TypeCode    :tk_long
;; :tk_alias    id              name            :tk_TypeCode
;; :tk_enum     id              name            members
;; :tk_sequence content_type    max      
;; :tk_string   max      
;; :tk_wstring  max
;; :tk_array  
;; :tk_fixed    :tk_ushort      :tk_short

;;; Accessors for TypeCode parameters

(defun tcp-id (params) (first params))
(defun tcp-name (params) (second params))
(defun tcp-members (params) (third params))
(defun tcp-content-type (params) (first params))
(defun tcp-max (params) (car (last params)))


;;;(declaim (inline make-typecode typecode-kind typecode-params))

(defun make-typecode (kind &rest params)
  (make-instance 'corba:typecode 
    :kind kind :params params))

(defun typecode-kind (tc)
  (slot-value tc 'kind))

(defun typecode-params (tc)
  (slot-value tc 'params))

(defun (setf typecode-params) (params tc)
  (setf (slot-value tc 'params) params))

(defun type-expand (type)
  (typecase type
    (symbol (values type nil))
    (cons   (values (car type) (cdr type)))
    (t      (values (typecode-kind type) (typecode-params type)))))

(defun lispy-name (string)
  (cond ((symbolp string)
	 string)
	(t
         (read-from-string (concatenate 'string ":" string)))))

(macrolet
    ((make-tc-constants (&rest specs)
       `(progn
          ,@(loop for (tc-name tk-name . tc-params) in specs
                collect
                  `(defparameter ,tc-name
                       (make-typecode ,tk-name 
                                      ,@(mapcar (lambda (p) (list 'quote p))
                                                tc-params )))))))
  (make-tc-constants
   (CORBA:tc_null :tk_null)
   (CORBA:tc_void :tk_void)
   (CORBA:tc_short :tk_short)
   (CORBA:tc_long :tk_long)
   (CORBA:tc_ushort :tk_ushort)
   (CORBA:tc_ulong :tk_ulong)
   (CORBA:tc_float :tk_float)
   (CORBA:tc_double :tk_double)
   (CORBA:tc_boolean :tk_boolean)
   (CORBA:tc_char :tk_char)
   (CORBA:tc_octet :tk_octet)
   (CORBA:tc_any :tk_any)
   (CORBA:tc_typecode :tk_typecode)
   ;;(CORBA:tc_principal :tk_principal)
   (CORBA:tc_objref :tk_objref "")
   ;;(CORBA:tc_struct :tk_struct)
   ;;(CORBA:tc_union :tk_union)
   ;;(CORBA:tc_enum :tk_enum)
   (CORBA:tc_string :tk_string 0)
   ;;(CORBA:tc_sequence :tk_sequence)
   ;;(CORBA:tc_array :tk_array)
   ;;(CORBA:tc_alias :tk_alias)
   ;;(CORBA:tc_except :tk_except)
   (CORBA:tc_longlong :tk_longlong)
   (CORBA:tc_ulonglong :tk_ulonglong)
   (CORBA:tc_longdouble :tk_longdouble)
   (CORBA:tc_wchar :tk_wchar)
   (CORBA:tc_wstring :tk_wstring 0)
   ;;(CORBA:tc_fixed :tk_fixed)
   ))

(defparameter CORBA::tc_completion_status
    (make-typecode :tk_enum
                   "IDL:omg.org/CORBA/completion_status:1.0"
                   "completion_status"
                   '#("COMPLETED_YES" "COMPLETED_NO" "COMPLETED_MAYBE")))


;;;; PIDL interface to TypeCode

;; exception Bounds
(define-condition corba:typecode/bounds (corba:userexception) ())

;; exception BadKind
(define-condition corba:typecode/badkind (corba:userexception) ())


(define-method equal ((tc1 corba:typecode) tc2)
  (or (eq tc1 tc2)
      (and (eq (typecode-kind tc1) (typecode-kind tc2))
           (tcp-equal (typecode-params tc1)
                      (typecode-params tc2)))))

(defun tcp-equal (tcp1 tcp2)
  (or (eq tcp1 tcp2)
      (every (lambda (p1 p2)
               (if (vectorp p1)
                   (every #'equalp p1 p2)
                 (equalp p1 p2)))
             tcp1
             tcp2)))

(define-method kind ((tc corba:typecode))
  (typecode-kind tc))

(define-method id ((tc corba:typecode))
  (case (typecode-kind tc)
    ((:tk_objref :tk_struct :tk_union :tk_alias :tk_except :tk_enum)
     (tcp-id (typecode-params tc)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method name ((tc corba:typecode))
  (case (typecode-kind tc)
    ((:tk_objref :tk_struct :tk_union :tk_alias :tk_except :tk_enum)
     (tcp-name (typecode-params tc)))
    (otherwise
     (error 'corba:typecode/badkind))))

(defun tc-members (tc)
  (case (typecode-kind tc)
    ((:tk_struct :tk_except :tk_enum)
     (third (typecode-params tc)))
    (:tk_union
     (fifth (typecode-params tc)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method member_count ((tc corba:typecode))
  (length (tc-members tc)))

(define-method member_name ((tc corba:typecode) index)
  (case (typecode-kind tc)
    ((:tk_enum)
     (elt (tc-members tc) index))
    ((:tk_struct :tk_except)
     (first (elt (tc-members tc) index)))
    ((:tk_union)
     (second (elt (fifth (typecode-params tc)) index)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method member_type ((tc corba:typecode) index)
  (case (typecode-kind tc)
    ((:tk_struct :tk_except)
     (second (elt (tc-members tc) index)))
    ((:tk_union)
     (third (elt (tc-members tc) index)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method member_label ((tc corba:typecode) index)
  (case (typecode-kind tc)
    ((:tk_union)
     (first (elt (tc-members tc) index)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method discriminator_type ((tc corba:typecode))
  (case (typecode-kind tc)
    ((:tk_union)
     (third (typecode-params tc)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method default_index ((tc corba:typecode))
  (case (typecode-kind tc)
    ((:tk_union)
     (fourth (typecode-params tc)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method length ((tc corba:typecode))
  (case (typecode-kind tc)
    ((:tk_string :tk_wstring)
     (or (first (typecode-params tc)) 0))
    ((:tk_sequence :tk_array)
     (or (second (typecode-params tc)) 0))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method content_type ((tc corba:typecode))
  (case (typecode-kind tc)
    ((:tk_sequence :tk_array)
     (first (typecode-params tc)))
    (:tk_string corba:tc_char)
    (:tk_wstring corba:tc_wchar)
    (otherwise
     (error 'corba:typecode/badkind))))


;;;; Accessing typecodes of defined types

(defun symbol-typecode (symbol)
  ;; Return the type code for the scoped symbol of an idltype.
  (let ((typecode (get symbol 'typecode)))
    (if (functionp typecode) 
      (setf (get symbol 'typecode) (funcall typecode))
      typecode)))

(defun set-symbol-typecode (symbol typecode)
  ;; Set the typecode for a scoped symbol. Typecode can also be a function to compute the typecode.
  (setf (get symbol 'typecode) typecode))

(defun symbol-ifr-id (symbol)
  ;; Return the interface repository id for the scoped symbol.
  (get symbol 'ifr-id))

(defun set-symbol-ifr-id (symbol id)
  (setf (get symbol 'ifr-id) id))


;;;; Convenience ?

(defun tcp-member-symbols (params)
  (map 'vector #'lispy-name (tcp-members params)))


;;;; Constructors

(defun MAKE-SEQUENCE-TYPECODE (member-type &optional (maxsize 0))
  (make-typecode :tk_sequence member-type maxsize))

(defun MAKE-ARRAY-TYPECODE (member-type size)
  (make-typecode :tk_array member-type size))

(defun make-tc-alias (id name typecode)
  (make-typecode :tk_alias id name typecode))
