;;;; Type codes

(in-package :clorb)


(defgeneric tc-members (tc))


(defmacro tcp-elt (x i)
  (case i
    (0 `(first (typecode-params ,x)))
    (1 `(second (typecode-params ,x)))
    (2 `(third (typecode-params ,x)))
    (3 `(fourth (typecode-params ,x)))
    (4 `(fifth (typecode-params ,x)))
    (otherwise `(elt (typecode-params ,x) ,i))))


(defmacro define-typecode (class-name &key kind cdr-syntax params member-params constant )
  `(progn
     (defclass ,class-name (CORBA:TypeCode)
       ())
     (setf (get ',kind 'tk-params) ',cdr-syntax)
     (setf (get ',kind 'tk-class) ',class-name)
     ,@(loop for param in params
             for i from 0
             collect (if (eq param :members)
                       `(defmethod tc-members ((tc ,class-name))
                          (tcp-elt tc ,i))
                       `(define-method ,param ((tc ,class-name))
                          (tcp-elt tc ,i))))
     ,@(cond ((consp member-params)
              (loop for mp in member-params 
                    for i from 0
                    collect `(define-method ,mp ((tc ,class-name) index)
                               (elt (elt (tc-members tc) index) ,i))))
             ((null member-params) nil)
             ((symbolp member-params)
              `((define-method ,member-params ((tc ,class-name) index)
                 (elt (tc-members tc) index)))))     
     ,@(if constant
         `((defparameter ,(if (consp constant) (car constant) constant)
             (make-typecode ,kind ,@(mapcar #'kwote (if (consp constant)
                                                      (cdr constant)))))))))





(define-typecode null-typecode
  :kind :tk_null
  :constant corba:tc_null)

(define-typecode void-typecode
  :kind :tk_void
  :constant corba:tc_void)

(define-typecode short-typecode
  :kind :tk_short
  :constant corba:tc_short)

(define-typecode long-typecode
  :kind :tk_long
  :constant corba:tc_long)

(define-typecode ushort-typecode
  :kind :tk_ushort
  :constant corba:tc_ushort)

(define-typecode ulong-typecode
  :kind :tk_ulong
  :constant corba:tc_ulong)

(define-typecode float-typecode
  :kind :tk_float
  :constant corba:tc_float)

(define-typecode double-typecode
  :kind :tk_double
  :constant corba:tc_double)

(define-typecode boolean-typecode
  :kind :tk_boolean
  :constant corba:tc_boolean)

(define-typecode longlong-typecode
  :kind :tk_longlong
  :constant corba:tc_longlong)

(define-typecode ulonglong-typecode
  :kind :tk_ulonglong
  :constant corba:tc_ulonglong)

(define-typecode longdouble-typecode
  :kind :tk_longdouble
  :constant corba:tc_longdouble)

(define-typecode wchar-typecode
  :kind :tk_wchar
  :constant corba:tc_wchar)

(define-typecode char-typecode
  :kind :tk_char
  :constant corba:tc_char)

(define-typecode octet-typecode
  :kind :tk_octet
  :constant corba:tc_octet)

(define-typecode any-typecode
  :kind :tk_any
  :constant corba:tc_any)

(define-typecode typecode-typecode
  :kind :tk_typecode
  :constant corba:tc_typecode)

(define-typecode fixed-typecode
  :kind :tk_fixed
  :cdr-syntax (:tk_ushort :tk_short)
  :params (fixed_digits fixed_scale))

(define-typecode objref-typecode
  :kind :tk_objref
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name)
  :constant (corba:tc_object "IDL:omg.org/CORBA/Object:1.0" "Object"))

(define-typecode struct-typecode
  :kind :tk_struct
  :cdr-syntax (complex :tk_string :tk_string
                       (sequence (:tk_string :tk_typecode)))
  :params (id name :members)
  :member-params (member_name member_type))


(define-typecode union-typecode
  :kind :tk_union
  :cdr-syntax (complex :tk_string :tk_string :tk_typecode :tk_long     
                       (sequence (2 :tk_string :tk_typecode)))
  :params (id name discriminator_type default_index :members)
  :member-params (member_label member_name member_type))


(define-typecode enum-typecode
  :kind :tk_enum
  :params (id name :members)
  :cdr-syntax (complex :tk_string :tk_string (sequence :tk_string))
  :member-params member_name)


(define-typecode sequence-typecode
  :kind :tk_sequence
  :cdr-syntax (complex :tk_typecode :tk_ulong)
  :params (content_type length))


(define-typecode string-typecode
  :kind :tk_string
  :cdr-syntax (:tk_ulong)
  :params (length)
  :constant (corba:tc_string 0))

(define-typecode wstring-typecode
  :kind :tk_wstring
  :cdr-syntax (:tk_ulong)
  :params (length)
  :constant (corba:tc_wstring 0))


(define-typecode array-typecode
  :kind :tk_array
  :cdr-syntax (complex :tk_typecode :tk_ulong)
  :params (content_type length))


(define-typecode alias-typecode
  :kind :tk_alias
  :cdr-syntax (complex :tk_string :tk_string :tk_typecode)
  :params (id name content_type))

(define-typecode except-typecode
  :kind :tk_except
  :cdr-syntax (complex :tk_string :tk_string (sequence (:tk_string :tk_typecode)))
  :params (id name :members)
  :member-params (member_name member_type))

(define-typecode value-typecode
  :kind :tk_value
  :cdr-syntax (complex :tk_string :tk_string :tk_short :tk_typecode 
                       (sequence (:tk_string :tk_typecode :tk_short)))
  :params (id name type_modifier concrete_base_type :members)
  :member-params (member_name member_type member_visibility)
  :constant (corba::TC_ValueBase "IDL:omg.org/CORBA/ValueBase:1.0" "ValueBase"
                                 CORBA::VM_NONE nil ()))

(define-typecode value_box-typecode
  :kind :tk_value_box
  :cdr-syntax (complex :tk_string :tk_string :tk_typecode)
  :params (id name content_type))

(define-typecode native-typecode
  :kind :tk_native
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name))

(define-typecode abstract_interface-typecode
  :kind :tk_abstract_interface
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name))

(define-typecode local_interface-typecode
  :kind :tk_local_interface
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name))







;;;; PIDL interface to TypeCode

;; exception Bounds
(define-condition corba:typecode/bounds (corba:userexception) ())

;; exception BadKind
(define-condition corba:typecode/badkind (corba:userexception) ())


(defmethod tc-members ((tc corba:typecode))
  (error 'corba:typecode/badkind))


(define-method id ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method name ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method member_count ((tc corba:typecode))
  (length (tc-members tc)))

(define-method member_name ((tc corba:typecode) index)
  (declare (ignore index))
  (error 'corba:typecode/badkind))

(define-method member_type ((tc corba:typecode) index)
  (declare (ignore index))
  (error 'corba:typecode/badkind))

(define-method member_label ((tc corba:typecode) index)
  (declare (ignore index))
  (error 'corba:typecode/badkind))

(define-method member_visibility ((tc corba:typecode) index)
  (declare (ignore index))
  (error 'corba:typecode/badkind))

(define-method discriminator_type ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method default_index ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method length ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method content_type ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method fixed_digits ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method fixed_scale ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method type_modifier ((tc corba:typecode))
  (error 'corba:typecode/badkind))

(define-method concrete_base_type ((tc corba:typecode))
  (error 'corba:typecode/badkind))


;;;; Constructors

(defun create-array-tc (size member-type)
  (make-typecode :tk_array member-type size))

(defun create-sequence-tc (maxsize member-type)
  (make-typecode :tk_sequence member-type maxsize))
(defun make-sequence-typecode (member-type &optional (maxsize 0))
  (create-sequence-tc maxsize member-type))

(defun create-fixed-tc (digits scale)
  (make-typecode :tk_fixed digits scale))

(defun create-wstring-tc (maxsize)
  (make-typecode :tk_wstring maxsize))

(defun create-string-tc (maxsize)
  (make-typecode :tk_string maxsize))

(defun create-interface-tc (id name)
  (make-typecode :tk_objref id name))

(defun create-alias-tc (id name typecode)
  (make-typecode :tk_alias id name typecode))

(defun create-enum-tc (id name members)
  (make-typecode :tk_enum id name (coerce members 'vector)))

(defun create-value-box-tc (id name typecode)
  (make-typecode :tk_value_box id name typecode))

(defun create-native-tc (id name)
  (make-typecode :tk_native id name))

(defun create-abstract-interface-tc (id name)
  (make-typecode :tk_abstract_interface id name))

(defun create-local-interface-tc (id name)
  (make-typecode :tk_local_interface id name))


;; ValueModifier constants
(DEFCONSTANT OMG.ORG/CORBA:VM_TRUNCATABLE (QUOTE 3))
(DEFCONSTANT OMG.ORG/CORBA:VM_ABSTRACT (QUOTE 2))
(DEFCONSTANT OMG.ORG/CORBA:VM_CUSTOM (QUOTE 1))
(DEFCONSTANT OMG.ORG/CORBA:VM_NONE (QUOTE 0))

(defun create-value-tc (id name type-modifier concrete-base members)
  (check-type id string)
  (check-type name string)
  (check-type type-modifier fixnum) ; ValueModifier
  (check-type concrete-base (or null CORBA:TypeCode))
  (check-type members sequence)
  (map nil (lambda (m)
             (check-type m 
                         (cons string 
                               (cons CORBA:TypeCode (cons fixnum null)))))
       members)
  (make-typecode :tk_value id name type-modifier concrete-base
                 (coerce members 'vector)))


;;;; Convenience ?

(defun tc-keywords (tc)
  (unless (slot-boundp tc 'keywords)
    (setf (slot-value tc 'keywords)
          (map 'vector 
               (ecase (typecode-kind tc)
                 ((:tk_struct :tk_except)
                  (lambda (m) (key (first m))))
                 ((:tk_union)
                  (lambda (m) (key (second m))))
                 ((:tk_enum)
                  #'key))
               (tc-members tc))))
  (slot-value tc 'keywords))


(defun tc-features (tc)
  ;; Return a vector of feature symbols for the members.
  ;; (FIXME: why vector?)
  ;; Works with struct and except.
  ;; Union ?
  ;; Enum - seems pointless (FIXME?)
  (map 'vector
       (ecase (typecode-kind tc)
         ((:tk_struct :tk_except)
          (lambda (m) (feature (first m))))
         ((:tk_union)
          (lambda (m) (feature (second m))))
         ((:tk_enum)
          #'feature))
       (tc-members tc)))


(defun arbritary-value (tc)
  (ecase (op:kind tc)
    ((:tk_short :tk_long :tk_ushort :tk_ulong :tk_float :tk_double :tk_octet 
                :tk_longlong :tk_ulonglong :tk_enum) 
     ;; FIXME: enum ?? 
     0)
    ((:tk_boolean) nil)
    ((:tk_char :tk_wchar) #\space)))
