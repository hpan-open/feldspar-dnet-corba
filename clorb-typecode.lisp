;;;; Type codes

(in-package :clorb)


(defparameter tckind
  '#(:tk_null :tk_void :tk_short :tk_long :tk_ushort :tk_ulong
     :tk_float :tk_double :tk_boolean :tk_char
     :tk_octet :tk_any :tk_typecode :tk_principal :tk_objref
     :tk_struct :tk_union :tk_enum :tk_string
     :tk_sequence :tk_array :tk_alias :tk_except
     :tk_longlong :tk_ulonglong :tk_longdouble
     :tk_wchar :tk_wstring :tk_fixed :tk_value :tk_value_box
     :tk_native :tk_abstract_interface) 
  "The symbols for the TCKind enum")

(deftype corba:tckind ()
  (cons 'member (coerce tckind 'list)))


(defclass CORBA:TYPECODE ()
  ((kind :initarg :kind)
   (params :initarg :params)
   (keywords )))

;;(define-slot-dumper CORBA:TypeCode)


(defmethod print-object ((tc corba:typecode) stream)
  (cond
   (*print-readably*
    (format stream "#.(CLORB::MAKE-TYPECODE '~S~{ '~S~})"
            (slot-value tc 'kind) (slot-value tc 'params)))
   (*print-pretty*
    (format stream "~<#<TYPECODE~;~:I~@{~:_ ~W~}~;>~:>" (cons (slot-value tc 'kind) (slot-value tc 'params))))
   (t
    (print-unreadable-object (tc stream :type t :identity t)
      (prin1 (slot-value tc 'kind) stream)))))


(eval-when (load eval)
  (loop for i from 0 below (length tckind)
      do (setf (get (elt tckind i) 'tk-value) i)))

(defmacro put (sym key val)
  `(setf (get ,sym ,key) ,val))




(put ':tk_fixed 'tk-params '(:tk_ushort :tk_short))

(put ':tk_objref 'tk-params '(complex :tk_string :tk_string))

(put ':tk_struct 'tk-params 
     '(complex :tk_string :tk_string
       (sequence (anon-struct :tk_string :tk_typecode))))

(put ':tk_union 'tk-params 
     '(complex 
       :tk_string                       ; Repository id
       :tk_string                       ; name
       :tk_typecode                     ; discriminant-type
       :tk_long                         ; default index
       #| Members: (label name typecode)* |#))

(put ':tk_enum 'tk-params 
     '(complex
       :tk_string                       ; Repository id
       :tk_string                       ; name
       (sequence                        ; members
        :tk_string)))

(put ':tk_sequence 'tk-params '(complex :tk_typecode :tk_ulong))

(put ':tk_string 'tk-params '(:tk_ulong))

(put ':tk_wstring 'tk-params '(:tk_ulong))

(put ':tk_array 'tk-params '(complex :tk_typecode :tk_ulong))

(put ':tk_alias 'tk-params '(complex :tk_string :tk_string :tk_typecode))

(put ':tk_except 'tk-params '(complex :tk_string :tk_string
                              (sequence (anon-struct :tk_string :tk_typecode))))


(put ':tk_value 'tk-params 
     '(complex :tk_string :tk_string :tk_short :tk_typecode 
       (sequence (anon-struct :tk_string :tk_typecode :tk_short))))

(put ':tk_value_box 'tk-params '(complex :tk_string :tk_string :tk_typecode))

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
;; :tk_fixed    digits		scale

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

(defun typecode-smash (tc new-tc)
  (setf (slot-value tc 'kind) (slot-value new-tc 'kind)
        (slot-value tc 'params) (slot-value new-tc 'params))
  (slot-makunbound tc 'keywords))

(defun type-expand (type)
  (typecase type
    (symbol (values type nil))
    (cons   (values (car type) (cdr type)))
    (t      (values (typecode-kind type) (typecode-params type)))))

(defun feature (name)
  (intern (string-upcase name) :op))

(defun key (string)
  (check-type string string)
  (intern (string-upcase string) :keyword))

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
   (corba:tc_null :tk_null)
   (corba:tc_void :tk_void)
   (corba:tc_short :tk_short)
   (corba:tc_long :tk_long)
   (corba:tc_ushort :tk_ushort)
   (corba:tc_ulong :tk_ulong)
   (corba:tc_float :tk_float)
   (corba:tc_double :tk_double)
   (corba:tc_boolean :tk_boolean)
   (corba:tc_char :tk_char)
   (corba:tc_octet :tk_octet)
   (corba:tc_any :tk_any)
   (corba:tc_typecode :tk_typecode)
   (corba:tc_object :tk_objref "IDL:omg.org/CORBA/Object:1.0" "Object")
   (corba:tc_string :tk_string 0)
   (corba:tc_longlong :tk_longlong)
   (corba:tc_ulonglong :tk_ulonglong)
   (corba:tc_longdouble :tk_longdouble)
   (corba:tc_wchar :tk_wchar)
   (corba:tc_wstring :tk_wstring 0)))

;; deprecated
;;(defparameter corba:tc_objref corba:tc_object)


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

(define-method equal ((x t) y)
  (equalp x y))

(define-method equal ((x string) y)
  (equalp x y))

(define-method equal ((x vector) y)
  (every #'op:equal x y))

(define-method equal ((x cons) y)
  (every #'op:equal x y))

(defun tcp-equal (tcp1 tcp2)
  (or (eq tcp1 tcp2)
      (every #'op:equal tcp1 tcp2)))

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
  (coerce (case (typecode-kind tc)
            ((:tk_struct :tk_except :tk_enum)
             (third (typecode-params tc)))
            (:tk_union
             (fifth (typecode-params tc)))
            (otherwise
             (error 'corba:typecode/badkind))) 
          'vector ))

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
    (:tk_alias
     (third (typecode-params tc)))
    (:tk_string corba:tc_char)
    (:tk_wstring corba:tc_wchar)
    (otherwise
     (error 'corba:typecode/badkind))))


;; for tk_fixed

(define-method fixed_digits ((tc corba:typecode))
  (case (typecode-kind tc)
    (:tk_fixed
     (first (typecode-params tc)))
    (otherwise
     (error 'corba:typecode/badkind))))

(define-method fixed_scale ((tc corba:typecode))
  (case (typecode-kind tc)
    (:tk_fixed
     (second (typecode-params tc)))
    (otherwise
     (error 'corba:typecode/badkind))))


;;;; Accessing typecodes of defined types

(defun symbol-typecode (symbol)
  ;; Return the type code for the scoped symbol of an idltype.
  ;; handling of recursive typecode computation similar to 
  ;; op:type in IFR.
  (let ((typecode (get symbol 'typecode)))
    (cond ((null typecode)
           (setf (get symbol 'typecode) (make-typecode t)))
          ((functionp typecode) 
           (setf (get symbol 'typecode) nil)
           (set-symbol-typecode symbol typecode))
          (t
           typecode))))

(defun set-symbol-typecode (symbol typecode)
  ;; Set the typecode for a scoped symbol. Typecode can also be a function to compute the typecode.
  (typecode-smash (symbol-typecode symbol)
                  (if (functionp typecode)
                    (funcall typecode)
                    typecode)))


(defvar *ifr-id-symbol*
  (make-hash-table :size 251 :test #'equal)
  "Map interface repository ID to the scoped symbol for the type.")

(defun symbol-ifr-id (symbol)
  "Return the interface repository id for the scoped symbol."
  (get symbol 'ifr-id))

(defun ifr-id-symbol (id)
  "Return the scoped symbol for a known interface repository ID."
  (gethash id *ifr-id-symbol*))

(defun set-symbol-ifr-id (symbol id)
  (setf (gethash id *ifr-id-symbol*) symbol)
  (setf (get symbol 'ifr-id) id))

(defun set-symbol-id/typecode (symbol id typecode)
  (set-symbol-ifr-id symbol id)
  (set-symbol-typecode symbol typecode))


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

(defun arbritary-value (tc)
  (ecase (op:kind tc)
    ((:tk_short :tk_long :tk_ushort :tk_ulong :tk_float :tk_double :tk_octet 
                :tk_longlong :tk_ulonglong :tk_enum) 
     ;; FIXME: enum ?? 
     0)
    ((:tk_boolean) nil)
    ((:tk_char :tk_wchar) #\space)))

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

(defun create-exception-tc (id name members)
  "Create Exception TypeCode for interface repository ID and NAME, with MEMEBERS.
Members on form: (name TypeCode)"
  (make-typecode :tk_except id name members))

(defun make-tc-alias (id name typecode)
  (make-typecode :tk_alias id name typecode))

(defun create-alias-tc (id name typecode)
  (make-typecode :tk_alias id name typecode))

(defun create-enum-tc (id name members)
  (make-typecode :tk_enum id name (coerce members 'vector)))




