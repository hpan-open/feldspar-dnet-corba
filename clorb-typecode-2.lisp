;;;; Type codes

(in-package :clorb)


;;;; Basic Types

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



;;;; abstract class named-typecode

(define-typecode named-typecode
  :params (id name))



;;;; Simple named typecodes: Native, LocalInterface

(define-typecode native-typecode
  :kind :tk_native
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name)
  :share named-typecode :shared-params 2)


(define-typecode local_interface-typecode
  :kind :tk_local_interface
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name)
  :share named-typecode :shared-params 2)



;;;; Fixed 

(define-typecode fixed-typecode
  :kind :tk_fixed
  :cdr-syntax (:tk_ushort :tk_short)
  :params (fixed_digits fixed_scale))


(defmethod marshal (arg (tc fixed-typecode) buffer)
  (let ((digits (op:fixed_digits tc))
        (scale  (op:fixed_scale tc)))
    (unless (typep arg `(fixed ,digits ,scale))
      (error 'type-error :datum arg :expected-type `(fixed ,digits ,scale)))
    (multiple-value-bind (scaled rest)
                         (floor (* (abs arg) (expt 10 scale)))
      (unless (zerop rest)
        (warn "Fixed<~D,~D> precision loss in ~S" digits scale arg))
      (let ((marshal-digits digits))
        (when (evenp digits) (incf marshal-digits))
        (let ((string (format nil "~v,'0D~A"
                              marshal-digits scaled 
                              (if (< arg 0) "D" "C"))))
          (with-out-buffer (buffer)
            (do ((i 0 (+ i 2)))
                ((> i marshal-digits))
              (put-octet (logior (* (digit-char-p (char string i) 16) 16)
                                 (digit-char-p (char string (+ i 1)) 16))))))))))
          

(defmethod unmarshal ((tc fixed-typecode) buffer)
  (let ((digits (op:fixed_digits tc))
        (scale  (op:fixed_scale tc)))
    (let ((octet-count (ceiling (+ digits 1) 2)))
      (with-in-buffer (buffer)
        (do ((i 0 (+ i 1))
             (n 0))
            ((>= i octet-count) 
             (/ n (expt 10 scale)))
          (macrolet ((accumulate (digit) `(setf n (+ (* n 10) ,digit))))
            (let ((octet (get-octet)))
              (accumulate (ash octet -4))
              (let ((digit (logand octet #xF)))
                (cond ((< digit 10) (accumulate digit))
                      ((= digit #xD) (setf n (- n))))))))))))



;;;; Sequence

(define-typecode sequence-typecode
  :kind :tk_sequence
  :cdr-syntax (complex :tk_typecode :tk_ulong)
  :params (content_type length))

(defmethod marshal (arg (tc sequence-typecode) buffer)
  (let ((el-type (op:content_type tc))
        (max-length (op:length tc))
        (length (length arg)))
    (when (and (not (zerop max-length))
               (> length max-length))
      (error 'CORBA:MARSHAL))
    (marshal-ulong length buffer)
    (map nil #'marshal arg (repeated el-type) (repeated buffer))))

(defmethod unmarshal ((tc sequence-typecode) buffer)
  (let ((eltype (op:content_type tc)))
    (typecase eltype
      (octet-typecode (unmarshal-osequence buffer))
      (t (unmarshal-sequence-m (buffer) (unmarshal eltype buffer))))))



;;;; Strings

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



;;;; Array

(define-typecode array-typecode
  :kind :tk_array
  :cdr-syntax (complex :tk_typecode :tk_ulong)
  :params (content_type length))

(defmethod marshal (arg (tc array-typecode) buffer)
  (let ((el-type (op:content_type tc))
        (max-length (op:length tc))
        (length (length arg)))
    (unless (= length max-length)
      (error 'CORBA:MARSHAL))
    (map nil #'marshal arg (repeated el-type) (repeated buffer))))

(defmethod unmarshal ((tc array-typecode) buffer)
  (let ((eltype (op:content_type tc))
        (len (op:length tc)))
    (let ((arr (make-array len)))
      ;; (map-into arr #'unmarshal (repeated eltype) (repeated buffer))
      (dotimes (i len)
        (setf (aref arr i) (unmarshal eltype buffer)))
      arr)))





;;;; Objref

(define-typecode objref-typecode
  :kind :tk_objref
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name)
  :share named-typecode :shared-params 2
  :constant (corba:tc_object "IDL:omg.org/CORBA/Object:1.0" "Object"))

(defmethod marshal (arg (tc objref-typecode) buffer)
  (marshal-object arg buffer))

(defmethod unmarshal ((tc objref-typecode) buffer)
  (unmarshal-object buffer (op:id tc)))



;;;; Enum

(define-typecode enum-typecode
  :kind :tk_enum
  :share named-typecode
  :shared-params 2
  :params (id name :members)
  :cdr-syntax (complex :tk_string :tk_string (sequence :tk_string))
  :member-params member_name)

(defmethod marshal (arg (enum-tc enum-typecode) buffer)
  (declare (optimize speed))
  ;;(check-type arg (or symbol integer) "a CORBA enum (ingeger or keyword)")
  (let ((symbols (tc-keywords enum-tc)))
    (marshal-ulong 
     (if (integerp arg)
         arg
       (or (position arg symbols)
           (error 'type-error 
                  :datum arg 
                  :expected-type (concatenate 'list '(member) symbols))))
     buffer)))

(defmethod unmarshal ((tc enum-typecode) buffer)
  (let ((index (unmarshal-ulong buffer)))
    (elt (tc-keywords tc) index)))



;;;; Alias

(define-typecode alias-typecode
  :kind :tk_alias
  :share named-typecode
  :shared-params 2
  :cdr-syntax (complex :tk_string :tk_string :tk_typecode)
  :params (id name content_type))

(defmethod marshal (arg (tc alias-typecode) buffer) 
  (marshal arg (op:content_type tc) buffer))

(defmethod unmarshal ((tc alias-typecode) buffer)
  (unmarshal (op:content_type tc) buffer))



;;;; PIDL interface to TypeCode

;; exception Bounds
(define-condition corba:typecode/bounds (corba:userexception) ())

;; exception BadKind
(define-condition corba:typecode/badkind (corba:userexception) ())


(defmacro ignore-badkind (form)
  `(handler-case ,form 
     (corba:typecode/badkind () nil)))


(define-method get_compact_typecode ((tc corba:typecode))
  (let ((params (compact-params tc)))
    (if params
      (map-typecode #'op:get_compact_typecode tc params)
      tc)))


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

(defgeneric tc-unalias (tc)
  (:method ((tc t)) tc)
  (:method ((tc alias-typecode)) (op:content_type tc)))
(define-feature equivalent)

(define-method equivalent ((tc1 CORBA:TypeCode) tc2)
  (macrolet ((maybe (form)
               `(handler-case ,form (corba:typecode/badkind nil t))))
    (let ((tc1 (tc-unalias tc1))
          (tc2 (tc-unalias tc2)))
      (labels ((compare-members ()
                 (let ((n1 (op:member_count tc1)))
                   (and (eql n1 (op:member_count tc2))
                        (loop for i below n1
                              always (and (maybe (op:equivalent 
                                                  (op:member_type tc1 i)
                                                  (op:member_type tc2 i)))
                                          (maybe (equal 
                                                  (op:member_label tc1 i)
                                                  (op:member_label tc2 i)))
                                          (maybe (eql
                                                  (op:member_visibility tc1 i)
                                                  (op:member_visibility tc2 i))))))))
               (compare-params (l1 l2)
                 (loop for x1 in l1 for x2 in l2
                       always (typecase x1
                                (CORBA:TypeCode (op:equivalent x1 x2))
                                (vector (compare-members))
                                (t (equal x1 x2))))))
        (let ((kind1 (typecode-kind tc1))
              (kind2 (typecode-kind tc2)))
          (and (eql kind1 kind2)
               (if (typep tc1 'named-typecode)
                 (let ((id1 (op:id tc1))
                       (id2 (op:id tc2)))
                   (if (or (equal id1 "") (equal id2 ""))
                     (compare-params (cddr (typecode-params tc1))
                                     (cddr (typecode-params tc2)))
                     (equal id1 id2)))
                 (compare-params (typecode-params tc1)
                                 (typecode-params tc2)))))))))
  



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
  "members: (name type visibility)*"
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


(defun tc-member-names (tc)
  (loop for i below (op:member_count tc) collect (op:member_name tc i)))


(defun tc-keywords (tc)
  (with-cache-slot (tc keywords)
    (map 'vector #'key (tc-member-names tc))))


(defun tc-feature-symbols (tc)
  ;; Return a list of feature symbols for the members.
  (if (slot-exists-p tc 'feature-symbols)
    (with-cache-slot (tc feature-symbols)
      (mapcar #'feature (tc-member-names tc)))
    (mapcar #'feature (tc-member-names tc))))

(defun tc-member-types (tc)
  (with-cache-slot (tc member-types)
    (loop for i from 0 below (op:member_count tc)
          collect (op:member_type tc i))))


(defun arbritary-value (tc)
  (ecase (op:kind tc)
    ((:tk_short :tk_long :tk_ushort :tk_ulong :tk_float :tk_double :tk_octet 
                :tk_longlong :tk_ulonglong)
     0)
    ((:tk_enum) (elt (tc-keywords tc) 0))
    ((:tk_boolean) nil)
    ((:tk_char :tk_wchar) #\space)))
