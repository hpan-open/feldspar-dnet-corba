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
     :tk_native :tk_abstract_interface
     :tk_local_interface) 
  "The symbols for the TCKind enum")

(deftype corba:tckind ()
  (cons 'member (coerce tckind 'list)))


(defclass CORBA:TYPECODE ()
  ((kind :initarg :kind)
   (params :initarg :params)
   (keywords )))

(defmethod print-object ((tc corba:typecode) stream)
  (cond
   (*print-readably*
    (format stream "#.(CLORB::MAKE-TYPECODE '~S~{ '~S~})"
            (slot-value tc 'kind) (slot-value tc 'params)))
   (*print-pretty*
    (format stream "~<#<TYPECODE~;~:I~@{~:_ ~W~}~;>~:>" (cons (slot-value tc 'kind) (slot-value tc 'params))))
   (t
    (print-unreadable-object (tc stream :type t :identity t)
      (let ((params (slot-value tc 'params)))
        (when (stringp (ignore-errors (cadr params)))
          (prin1 (cadr params))))))))
  


(eval-when (load eval)
  (loop for i from 0 below (length tckind)
      do (setf (get (elt tckind i) 'tk-value) i)))



(defun make-typecode (kind &rest params)
  (let ((class (if (eq kind :recursive)
                 'CORBA:TypeCode
                 (get kind 'tk-class))))
    (unless class
      (error "Bad TypeCode kind: ~S" kind))
    (make-instance class :kind kind :params params)))

(defun typecode-kind (tc)
  (slot-value tc 'kind))

(defun typecode-params (tc)
  (slot-value tc 'params))

(defun (setf typecode-params) (params tc)
  (setf (slot-value tc 'params) params))

(defun typecode-smash (tc new-tc)
  (setf (slot-value tc 'kind) (slot-value new-tc 'kind)
        (slot-value tc 'params) (slot-value new-tc 'params))
  (slot-makunbound tc 'keywords)
  (change-class tc (class-of new-tc)))


(defun feature (name)
  (intern (string-upcase name) :op))

(defun key (string)
  (check-type string string)
  (intern (string-upcase string) :keyword))


;;;; PIDL interface to TypeCode

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


(define-feature id)

(define-feature name)

(define-feature member_count)

(define-feature member_name)

(define-feature member_type)

(define-feature member_label)

(define-feature member_visibility)

(define-feature discriminator_type)

(define-feature default_index)

(define-feature length)

(define-feature content_type)

(define-feature fixed_digits)

(define-feature fixed_scale)

(define-feature type_modifier)

(define-feature concrete_base_type)



;;;; Accessing typecodes of defined types

(defun symbol-typecode (symbol)
  ;; Return the type code for the scoped symbol of an idltype.
  ;; handling of recursive typecode computation similar to 
  ;; op:type in IFR.
  (let ((typecode (get symbol 'typecode)))
    (cond ((null typecode)
           (setf (get symbol 'typecode) (make-instance 'CORBA:TypeCode :kind t)))
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


;;;; Interface Repository Descriptions
;; Not exactly TypeCode related, but similar to the stuff above

(defun set-ifr-description (symbol desc)
  (setf (get symbol 'ifr-desc) desc))

(defgeneric generate-ifr-description (tc symbol))

(defun ifr-description (symbol)
  (let ((desc (get symbol 'ifr-desc)))
    (cond ((functionp desc)
           (set-ifr-description symbol (funcall desc)))
          ((null desc)
           (set-ifr-description symbol
            (generate-ifr-description (get symbol 'typecode) symbol)))
          (t 
           desc))))

(defun set-ifr-info (symbol &key id name typecode version defined_in parameters exceptions result mode type bases)
  (setf (get symbol 'ifr-desc) nil)
  (when id 
    (set-symbol-ifr-id symbol id))
  (when name
    (setf (get symbol 'ifr-name) name))
  (when typecode
    (set-symbol-typecode symbol typecode))
  (when version
    (setf (get symbol 'ifr-version) version))
  (when defined_in
    (setf (get symbol 'ifr-parent) defined_in)
    (pushnew symbol (get defined_in 'ifr-contents)))
  (when result
    (setf (get symbol 'ifr-result) result))
  (when mode
    (setf (get symbol 'ifr-mode) mode))
  (when type
    (setf (get symbol 'ifr-type) type))
  (when parameters
    (setf (get symbol 'ifr-params) parameters))
  (when exceptions
    (setf (get symbol 'ifr-exceptions) exceptions))
  (when bases
    (setf (get symbol 'ifr-bases) bases)))