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
        (let ((name (cadr params)))
          (if (and (stringp name)
                   (not (equal name "")))
            (prin1 name stream)
            (format stream "~@[~A~]~@[/~A~]" (car params) (cadr params)))))))))


(eval-when (load eval)
  (loop for i from 0 below (length tckind)
      do (setf (get (elt tckind i) 'tk-value) i)))


;;;; Internal interface to TypeCode

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

(defgeneric compact-params (tc)
  (:method ((tc CORBA:TypeCode)) (typecode-params tc)))

(defun typecode-smash (tc new-tc)
  (setf (slot-value tc 'kind) (slot-value new-tc 'kind)
        (slot-value tc 'params) (slot-value new-tc 'params))
  (unset-extra-slots tc)
  (change-class tc (class-of new-tc)))

(defun map-typecode (func tc &optional (params (typecode-params tc)))
  (if (null params)
    tc
    (labels ((transform (x)
               (typecase x
                 (CORBA:TypeCode  (funcall func x))
                 (sequence  (map 'vector #'transform x))
                 (t  x))))
      (apply #'make-typecode (typecode-kind tc)
             (mapcar #'transform params)))))


(defgeneric tc-members (tc))




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

(define-feature get_compact_typecode)

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



;;;; Defining typecode classes

(defun make-compact-params (class-name params member-params)
  (let ((member-name-p (and (consp member-params) (find 'member_name member-params))))
    (when (or (find 'name params) member-name-p)
      `(defmethod compact-params ((tc ,class-name))
         (let ((params (typecode-params tc)))
           (list
            ,@(loop for p in params and i from 0 collect
                    (cond ((eql p 'name) "")
                          ((and member-name-p (eql p :members))
                           `(map 'vector
                                 (lambda (member)
                                   (list
                                    ,@(loop for mp in member-params and j from 0
                                            collect (cond ((eql mp 'member_name) "")
                                                          (t `(elt member ,j))))))
                                 (elt params ,i)))
                          (t `(elt params ,i))))))))))


(defmacro tcp-elt (x i)
  (case i
    (0 `(first (typecode-params ,x)))
    (1 `(second (typecode-params ,x)))
    (2 `(third (typecode-params ,x)))
    (3 `(fourth (typecode-params ,x)))
    (4 `(fifth (typecode-params ,x)))
    (otherwise `(elt (typecode-params ,x) ,i))))


(defmacro define-typecode (class-name &key kind cdr-syntax params member-params 
                                       constant extra-slots share (shared-params 0))
  `(progn
     (defclass ,class-name (,(or share 'CORBA:TypeCode))
       ,extra-slots)
     ,@(if kind
         `((setf (get ',kind 'tk-params) ',cdr-syntax)
           (setf (get ',kind 'tk-class) ',class-name)
           ,(make-compact-params class-name params member-params)))
     ,@(loop for param in params
             for i from 0
             unless (< i shared-params)
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
     ,@(if extra-slots
         `((defmethod unset-extra-slots ((tc ,class-name))
             ,@(loop for name in extra-slots
                     collect `(slot-makunbound tc ',name)))))
     ,@(if constant
         `((defparameter ,(if (consp constant) (car constant) constant)
             (make-typecode ,kind ,@(mapcar #'kwote (if (consp constant)
                                                      (cdr constant)))))))))


(defgeneric unset-extra-slots (tc)
  (:method ((tc CORBA:TypeCode))
           (slot-makunbound tc 'keywords)))


(defmacro with-cache-slot ((object slot &key 
                                   (dont-cache-types nil)
                                   (cache-types t))
                           &body body)
  (let ((objvar (gensym)))
    `(let ((,objvar ,object))
       (if (slot-boundp ,objvar ',slot)
         (slot-value ,objvar ',slot)
         (let ((new-value (progn ,@body)))
           (typecase new-value
             ,@(if dont-cache-types
                 `((,dont-cache-types new-value)))
             (,cache-types (setf (slot-value ,objvar ',slot) new-value))
             ,@(if (not (eql cache-types t))
                 `((t new-value)))))))))



;;;; Accessing typecodes of defined types

(defun symbol-typecode (symbol)
  ;; Return the type code for the scoped symbol of an idltype.
  ;; handling of recursive typecode computation similar to 
  ;; op:type in IFR.
  (let ((typecode (get symbol 'typecode)))
    (cond ((null typecode)
           (setf (get symbol 'typecode) 
                 (make-instance 'CORBA:TypeCode 
                   :kind t
                   :params (list "" (symbol-name symbol)))))
          ((functionp typecode) 
           (setf (get symbol 'typecode) nil)
           (set-symbol-typecode symbol typecode))
          (t
           #-omit-debug
           (when (eql t (typecode-kind typecode))
             (setf (typecode-params typecode)
                   (list "" (symbol-name symbol))))
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

(defun typecode-symbol (tc)
  (ifr-id-symbol (op:id tc)))

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


