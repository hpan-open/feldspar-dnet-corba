(in-package :clorb)

(defclass code-target ()
  ((packages :initform nil)
   (symbols  :initform nil)
   (defs     :initform nil)
   (dynamic-stubs :initform t
                  :initarg :dynamic-stubs
                  :accessor target-dynamic-stubs)))

(defgeneric target-typecode (obj target)
  (:documentation
   "The target code to compute the typecode for idltype object."))

(defgeneric target-code (obj target)
  (:documentation
   "The target code defining the object."))

(defgeneric target-type (idltype target)
  (:documentation
   "Lisp type mapping for IDL-type."))


(defun make-target-symbol (target name package)
  (let* ((package
          (or #+clisp (if (packagep package) package)
              (find-package package)
              (make-package package :use '())))
         (symbol (intern (string-upcase name) package)))
    (export symbol package)
    (pushnew package (slot-value target 'packages))
    (pushnew symbol  (slot-value target 'symbols))
    symbol))

(defun scoped-target-symbol (target obj)
  (let* ((this-type (op:def_kind obj))
         (container (op:defined_in obj))
         (container-type (and container (op:def_kind container)))
         (name
          (if (or (null container)
                  (eq container-type :dk_Repository)
                  (and (not (eq this-type :dk_Module))
                       (eq container-type :dk_Module)))
              (op:name obj)
            (concatenate 'string
                         (symbol-name (scoped-target-symbol target container))
                         "/" (op:name obj))))
         (package
          (if (eq this-type :dk_Module)
              :keyword
            ;; Find enclosing Module
            (do* ((container container (op:defined_in container))
                  (container-type container-type
                                  (and container (op:def_kind container))))
                ((or (null container) (eq container-type :dk_Module))
                 (if container (scoped-target-symbol target container)
                   "OMG.ORG/ROOT"))))))
    (make-target-symbol target name package)))

(defun scoped-target-symbol-in (target name container)
  (let* ((container-type (and container (op:def_kind container)))
         (name
          (if (or (null container)
                  (eq container-type :dk_Repository))
            name
            (concatenate 'string
                         (symbol-name (scoped-target-symbol target container))
                         "/" name)))
         (package
          ;; Find enclosing Module
          (do* ((container container (op:defined_in container))
                (container-type container-type
                                (and container (op:def_kind container))))
               ((or (null container) (eq container-type :dk_Module))
                (if container (scoped-target-symbol target container)
                    "OMG.ORG/ROOT")))))
    (make-target-symbol target name package)))


(defun make-progn (l)
  (cons 'progn
        (loop for x in l
            append (if (and (consp x) (eq 'progn (car x)))
                     (cdr x)
                     (list x)))))

(defun make-target-ensure-package (package target)
  (let* ((package-name (package-name package))
         (exports (loop for sym in (slot-value target 'symbols)
                        when (string= package-name (package-name (symbol-package sym)))
                        collect (symbol-name sym))))
    `(ensure-corba-package ,package-name
                           :export ',exports)))

(defun make-allways-eval (code)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,code))

(defun make-idltype (symbol id typecode &rest forms)
  (make-progn
   `((set-symbol-id/typecode ',symbol ,id ,typecode)
     ,@forms)))

(defun target-base-list (target bases make-symbol-fun root-class)
  (if (zerop (length bases))
      `(,root-class)
      (map 'list (lambda (base)
                   (funcall make-symbol-fun target base))
           bases)))

(defun target-defclass (target idef)
  (let ((bases (op:base_interfaces idef)))
    `(defclass ,(scoped-target-symbol target idef)
      ,(target-base-list target bases #'scoped-target-symbol 'CORBA:Object)
       ())))

(defun target-defproxyclass (target idef)
  (let ((bases (op:base_interfaces idef))
        (class-symbol (target-proxy-class-symbol target idef)))
    `(progn
       (defclass ,class-symbol
         ,(list* (scoped-target-symbol target idef)
                 (target-base-list target bases #'target-proxy-class-symbol
                                   'CORBA:Proxy))
         ())
       (register-proxy-class ,(op:id idef) ',class-symbol))))

(defun target-proxy-class-symbol (target idef)
  (let ((scoped-symbol (scoped-target-symbol target idef)))
    (make-target-symbol target
                        (concatenate 'string
                                     (symbol-name scoped-symbol)
                                     "-PROXY")
                        (symbol-package scoped-symbol))))


(defun setter-name (name)
  (concatenate 'string "_set_" name))

(defun getter-name (name)
  (concatenate 'string "_get_" name))

(defmethod target-type ((obj CORBA:PrimitiveDef) target)
  (declare (ignore target))
  (ecase (op:kind obj)
    (:pk_short 'CORBA:short)
    (:pk_long 'CORBA:long)
    (:pk_ushort 'CORBA:ushort)
    (:pk_ulong 'CORBA:ulong)
    (:pk_float 'CORBA:float)
    (:pk_double 'CORBA:double)
    (:pk_boolean 'CORBA:boolean)
    (:pk_char 'CORBA:char)
    (:pk_octet 'CORBA:octet)
    (:pk_any 'CORBA:any)
    (:pk_TypeCode 'CORBA:TypeCode)
    (:pk_string 'CORBA:string)
    (:pk_objref 'CORBA:Object)
    (:pk_longlong 'CORBA:longlong)
    (:pk_ulonglong 'CORBA:ulonglong)
    (:pk_longdouble 'CORBA:longdouble)
    (:pk_wchar 'CORBA:wchar)
    (:pk_wstring 'CORBA:wstring)))


(defmethod target-type ((obj CORBA:SequenceDef) target)
  (declare (ignore target))
  `sequence)

(defmethod target-type ((obj CORBA:ArrayDef) target)
  (declare (ignore target))
  ;;FIXME: handle multi dim arrays
  `(array t (,(op:length obj))))

(defmethod target-type ((obj CORBA:AliasDef) target)
  (scoped-target-symbol target obj))

(defmethod target-type ((obj CORBA:StructDef) target)
  (scoped-target-symbol target obj))

(defmethod target-typecode ((obj CORBA:PrimitiveDef) target)
  (declare (ignore target))
  (ecase (op:kind obj)
   (:pk_any `CORBA:tc_any)
   (:pk_boolean `CORBA:tc_boolean)
   (:pk_char `CORBA:tc_char)
   (:pk_double `CORBA:tc_double)
   (:pk_float `CORBA:tc_float)
   (:pk_long `CORBA:tc_long)
   (:pk_longdouble `CORBA:tc_longdouble)
   (:pk_longlong `CORBA:tc_longlong)
   (:pk_objref `CORBA:tc_objref)
   (:pk_octet `CORBA:tc_octet)
   (:pk_short `CORBA:tc_short)
   (:pk_string `CORBA:tc_string)
   (:pk_typecode `CORBA:tc_typecode)
   (:pk_ulong `CORBA:tc_ulong)
   (:pk_ulonglong `CORBA:tc_ulonglong)
   (:pk_ushort `CORBA:tc_ushort)
   (:pk_wchar `CORBA:tc_wchar)
   (:pk_void `CORBA:tc_void)))

(defmethod target-typecode ((x CORBA:StringDef) target)
  (declare (ignore target))
  (if (zerop (op:bound x))
     `CORBA:tc_string
     `(make-typecode :tk_string ,(op:bound x))))

(defmethod target-typecode ((x CORBA:SequenceDef) target)
  `(make-sequence-typecode
    ,(target-typecode (op:element_type_def x) target)
    ,(op:bound x)))

(defmethod target-typecode ((x CORBA:Contained) target)
  `(symbol-typecode ',(scoped-target-symbol target x)))

(defmethod target-typecode ((x CORBA:ExceptionDef) target)
  `(symbol-typecode ',(scoped-target-symbol target x)))

(defmethod target-typecode ((x CORBA:ArrayDef) target)
  `(make-array-typecode ,(target-typecode (op:element_type_def x) target)
                        ,(op:length x)))


;;;; target-code methods

(defmethod target-code ((x CORBA:Contained) target)
  (declare (ignore target))
  (mess 4 "Can't generate code for ~A of kind ~S"
        (op:name x)
        (op:def_kind x)))

(defmethod target-code ((x CORBA:AliasDef) target)
  (let ((symbol (scoped-target-symbol target x)))
    (make-idltype
     symbol
     (op:id x)
     `(lambda ()
        (make-tc-alias ,(op:id x) ,(op:name x)
                       ,(target-typecode (op:original_type_def x) target)))
     `(deftype ,symbol ()
        ',(target-type (op:original_type_def x) target)))))

(defmethod target-code ((const CORBA:ConstantDef) target)
  `(defconstant ,(scoped-target-symbol target const)
     ',(op:value const)))

(defmethod target-code ((idef CORBA:InterfaceDef) target)
  (make-idltype
   (scoped-target-symbol target idef)
   (op:id idef)
   `(make-typecode :tk_objref ,(op:id idef) ,(op:name idef))
   (target-defclass target idef)
   (target-defproxyclass target idef)
   (call-next-method)))

(defmethod target-code ((op CORBA:OperationDef) target)
  (let* ((op-name (op:name op))
         (class (target-proxy-class-symbol target (op:defined_in op))))
    (if (target-dynamic-stubs target)
      (let ((lisp-name (make-target-symbol target op-name :op)))
        `(defmethod ,lisp-name ((obj ,class) &rest args)
           (apply #'invoke obj ,op-name args)))
      (let* ((lisp-name (string-upcase op-name))
             (params (coerce (op:params op) 'list))
             (args (loop for p in params
                         unless (eq (op:mode p) :param_out)
                         collect (make-target-symbol target (op:name p) :clorb))))
        `(define-method ,lisp-name ((obj ,class) ,@args)
           (let ((_request
                  (request-create obj ,op-name ,(target-typecode (op:result_def op) target))))
             ,@(loop for pd in params
                     for mode = (op:mode pd)
                     collect `(add-arg _request ,(op:name pd)
                                       ,(ecase mode
                                          (:param_in 'ARG_IN)
                                          (:param_out 'ARG_OUT)
                                          (:param_inout 'ARG_INOUT))
                                       ,(target-typecode (op:type_def pd) target)
                                       ,@(if (eq mode :param_out) nil (list (pop args)))))
             ,@(map 'list
                    (lambda (ed)
                      `(add-exception _request ,(target-typecode ed target)))
                    (op:exceptions op))
             (request-funcall _request)))))))


(defmethod target-code ((op CORBA:AttributeDef) target)
  (let* ((att-name (op:name op))
         (lisp-name (string-upcase att-name))
         (class (target-proxy-class-symbol target (op:defined_in op))))
    `(progn
       (define-method ,lisp-name ((obj ,class))
         ,(if (target-dynamic-stubs target)
            `(corba:funcall ,(getter-name att-name) obj)
            `(get-attribute obj ,(getter-name att-name) 
                            ,(target-typecode (op:type_def op) target))))
       ,@(if (eq (op:mode op) :attr_normal)
             (list `(define-method (setf ,lisp-name) (newval (obj ,class))
                      ,(if (target-dynamic-stubs target)                      
                         `(corba:funcall ,(setter-name att-name) obj newval)
                         `(set-attribute obj ,(setter-name att-name) 
                                         ,(target-typecode (op:type_def op) target) newval))))))))


(defmethod target-code ((mdef CORBA:Container) target)
  (make-progn (map 'list (lambda (contained)
                           (target-code contained target))
                   (op:contents mdef :dk_All t))))

(defmethod target-code ((sdef CORBA:StructDef) target)
  `(define-struct ,(scoped-target-symbol target sdef)
     :id ,(op:id sdef)
     :name ,(op:name sdef)
     :members ,(map 'list
                    (lambda (smember)
                      (list (op:name smember)
                            (target-typecode (op:type_def smember) target)
                            (make-target-symbol target (op:name smember)
                                                'clorb)))
                    (op:members sdef))))

(defmethod target-code ((enum CORBA:EnumDef) target)
  (make-idltype
   (scoped-target-symbol target enum)
   (op:id enum)
   `(make-typecode :tk_enum ,(op:id enum) ,(op:name enum) ',(op:members enum))
   `(deftype ,(scoped-target-symbol target enum) ()
      '(member ,@(map 'list (lambda (name) (make-target-symbol target name :keyword))
                      (op:members enum))))))

(defmethod target-code ((exc CORBA:ExceptionDef) target)
  `(define-user-exception ,(scoped-target-symbol target exc)
       :id ,(op:id exc)
       :name ,(op:name exc)
       :members ,(map 'list
                      (lambda (smember)
                        (let ((m-name (op:name smember)))
                          (list m-name (target-typecode (op:type_def smember) target))))
                      (op:members exc))))


(defmethod target-code ((def CORBA:UnionDef) target)
  (let ((collected-members '())
        (default-member nil)
        (used-labels '())
        (default-index (op:default_index (op:type def))))
    (doseq (m (op:members def))
      (let* ((name (op:name m))
             (raw-label (op:label m))
             (label (any-value raw-label))
             (defaultp (zerop default-index)))
        (push (list label (target-typecode (op:type_def m) target)
                    :name name :default defaultp
                    :creator (scoped-target-symbol-in target name def))
              collected-members)
        (cond (defaultp
                (setq default-member collected-members))
              (t
               (push label used-labels))))
      (decf default-index))
    (when default-member
      (setf (caar default-member)
            (block nil
              (typecode-values-do (lambda (x) (unless (member x used-labels) (return x)))
                                  (op:discriminator_type def))))) 
    `(define-union ,(scoped-target-symbol target def)
       :id   ,(op:id def)
       :name ,(op:name def)
       :discriminator-type ,(target-typecode (op:discriminator_type_def def) target)
       :members ,(nreverse collected-members))))



;;;; Stub code generator

(defparameter *stub-code-ignored-packages*
  (list (find-package :keyword)
        (find-package :clorb)
        (find-package :op)))

(defun stub-code (object)
  (when (stringp object)
    (setq object (lookup-name object)))
  (let ((target (make-instance 'code-target)))
    (let ((code (target-code object target))
          (ignored-packages *stub-code-ignored-packages*))
      `(,@(loop for package in (slot-value target 'packages)
                unless (member package ignored-packages)
                collect `(defpackage ,(package-name package) (:use)))
        (export (,@(slot-value target 'symbols)))
        ,code))))

(defun gen-stub-file (object filename &key package-def dynamic-stubs)
  (with-open-file (*standard-output* filename :direction :output :if-exists :supersede)
    (let* ((target (make-instance 'code-target :dynamic-stubs dynamic-stubs))
           (code (target-code object target)))
      (pprint '(in-package :clorb))
      (when package-def
        (loop for package in (slot-value target 'packages)
                unless (member package *stub-code-ignored-packages*)
                do (terpri)
                   (pprint (make-allways-eval
                            (make-target-ensure-package package target)))))
      (dolist (x (remove nil (cdr code)))
        (terpri)
        (pprint x))
      (terpri))))


;; ----------------------------------------------------------------------
;;;; Configure the pretty printer
;; ----------------------------------------------------------------------

(set-pprint-dispatch '(cons (member define-method))
                     (pprint-dispatch '(defmethod foo ()) ))

(defun pprint-def-and-keys (*standard-output* list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-exit-if-list-exhausted)
    (write (pprint-pop))
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (pprint-newline :linear)
      (write (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (write (pprint-pop)))))

(set-pprint-dispatch '(cons (member define-user-exception define-corba-struct
                                    define-struct define-union))
                     'pprint-def-and-keys)

#||
(load "clorb:src;iop-idl")
(gen-stub-file (lookup-name "IOP") "clorb:x-iop.lisp")
(gen-stub-file (lookup-name "CosNaming") "clorb:x-cosnaming.lisp" :package-def t)
(gen-stub-file (vsns-get "clorb") "clorb:x-orb-base.lisp")
(gen-stub-file (vsns-get "ir") "clorb:x-ifr-base.lisp")
(gen-stub-file (vsns-get "file.i") "clorb:x-file.lisp")

(load "clorb:src;ifr-idl")
(gen-stub-file (lookup-name "CORBA") "clorb:x-ifr-base.lisp")

(load "clorb:src;file-idl")
(gen-stub-file (lookup-name "poa") "clorb:x-file.lisp" :package-def t)

(defvar *target* (make-instance 'code-target))
(target-code (lookup-name "CosNaming") *target*)
||#
