(in-package :clorb)

(defclass code-target ()
  ((packages :initform nil)
   (symbols  :initform nil)
   (defs     :initform nil)))

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
   `((set-symbol-ifr-id ',symbol ,id)
     (set-symbol-typecode ',symbol ,typecode)
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

(defmethod target-type ((obj CORBA:AliasDef) target)
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
   (:pk_wchar `CORBA:tc_wchar)))


(defmethod target-typecode ((x CORBA:SequenceDef) target)
  `(make-sequence-typecode 
    ,(target-typecode (op:element_type_def x) target)
    ,(op:bound x)))


(defmethod target-typecode ((x CORBA:IDLType) target)
  `(symbol-typecode ',(scoped-target-symbol target x)))


;;;; target-code methods

(defmethod target-code ((x CORBA:Contained) target)
  (declare (ignore target))
  (mess 4 "Can't generate code for ~A of kind ~S"
        (op:name x)
        (op:def_kind x))
  )

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
   (make-progn
    (map 'list (lambda (x) (target-code x target))
         (op:contents idef :dk_all t)))))

(defmethod target-code ((op CORBA:OperationDef) target)
  (let* ((op-name (op:name op))
         (lisp-name (make-target-symbol target op-name :op))
         (class (target-proxy-class-symbol target (op:defined_in op))))
    `(defmethod ,lisp-name ((obj ,class) &rest args)
       (apply 'clorb::invoke obj ,op-name args))))


(defmethod target-code ((op CORBA:AttributeDef) target)
  (let* ((att-name (op:name op))
         (lisp-name (make-target-symbol target att-name :op))
         (class (target-proxy-class-symbol target (op:defined_in op))))
    `(progn
       (defmethod ,lisp-name ((obj ,class) &rest args)
         (apply 'clorb::invoke obj ,(getter-name att-name) args))
       ,@(if (eq (op:mode op) :attr_normal)
             (list `(defmethod (setf ,lisp-name) (newval (obj ,class))
                      (apply 'clorb::invoke obj ,(setter-name att-name)
                             newval)))))))


(defmethod target-code ((mdef CORBA:ModuleDef) target)
  (make-progn (map 'list (lambda (contained)
                           (target-code contained target))
                   (op:contents mdef :dk_All t))))

(defmethod target-code ((sdef CORBA:StructDef) target)
  (make-idltype 
   (scoped-target-symbol target sdef)
   (op:id sdef)
   `(lambda ()
      (struct-typecode ,(op:id sdef) ,(op:name sdef)
                       ,@(loop for smember in (coerce (op:members sdef) 'list)
                               nconc (list (op:name smember)
                                           (target-typecode (op:type_def smember) target)))))
   `(define-corba-struct ,(scoped-target-symbol target sdef)
      :id ,(op:id sdef)
      :members ,(map 'list 
                     (lambda (smember)
                       (list (make-target-symbol target (op:name smember)
                                                 'clorb)
                             nil))
                     (op:members sdef)))))

(defmethod target-code ((enum CORBA:EnumDef) target)
  `(deftype ,(scoped-target-symbol target enum) ()
     '(member ,@(map 'list (lambda (name) (make-target-symbol target name :keyword))
                     (op:members enum)))))

(defmethod target-code ((exc CORBA:ExceptionDef) target)
  `(Define-user-exception ,(scoped-target-symbol target exc)
       :id ,(op:id exc)
       :slots ,(map 'list
                 (lambda (smember)
                   (let ((m-name (op:name smember)))
                     (list (make-target-symbol target m-name 'clorb)
                           :initarg (make-target-symbol target m-name :keyword))))
                 (op:members exc))))


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

(defun gen-stub-file (object filename &key package-def)
  (with-open-file (*standard-output* filename :direction :output :if-exists :supersede)
    (let* ((target (make-instance 'code-target))
           (code (target-code object target)))
      (print '(in-package :clorb))
      (when package-def
        (loop for package in (slot-value target 'packages)
                unless (member package *stub-code-ignored-packages*)
                do (terpri)
                   (pprint (make-allways-eval 
                            (make-target-ensure-package package target)))))
      (mapc (lambda (x) (terpri) (pprint x))
            (remove nil (cdr code)))
      (terpri))))


#|
(load "clorb:src;iop-idl")
(gen-stub-file (lookup-name "IOP") "clorb:x-iop.lisp")
(gen-stub-file (lookup-name "CORBA") "clorb:orb-stub.lisp")
(gen-stub-file (lookup-name "CosNaming") "clorb:x-cosnaming.lisp" :package-def t)


(defvar *target* (make-instance 'code-target))
(target-code (lookup-name "CosNaming") *target*)

(pprint
(loop for sym in (slot-value *target* 'symbols)
      collect (cons (package-name (symbol-package sym)) (symbol-name sym))))

(pprint (make-target-ensure-package (elt (slot-value *target* 'packages) 2) *target*))



|#
