(in-package :clorb)

(defclass code-target ()
  ((packages :initform nil)
   (symbols  :initform nil)
   (defs     :initform nil)))


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


(defun target-base-list (target bases root-class)
  (if bases 
      (map 'list (lambda (base)
                   (scoped-target-symbol target base))
           bases)
      `(,root-class)))

(defun target-defclass (target idef)
  (let ((bases (op:base_interfaces idef)))
    `(defclass ,(scoped-target-symbol target idef) 
      ,(target-base-list target bases 'CORBA:Object)
       ())))

(defun target-defproxyclass (target idef)
  (let ((bases (op:base_interfaces idef))
        (class-symbol (target-proxy-class-symbol target idef)))
    `(progn
       (defclass ,class-symbol
           ,(target-base-list target bases
                              (list (scoped-target-symbol target idef) 'CORBA:Proxy))
         ())
       (register-proxy-class ,(op:id idef) ',class-symbol))))

(defun target-proxy-class-symbol (target idef)
  (let ((scoped-symbol (scoped-target-symbol target idef)))
    (make-target-symbol target
                        (concatenate 'string
                                     (symbol-name scoped-symbol)
                                     "-PROXY")
                        (symbol-package scoped-symbol))))

(defmethod target-icode ((def contained) class target)
  (declare (ignore class))
  (target-code def target))

(defmethod target-icode ((op operation-def) class target)
  (let* ((op-name (op:name op))
         (lisp-name (make-target-symbol target op-name :op)))
    `(defmethod ,lisp-name ((obj ,class) &rest args)
       (apply 'clorb::invoke obj ,op-name args))))

(defmethod target-icode ((op attribute-def) class target)
  (let* ((att-name (op:name op))
         (lisp-name (make-target-symbol target att-name :op)))
    `(progn
       (defmethod ,lisp-name ((obj ,class) &rest)
         (apply 'clorb::invoke obj ,(getter-name att-name)))
       ,@(if (eq (op:mode op) :attr_normal)
             (list `(defmethod (setf ,lisp-name) (newval (obj ,class))
                      (apply 'clorb::invoke obj ,(setter-name att-name)
                             newval)))))))


(defun setter-name (name)
  (concatenate 'string "_set_" name))

(defun getter-name (name)
  (concatenate 'string "_get_" name))

(defmethod gen-type ((obj primitive-def) target)
  (ecase (op:kind obj)
    ;;(:pk_null 'CORBA:null)
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
    ;;(:pk_longdouble 'CORBA:longdouble)
    (:pk_wchar 'CORBA:wchar)
    ;;(:pk_wstring 'CORBA:wstring)
))



(defmethod gen-type ((obj sequence-def) target)
  `sequence)

(defmethod gen-type ((obj alias-def) target)
  (scoped-target-symbol target obj))


(defmethod target-code ((x contained) target)
  (mess 4 "Can't generate code for ~A of kind ~S"
        (op:name x)
        (op:def_kind x))
  (describe x))

(defmethod target-code ((x alias-def) target)
  `(deftype ,(scoped-target-symbol target x) ()
     ',(gen-type (op:original_type_def x) target)))

(defun make-progn (l)
  (cons 'progn
        (loop for x in l
            append (if (eq 'progn (car x))
                       (cdr x)
                     (list x)))))

(defmethod target-code ((idef interface-def) target)
  (let ((class (target-proxy-class-symbol target idef)))
    (make-progn 
     (list* (target-defclass target idef)
            (target-defproxyclass target idef)
            (map 'list (lambda (x) (target-icode x class target))
              (op:contents idef :dk_all t))))))

(defmethod target-code ((mdef module-def) target)
  (make-progn (map 'list (lambda (contained)
                           (target-code contained target))
                   (op:contents mdef :dk_All t))))

(defmethod target-code ((sdef struct-def) target)
  `(define-corba-struct ,(scoped-target-symbol target sdef)
       :id ,(op:id sdef)
       :members ,(map 'list 
                  (lambda (smember)
                    (list (make-target-symbol target (op:name smember)
                                             'clorb)
                          nil))
                  (op:members sdef))))

(defmethod target-code ((enum enum-def) target)
  `(deftype ,(scoped-target-symbol target enum) ()
     '(member ,@(map 'list (lambda (name) (make-target-symbol target name :keyword))
                     (op:members enum)))))

(defmethod target-code ((exc exception-def) target)
  `(define-user-exception ,(scoped-target-symbol target exc)
       :id ,(op:id exc)
       :slots ,(map 'list
                 (lambda (smember)
                   (let ((m-name (op:name smember)))
                     (list (make-target-symbol target m-name 'clorb)
                           :initarg (make-target-symbol target m-name :keyword))))
                 (op:members exc))))


(defun stub-code (name)
  (let ((target (make-instance 'code-target)))
    (let ((code (target-code (lookup-name name) target))
          (ignored-packages (list (find-package :keyword)
                                  (find-package :clorb)
                                  (find-package :op))))
      `(,@(loop for package in (slot-value target 'packages)
                unless (member package ignored-packages)
                collect `(defpackage ,(package-name package) (:use)))
        (export (,@(slot-value target 'symbols)))
        ,code))))
