(in-package :clorb)

(defun make-corba-symbol (name package)
  (let* ((package
          (or #+clisp (if (packagep package) package)
              (find-package package)
              (make-package package :use '())))
         (symbol (intern (string-upcase name) package)))
    (export symbol package)
    symbol))

(defun scoped-symbol (obj)
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
              (symbol-name (scoped-symbol container)) "/" (op:name obj))))
         (package
          (if (eq this-type :dk_Module)
              :keyword
            ;; Find enclosing Module
            (do* ((container container (op:defined_in container))
                  (container-type container-type
                                  (and container (op:def_kind container))))
                ((or (null container) (eq container-type :dk_Module))
                 (if container (scoped-symbol container) 
                   "OMG.ORG/ROOT"))))))
    (make-corba-symbol name package)))


(defun gen-defclass (idef)
  (let ((bases (op:base_interfaces idef)))
    `(defclass ,(scoped-symbol idef) 
         ,(if bases 
              (map 'list 'scoped-symbol bases)
            '(CORBA:Object))
       ())))

(defun proxy-class-symbol (idef)
  (let ((scoped-symbol (scoped-symbol idef)))
    (make-corba-symbol (concatenate 'string
                         (symbol-name scoped-symbol)
                         "-PROXY")
                       (symbol-package scoped-symbol))))


(defun gen-defproxyclass (idef)
  (let ((bases (op:base_interfaces idef))
        (class-symbol (proxy-class-symbol idef)))
    `(progn
       (defclass ,class-symbol
         ,(if bases 
              (cons (scoped-symbol idef)
                    (map 'list 'proxy-class-symbol bases))
            (list (scoped-symbol idef) 'CORBA:Proxy))
         ())
       (register-proxy-class ,(op:id idef) ',class-symbol))))


(defun make-feature-symbol (name)
  (let ((symbol (make-corba-symbol name :op)))
    (unless (get symbol 'proxy-def)
      (eval `(defmethod ,symbol ((obj CORBA:Proxy) &rest args)
               (apply 'clorb::invoke obj ,name args)))
      (setf (get symbol 'proxy-def) t))
    symbol))




(defmethod gen-icode ((def contained) class)
  (declare (ignore class))
  (gen-code def))

(defmethod gen-icode ((op operation-def) class)
  (let* ((op-name (op:name op))
         (lisp-name (make-corba-symbol op-name :op)))
    `(defmethod ,lisp-name ((obj ,class) &rest args)
       (apply 'clorb::invoke obj ,op-name args))))

(defmethod gen-icode ((op attribute-def) class)
  (let* ((att-name (op:name op))
         (lisp-name (make-corba-symbol att-name :op)))
    `(progn
       (defmethod ,lisp-name ((obj ,class) &rest args)
         (apply 'clorb::invoke obj ,(getter-name att-name)))
       ,@(if (eq (op:mode op) :attr_normal)
             (list `(defmethod (setf ,lisp-name) (newval (obj ,class))
                      (apply 'clorb::invoke obj ,(setter-name att-name)
                             newval)))))))


(defun setter-name (name)
  (concatenate 'string "_set_" name))

(defun getter-name (name)
  (concatenate 'string "_get_" name))

(defmethod gen-type ((obj primitive-def))
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




(defmethod gen-type ((obj sequence-def))
  `sequence)

(defmethod gen-type ((obj alias-def))
  (scoped-symbol obj))


(defmethod gen-code ((x contained))
  (mess 4 "Can't generate code for ~A of kind ~S"
        (op:name x)
        (op:def_kind x))
  (describe x))

(defmethod gen-code ((x alias-def))
  `(deftype ,(scoped-symbol x) ()
     ',(gen-type (op:original_type_def x))))

(defun make-progn (l)
  (cons 'progn
        (loop for x in l
            append (if (eq 'progn (car x))
                       (cdr x)
                     (list x)))))

(defmethod gen-code ((idef interface-def))
  (let ((class (proxy-class-symbol idef)))
    (make-progn 
     (list* (gen-defclass idef)
            (gen-defproxyclass idef)
            (map 'list (lambda (x) (gen-icode x class))
              (op:contents idef :dk_all t))))))

(defmethod gen-code ((mdef module-def))
  (make-progn (map 'list 'gen-code (op:contents mdef :dk_All t))))

(defmethod gen-code ((sdef struct-def))
  `(define-corba-struct ,(scoped-symbol sdef)
       :id ,(op:id sdef)
       :members ,(map 'list 
                  (lambda (smember)
                    (list (make-corba-symbol (struct-get smember :name)
                                             'clorb)
                          nil))
                  (op:members sdef))))

(defmethod gen-code ((enum enum-def))
  `(deftype ,(scoped-symbol enum) ()
     '(member ,@(map 'list (lambda (name) (make-corba-symbol name :keyword))
                     (op:members enum)))))

(defmethod gen-code ((exc exception-def))
  `(define-user-exception ,(scoped-symbol exc)
       :id ,(op:id exc)
       :slots ,(map 'list
                 (lambda (smember)
                   (let ((m-name (struct-get smember :name)))
                     (list (make-corba-symbol m-name 'clorb)
                           :initarg (make-corba-symbol m-name :keyword))))
                 (op:members exc))))


(defmethod gen-code ((name string))
  (gen-code (lookup-name name)))
