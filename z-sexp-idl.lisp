(in-package :clorb)

(defparameter *idl-directory*
    (or #+(or clisp openmcl) #P"/Users/lenst/src/hacks/idldump/work/"
        #+(and mcl (not openmcl))  #3P"Macintosh HD:Users:lenst:src:hacks:idldump:work:"
        #+allegro  #P"/home/lenst/src/hacks/idldump/work/"
        ))

(defun get-idl-sexp (file)
  (let ((path (merge-pathnames file *idl-directory*))
        (*package* (find-package :clorb)))
    (setq path (make-pathname :type "sexp" :defaults path))
    (with-open-file (in path)
      (read in))))

(defgeneric process-form (trigger args))

(defun process (form)
  (if (consp (car form))
    (process-list form)
    (process-form (car form) (cdr form))))

(defun process-list (forms)
  (loop for form in forms 
        for result = (process form)
        nconc (if (consp result) result (list result))))

(defmacro dx (pattern &body body)
  (let ((decls (loop while (and (consp (car body)) (eq 'declare (caar body)))
                     collect (pop body)))
        (vars 
         (loop for x in (cdr pattern) collect
               (cond ((atom x) `(,x (process (pop args))))
                     ((eq 'quote (car x)) `(,(second x) (pop args)))
                     (t (let* ((var (gensym)) 
                               (name var))
                          (when (consp (car x))
                            (setq name (gensym)  x (car x)) )
                          (setq body `((multiple-value-bind ,x (decode-name ,name)
                                         (declare (ignorable ,@x)) ,@body)))
                          (unless (eql name var)
                            (setq body `((loop for ,name in ,var collect ,@body))))
                          `(,var (pop args))))))))
    `(defmethod process-form ((trigger (eql ',(car pattern))) args)
       ,@(unless vars `((declare (ignorable args))))
       (let ,vars ,@decls ,@body ))))

(defvar *container* (make-instance 'repository))

(defun the-repository ()
  (typecase *container*
    (CORBA:Repository *container*)
    (CORBA:Contained  (op:containing_repository *container*))))

(defun primitive (kind)
  (op:get_primitive (the-repository) kind))

(defun lookup-id (id)
  (op:lookup_id (the-repository) id))

(defun convert-to-array (type-def array-spec)
  (if array-spec
    (op:create_array (the-repository) (car array-spec)
                     (convert-to-array type-def (cdr array-spec)))
    type-def))

(defun decode-name (form)
  ;; (TYPE_ARRAY (IDENT "larr" "IDL:larr:1.0"   ) ((INTEGER 10)))
  (if (eq 'TYPE_ARRAY (first form))
    (multiple-value-bind (name id ver) (decode-name (second form))
      (values name id ver (process-list (third form))))
    (progn
      ;; (IDENT "larr" "IDL:larr:1.0"   )
      (assert (eq 'IDENT (first form)))
      (values (second form) (third form) "1.0"))))



(dx (nil) nil)


(dx (MODULE (name repoid version) 'contents)
  (let ((*container* (or (lookup-id repoid)
                         (op:create_module *container* repoid name version))))
    (process-list contents)))

(dx (INTERFACE (name repoid version) bases 'contents)
  (let ((*container* (or (lookup-id repoid)
                         (op:create_interface *container* repoid name version bases))))
    (setf (op:base_interfaces *container*) bases)    
    (process-list contents)))
  
(dx (IDENT 'name 'repoid)
  ;;(declare (ignore name))
  (or (lookup-id repoid)
      (intern (string-upcase name) :keyword)))

(dx (EXCEPT_DCL (name repoid version) members)
  (op:create_exception *container* repoid name version members))

(dx (OP_DCL 'oneway 'varargs result-type (name id version) parameters raises contexts)
  (declare (ignore varargs))
  (op:create_operation *container* id name version
                       (or result-type (primitive :pk_void))
                       (if (zerop oneway) :op_normal :op_oneway)
                       parameters raises contexts))

(dx (ATTR_DCL 'readonly param-type ((name id ver array)))
  (op:create_attribute *container* id name ver
                       (convert-to-array param-type array)
                       (if (not (zerop readonly)) :attr_readonly :attr_normal)))
  

(dx (PARAM_DCL 'mode type-def (name))
  (omg.org/corba:parameterdescription
   :name name :type_def type-def
   :mode (case mode (IN :param_in) (OUT :param_out) (INOUT :param_inout))))

(dx (TYPE_STRUCT (name id version) members)
    (op:create_struct *container* id name version members))

(dx (MEMBER type ((name id ver array-spec)))
  (CORBA:StructMember :name name 
                      :type_def (convert-to-array type array-spec)))

(dx (TYPE_DCL type-def ((name id version array-spec)))
  (op:create_alias *container* id name version
                   (convert-to-array type-def array-spec)))

(dx (TYPE_STRING bound)
  (if bound
    (op:create_string (the-repository) bound)
    (primitive :pk_string)))

(dx (INTEGER 'n) n)

(dx (TYPE_SEQUENCE element-type bound)
  (op:create_sequence (the-repository) bound element-type))

(dx (TYPE_ENUM (name id version) 'members)
  (op:create_enum *container* id name version (mapcar #'decode-name members)))

(dx (FORWARD_DCL (name id version))
  (op:create_interface *container* id name version  nil))

(dx (CONST_DCL type (name id version) value)
  (omg.org/features:create_constant *container* id name version type value))


(dx (TYPE_INTEGER 'unsigned-flag 'base-type)
  (primitive 
   (elt (ecase base-type
          (SHORT     '(:pk_ushort :pk_short))
          (LONG      '(:pk_ulong  :pk_long))
          (LONGLONG  '(:pk_ulonglong :pk_longlong)))
        unsigned-flag)))

(dx (ANY) (primitive :pk_any))
(dx (BOOLEAN) (primitive :pk_boolean))
(dx (OCTET) (primitive :pk_octet))
(dx (OBJECT) (primitive :pk_objref))
(dx (TYPECODE) (primitive :pk_typecode))

(dx (TYPE_FLOAT 'kind)
  (primitive (ecase kind
               (float :pk_float)
               (double :pk_double))))



(dx (TYPE_UNION (name id ver) discriminator-type members)
  (op:create_union *container* id name ver 
                   discriminator-type members))

(dx (CASE_STMT labels members)
  (loop for member in members nconc
        (if (null labels)
            (list
             (CORBA:UnionMember
              :name (op:name member)
              :label (CORBA:Any :any-typecode CORBA:TC_OCTET
                                :Any-value 0) 
              :type_def (op:type_def member) ))
          (loop for label in labels collect
                (CORBA:UnionMember
                 :name (op:name member)
                 :label label
                 :type_def (op:type_def member) )))))


(defclass libidl-compiler (idl-compiler) ())

(defmethod load-repository ((comp libidl-compiler) *container* file) 
  (process-list (get-idl-sexp file)))

(setq *default-idl-compiler* (make-instance 'libidl-compiler))


(setq *container* (make-instance 'repository))
;(process-list (get-idl-sexp "trader"))
#||
(process-list (get-idl-sexp "x-04"))
;;(process-list (get-idl-sexp "my-query"))
(unless (fboundp 'describe-repo)
  (load "CLORB:SRC;describe-repo"))
(describe-repo *container*)
(doseq (x (op:contents *container* :dk_all nil)) (pprint (omg.org/features:describe x)))
||#
