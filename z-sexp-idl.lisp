(in-package :clorb)

(defparameter *idl-directory*
  (or (make-pathname :directory '(:relative "src" "corba" "interfaces")
                     :defaults common-lisp-user::*current-user-folder* )))

(defparameter *tmp-directory*
  (or #+unix "/tmp"
      #+mcl (ccl::FindFolder #$kOnAppropriateDisk #$kUserSpecificTmpFolderType)
      ))

#+(or)
(defun pathname-unixname (pathname)
  (ccl::posix-namestring pathname)
  #+(or)
  (with-output-to-string (s)
    (loop for dir in (pathname-directory pathname)
          do (cond ((eq :absolute dir) (princ "/" s))
                   ((equal dir "Macintosh HD") )
                   (t (princ dir s) (princ "/" s))))
    (princ (pathname-name pathname) s)
    (princ "." s)
    (princ (pathname-type pathname) s)))

(defun get-idl-sexp (file)
  (let* ((idl-file (merge-pathnames file *idl-directory*))
         (sexp-file (make-pathname :name (pathname-name idl-file)
                                   :type "sexp"
                                   :defaults *tmp-directory*)))
    (assert (probe-file idl-file))
    (when (probe-file sexp-file)
      (delete-file sexp-file))
    (let ((result
           (shell-to-string-or-stream
            (format nil "/Users/lenst/src/hacks/idldump/idldump ~A 2>&1 > ~A"
                    (external-namestring idl-file)
                    (external-namestring sexp-file)))))
      (unless (or (null result)
                  (equal result ""))
        (format t "~&PARSE: ~A~%" result)))
    (assert (probe-file sexp-file))
    (with-open-file (in sexp-file)
      (let ((sexp (let ((*package* (find-package :clorb)))
                    (read in))))
         (when (symbolp sexp)
           (error "CORBA:IDL:~A ~A" sexp (read-line in)))
         sexp))))


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

(dx (TYPE_STRUCT (name id version) 'members)
  ;; Complicated to handle recursive types
  (let ((struct (op:create_struct *container* id name version nil)))
    (setf (op:members struct) (process-list members))
    struct))

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

(dx (TYPE_WIDE_STRING bound)
  (if bound
    (omg.org/features:create_wstring (the-repository) bound)
    (primitive :pk_wstring)))

(dx (INTEGER 'n) n)
(dx (STRING 's) s)

(dx (CHAR 'ch) 
  (cond ((null ch) (primitive :pk_char))
        (t (corba:any :any-value (char ch 0)
                      :any-typecode CORBA:tc_char))))

(dx (WIDE_CHAR 'ch) 
  (cond ((null ch) (primitive :pk_wchar))
        (t (corba:any :any-value (char ch 0)
                      :any-typecode CORBA:tc_wchar))))

(dx (BOOLEAN 'b) 
  (cond ((null b) (primitive :pk_boolean))
        ((eql b 0) nil)
        ((eql b 1) t)
        (t (warn "strange form (BOOLEAN ~S)" b)
           nil)))


(dx (TYPE_SEQUENCE element-type bound)
  (op:create_sequence (the-repository) (or bound 0) element-type))

(dx (TYPE_ENUM (name id version) 'members)
  (op:create_enum *container* id name version (mapcar #'decode-name members)))

(dx (FORWARD_DCL (name id version))
  (op:create_interface *container* id name version  nil))

(dx (CONST_DCL type (name id version) value)
  (op:create_constant *container* id name version type 
                      (corba:any :any-typecode (op:type type)
                                 :any-value (any-value value))))


(dx (TYPE_INTEGER 'unsigned-flag 'base-type)
  (primitive 
   (elt (ecase base-type
          (SHORT     '(:pk_ushort :pk_short))
          (LONG      '(:pk_ulong  :pk_long))
          (LONGLONG  '(:pk_ulonglong :pk_longlong)))
        unsigned-flag)))

(dx (ANY) (primitive :pk_any))

(dx (OCTET) (primitive :pk_octet))
(dx (OBJECT) (primitive :pk_objref))
(dx (TYPECODE) (primitive :pk_typecode))

(dx (TYPE_FLOAT 'kind)
  (primitive (ecase kind
               (float :pk_float)
               (double :pk_double)
               (longdouble :pk_longdouble))))

(dx (TYPE_FIXED digits scale)
  (op:create_fixed (the-repository) digits scale))

(dx (TYPE_UNION (name id ver) discriminator-type members)
  (op:create_union 
   *container* id name ver 
   discriminator-type 
   (map 'vector 
        (lambda (member)
          (if (typep (op:label member) 'CORBA:Any)
            member
            (CORBA:UnionMember
             :name (op:name member)
             :label (corba:any :any-typecode (op:type discriminator-type)
                               :any-value (op:label member))
             :type_def (op:type_def member))))
        members)))

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

(unless *default-idl-compiler*
  (setq *default-idl-compiler* (make-instance 'libidl-compiler)))


#||
(setq *container* (make-instance 'repository))
(process-list (get-idl-sexp "trader.idl"))
(process-list (get-idl-sexp "PortableServer.idl"))
(process-list (get-idl-sexp "x-04.idl"))
(process-list (get-idl-sexp "my-query.idl"))
(unless (fboundp 'describe-repo)
  (load "CLORB:SRC;describe-repo"))
(describe-repo *container*)
(doseq (x (op:contents *container* :dk_all nil)) (pprint (op:describe x)))
||#

