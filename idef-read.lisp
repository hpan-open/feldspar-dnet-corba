(in-package :clorb)

(defvar *idef-read-agenda* nil)
(defvar *idef-current-prefix* nil)

(defun idef-read (sexps repository)
  (let ((*idef-read-agenda* nil))
    (idef-read-contents sexps repository)
    (loop while *idef-read-agenda*
        do (funcall (pop *idef-read-agenda*)))))


(defun idef-read-contents (list container)
  (dolist (sexp list)
    (let ((todo
           (idef-read-part (first sexp) (rest sexp) container)))
      (when todo (push todo *idef-read-agenda*)))))


(defmethod idef-read-part ((op (eql 'with-prefix))
                           sexp container)
  (destructuring-bind (prefix &rest forms) sexp
    (let ((*idef-current-prefix* prefix))
      (idef-read-contents forms container)
      nil)))


(defmethod idef-read-part ((op (eql 'define-module))
                           sexp container)
  (destructuring-bind (name (&key version id) &rest forms) sexp
    (setq name (string name))
    (let ((module (op:lookup container name)))
      (unless module
        (setq module (make-instance-in container 'module-def
                                       :name name
                                       :version (or version "1.0")
                                       :id id)))
      (idef-read-contents forms module)
      nil)))


(defmethod idef-read-part ((op (eql 'define-interface))
                           sexp container)
  (destructuring-bind (name (&key bases id version) &rest forms) sexp
    (setq name (string name))
    (let ((idef (make-instance-in container 'interface-def 
                                  :name name :version version :id id)))
      (idef-read-contents forms idef)
      (lambda ()
        (setf (op:base_interfaces idef)
              (mapcar #'(lambda (i-name)
                          (lookup-name-in container i-name))
                      bases))))))



(defmethod idef-read-part ((op (eql 'define-type)) sexp container)
  (destructuring-bind (name typespec) sexp
    (setq name (string name))
    (let ((alias (make-instance-in container 'alias-def :name name)))
      (lambda ()
        (setf (op:original_type_def alias)
          (parse-type-in container typespec))))))


(defmethod idef-read-part ((op (eql 'define-operation)) sexp container)
  (destructuring-bind (name params &key result-type exceptions version id)
      sexp
    (setq name (string name))
    (let ((op (make-instance-in container 'operation-def 
                                :name name :version version :id id)))
      (lambda ()
        (setf (op:params op)
          (loop for p in params 
              for type-def = (parse-type-in container (third p))
              collect (CORBA:ParameterDescription
                       :name (string (second p))
                       :mode (first p)
                       :type_def type-def)))
        (setf (op:result_def op)
          (parse-type-in container (or result-type 'void)))
        (setf (op:exceptions op)
          (mapcar #'(lambda (name)
                      (lookup-name-in container name))
                  exceptions))))))


(defmethod idef-read-part ((op (eql 'define-enum)) sexp container)
  (destructuring-bind (name members &key version id) sexp
    (setq name (string name))
    (make-instance-in container 'enum-def 
                      :name name :members members
                      :version version :id id)
    nil))

(defmethod idef-read-part ((op (eql 'define-struct)) sexp container)
  (destructuring-bind (name members &key version id) sexp
    (setq name (string name))
    (let ((def (make-instance-in container 'struct-def 
                                 :name name :version version :id id)))
      (lambda ()
        (setf (member-list def)
          (mapcar (lambda (x)
                    (cons (string (first x))
                          (parse-type-in container (second x))))
                  members))))))


(defmethod idef-read-part ((op (eql 'define-exception)) sexp container)
  (destructuring-bind (name members &key version id) sexp
    (setq name (string name))
    (let ((def (make-instance-in container 'exception-def 
                                 :name name :version version :id id)))
      (lambda ()
        (setf (op:members def)
          (mapcar (lambda (x)
                    (let ((type (parse-type-in container (second x))))
                      (make-struct 
                       "IDL:omg.org/CORBA/StructMember:1.0"
                       :name (first x)
                       :type_def type
                       :type nil)))
                  members))))))


(defmethod idef-read-part ((op (eql 'define-attribute)) sexp container)
  (destructuring-bind (name type &key readonly) sexp
    (setq name (string name))
    (let ((def (make-instance-in container 'attribute-def 
                                 :name name :mode (if readonly 
                                                      :ATTR_READONLY
                                                    :ATTR_NORMAL))))
      (lambda ()
        (setf (op:type_def def)
          (parse-type-in container type))))))


(defmethod idef-read-part ((op (eql 'define-constant)) sexp container)
  (destructuring-bind (name type value &key id version) sexp
    (let ((def (make-instance-in container 'constant-def 
                                 :name name :value value
                                 :id id :version version)))
      (lambda ()
        (setf (op:type_def def)
          (parse-type-in container type))))))


(defun make-instance-in (container class &rest args
                         &key name version id
                         &allow-other-keys)
  (setq name (string name))
  (setq version (or version "1.0"))
  (setq id (or id
               (apply #'concatenate 'string
                      "IDL:"
                      `(,@(if *idef-current-prefix* 
                              (list *idef-current-prefix* "/"))
                          ,@(nreverse (repo-path container))
                          ,name ":" ,version))) )
  (let ((obj (apply #'make-instance class
                    :name name
                    :id id 
                    :version version
                    :defined_in container
                    args)))
    (addto container obj)
    obj))

(defun repo-path (module)
  (if (and module (not (typep module 'repository)))
      (list*
        "/" (op::name module)
        (repo-path (op::defined_in module)))
    nil))


(defun parse-type-in (container type-sexp)
  (cond
   ((symbolp type-sexp)
    (get-primitive type-sexp))
   ((stringp type-sexp)
    (lookup-name-in container type-sexp))
   ((and (consp type-sexp) (eq (car type-sexp) 'sequence))
    (destructuring-bind (member-type &optional (bound 0))
        (cdr type-sexp)
      (make-instance 'sequence-def
        :bound (or bound 0)
        :element_type_def (parse-type-in container member-type))))
   ((and (consp type-sexp) (member (car type-sexp) '(string wstring)))
    (destructuring-bind (string-type &optional (bound 0))
        (cdr type-sexp)
      (make-instance (if (eq string-type 'string)
                      'string-def
                      'wstring-def)
        :bound bound)))
   (t
    (error "Illegal type spec: ~S" type-sexp))))

(define-method defined_in ((r repository))
  nil)

(defun lookup-name-in (container qname 
                       &optional (default nil no-error-p))
  "Find an object given its qualified name"
  (let ((parts (parse-name qname))
        (start-container container))
    (when (eql :absolute (car parts))
      (setq start-container (repository-of container)
            parts (cdr parts))
      (unless parts (error "Illegal name: ~A" qname)))
    (let ((object
           (loop 
               for container = start-container then (op::defined_in container)
               while container
               thereis (op::lookup container (car parts)))))
      ;; FIXME: if object still nil, should the name be looked up in
      ;; the CORBA module?
      (loop while object
          do (setq parts (cdr parts))
          while parts 
          do (setq object (op::lookup object (car parts))))
      (if object
          object
        (if no-error-p
            default
          (error "Name '~A' (of '~A') not found" (car parts) qname))))))


(defun repository-of (container)
  (loop while (not (typep container 'repository))
      do (setf container (op:defined_in container)))
  container)

(defun parse-name (name)
  (loop with parts = '()
      for cp = (position #\: name)
      while cp
      do (progn
           (push (if (zerop cp) :absolute
                   (subseq name 0 cp)) parts)
           (loop while (eql #\: (elt name cp)) do (incf cp))
           (setq name (subseq name cp)))
      finally (return 
                (nreverse (cons name parts)))))

(defun get-primitive (name)
  (multiple-value-bind (pk tc)
      (ecase name
        (null (values :pk_null CORBA:tc_null))
        (void (values :pk_void CORBA:tc_void))
        (short (values :pk_short CORBA:tc_short))
        (long (values :pk_long CORBA:tc_long))
        (ushort (values :pk_ushort CORBA:tc_ushort))
        (ulong (values :pk_ulong CORBA:tc_ulong))
        (float (values :pk_float CORBA:tc_float))
        (double (values :pk_double CORBA:tc_double))
        (boolean (values :pk_boolean CORBA:tc_boolean))
        (char (values :pk_char CORBA:tc_char))
        (octet (values :pk_octet CORBA:tc_octet))
        (any (values :pk_any CORBA:tc_any))
        (TypeCode (values :pk_TypeCode CORBA:tc_TypeCode))
        ;;(Principal (values :pk_Principal CORBA:tc_Principal))
        (string (values :pk_string CORBA:tc_string))
        (object (values :pk_objref CORBA:tc_objref))
        (longlong (values :pk_longlong CORBA:tc_longlong))
        (ulonglong (values :pk_ulonglong CORBA:tc_ulonglong))
        (longdouble (values :pk_longdouble CORBA:tc_longdouble))
        (wchar (values :pk_wchar CORBA:tc_wchar))
        (wstring (values :pk_wstring CORBA:tc_wstring)))
    (make-instance 'primitive-def
      :kind pk
      :type tc)))
