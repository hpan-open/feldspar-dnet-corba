;;;; clorb-target.lisp -- Code Generator for CLORB

(in-package :clorb)

(defclass code-target ()
  ((packages :initform nil)
   (symbols  :initform nil)
   (defs     :initform nil)
   (dynamic-stubs :initform nil
                  :initarg :dynamic-stubs
                  :accessor target-dynamic-stubs)
   (dynamic-servant
    :initarg :dynamic-servant
    :initform nil
    :reader target-dynamic-servant )
   (struct-marshal
    :initarg :struct-marshal
    :initform t
    :reader target-struct-marshal )))

(defgeneric target-typecode (obj target)
  (:documentation
   "The target code to compute the typecode for idltype object."))

(defgeneric target-code (obj target)
  (:documentation
   "The target code defining the object."))

(defgeneric target-type (idltype target)
  (:documentation
   "Lisp type mapping for IDL-type."))



(defun param-symbol (parameterdesc)
  (intern (format nil "_~:@(~A~)" (op:name parameterdesc)) :clorb))

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


(defun target-base-list (target bases make-symbol-fun root-class)
  (if (zerop (length bases))
      `(,root-class)
      (map 'list (lambda (base)
                   (funcall make-symbol-fun target base))
           bases)))

(defun target-class-symbol (target idef suffix)
  (let ((scoped-symbol (scoped-target-symbol target idef)))
    (make-target-symbol target
                        (concatenate 'string
                                     (symbol-name scoped-symbol)
                                     suffix )
                        (symbol-package scoped-symbol))))

(defun target-proxy-class-symbol (target idef)
  (target-class-symbol target idef "-PROXY"))

(defun target-servant-class-symbol (target idef)
  (target-class-symbol target idef "-SERVANT"))


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
    `(define-alias ,symbol
       :id ,(op:id x)
       :name ,(op:name x)
       :type ,(target-type (op:original_type_def x) target)
       :typecode ,(target-typecode (op:original_type_def x) target) )))

(defmethod target-code ((const CORBA:ConstantDef) target)
  `(defconstant ,(scoped-target-symbol target const)
     ',(op:value const)))

(defmethod target-code ((idef CORBA:InterfaceDef) target)
  (make-progn
   (list 
    (let ((bases (op:base_interfaces idef))
          (class-symbol (scoped-target-symbol target idef))
          (proxy-symbol (target-proxy-class-symbol target idef)))
      `(define-interface ,class-symbol
         ,(target-base-list target bases #'scoped-target-symbol 'CORBA:Object)
         :proxy ,(list* proxy-symbol class-symbol
                        (target-base-list target bases #'target-proxy-class-symbol 
                                          'CORBA:Proxy))
         :id ,(op:id idef)
         :name ,(op:name idef)))
    
    (call-next-method))))


;;; Stub --------------------------------------------

(defun make-dynamic-stub-1 (op target)
  (let* ((op-name (op:name op))
         (class (target-proxy-class-symbol target (op:defined_in op)))
         (lisp-name (make-target-symbol target op-name :op)))
    `(defmethod ,lisp-name ((obj ,class) &rest args)
       (apply #'corba:funcall ,op-name obj args))))

(defun make-dynamic-stub-2 (op target)
  (let* ((op-name (op:name op))
         (class (target-proxy-class-symbol target (op:defined_in op)))
         (lisp-name (string-upcase op-name))
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
         (request-funcall _request)))))

(defun make-static-stub (opdef target)
  (let* ((params (coerce (op:params opdef) 'list))
         (in-params (loop for p in params unless (eq :param_out (op:mode p)) collect p))
         (out-params (loop for p in params unless (eq :param_in (op:mode p)) collect p)))
    (unless (eq :tk_void (op:kind (op:result opdef)))
      (push (omg.org/corba:parameterdescription
             :type_def (op:result_def opdef))
            out-params))
    `(define-method ,(string-upcase (op:name opdef))
                    ((obj ,(target-proxy-class-symbol target (op:defined_in opdef)))
                     ,@(mapcar #'param-symbol in-params))
       (static-call 
        (,(op:name opdef) obj)
        :output ((output)
                 ,@(loop for param in in-params
                         collect (target-marshal (op:type_def param) target
                                                 (param-symbol param)
                                                 'output)))
        :input ((input)
                ,@(loop for param in out-params
                        collect (target-unmarshal (op:type_def param) target 'input)))
        :exceptions ,(map 'list
                          (lambda (edef) (scoped-target-symbol target edef))
                          (op:exceptions opdef))))))


(defmethod target-code ((op CORBA:OperationDef) target)
  (case (target-dynamic-stubs target)
    (1
     (make-dynamic-stub-1 op target))
    ((2 t) 
     (make-dynamic-stub-2 op target))
    ((nil)
     (make-static-stub op target))))


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

(defmethod target-sort-key ((def CORBA:InterfaceDef))
  (1+ (reduce #'max (map 'list #'target-sort-key (op:base_interfaces def))
              :initial-value 0)))

(defmethod target-sort-key ((x t)) 0)

(defmethod target-code ((mdef CORBA:Container) target)
  (make-progn (map 'list (lambda (contained)
                           (target-code contained target))
                   (sort (op:contents mdef :dk_All t)
                         #'<  :key #'target-sort-key ))))


(defmethod target-code ((sdef CORBA:StructDef) target)
  (let ((sym (scoped-target-symbol target sdef)))
    `(progn
       (define-struct ,sym
         :id ,(op:id sdef)
         :name ,(op:name sdef)
         :members ,(map 'list
                        (lambda (smember)
                          (list (op:name smember)
                                (target-typecode (op:type_def smember) target)
                                (make-target-symbol target (op:name smember)
                                                    'clorb)))
                        (op:members sdef))
         ,@(if (target-struct-marshal target)
             `(
               :read ((buffer) 
                      (,sym
                       ,@(loop for member in (coerce (op:members sdef) 'list)
                               collect (lispy-name (op:name member))
                               collect (target-unmarshal (op:type_def member) target 'buffer))))
               :write ((obj buffer)
                       ,@(loop for member in (coerce (op:members sdef) 'list)
                               collect (target-marshal (op:type_def member) target
                                                       `(,(feature (op:name member)) obj)
                                                       'buffer)))))))))


(defmethod target-code ((enum CORBA:EnumDef) target)
  `(define-enum ,(scoped-target-symbol target enum)
     :id ,(op:id enum)
     :name ,(op:name enum)
     :members ,(coerce (op:members enum) 'list)))


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

;;;; Marshalling

;;; Marshal 

(defmethod target-marshal ((def CORBA:IRObject) target obj buffer)
  `(marshal ,obj ,(target-typecode def target) ,buffer))

(defmethod target-marshal ((alias CORBA:AliasDef) target obj buffer)
  (target-marshal (op:original_type_def alias) target obj buffer))

(defmethod target-marshal ((def omg.org/corba:primitivedef) target obj buffer)
  (declare (ignore target))
  (let ((func (case (op:kind def)
                (:pk_string 'marshal-string)
                (:pk_long   'marshal-long)
                (:pk_ulong  'marshal-ulong)
                (:pk_short  'marshal-short)
                (:pk_ushort 'marshal-ushort)
                (:pk_octet  'marshal-octet)
                (:pk_boolean 'marshal-bool))))
    (if func
      `(,func ,obj ,buffer)
      (call-next-method))))

(defmethod target-marshal ((seqdef omg.org/corba:sequencedef) target obj buffer)
  (let ((el-marshal (target-marshal (op:element_type_def seqdef) target 'obj 'buffer)))
    (cond ((equal el-marshal '(marshal-octet obj buffer))
           `(marshal-osequence ,obj ,buffer))
          ((and (eq 'obj (second el-marshal))
                (eq 'buffer (third el-marshal))
                (null (cdddr el-marshal)))
           ;; i.e. (marshal-xx obj buffer)
           `(marshal-sequence ,obj #',(first el-marshal) ,buffer))
          (t
           `(marshal-sequence ,obj (lambda (obj buffer) ,el-marshal) ,buffer)))))

(defmethod target-marshal ((def omg.org/corba:structdef) target obj buffer)
  `(struct-write ,obj ',(scoped-target-symbol target def) ,buffer))


;;; Unmarshal

(defmethod target-unmarshal ((def CORBA:IRObject) target buffer)
  `(unmarshal ,(target-typecode def target) ,buffer))

(defmethod target-unmarshal ((def omg.org/corba:primitivedef) target buffer)
  (declare (ignore target))
  (let ((func (case (op:kind def)
                (:PK_STRING 'unmarshal-string)
                (:pk_long   'unmarshal-long)
                (:pk_ulong  'unmarshal-ulong)
                (:pk_short  'unmarshal-short)
                (:pk_ushort 'unmarshal-ushort)
                (:pk_octet  'unmarshal-octet)
                (:pk_boolean 'unmarshal-bool))))
    (if func
      `(,func ,buffer)
      (call-next-method))))

(defmethod target-unmarshal ((alias CORBA:AliasDef) target buffer)
  (target-unmarshal (op:original_type_def alias) target buffer))

(defmethod target-unmarshal ((seqdef omg.org/corba:sequencedef) target buffer)
  (let ((el-unmarshal (target-unmarshal (op:element_type_def seqdef) target 'buffer)))
    (cond ((equal el-unmarshal '(unmarshal-octet buffer))
           `(unmarshal-osequence ,buffer))
          ((and (eq 'buffer (second el-unmarshal))
                (null (cddr el-unmarshal)))
           ;; i.e. (unmarshal-xx buffer)
           `(unmarshal-sequence #',(first el-unmarshal) ,buffer))
          (t
           `(unmarshal-sequence (lambda (buffer) ,el-unmarshal) ,buffer)))))
    
(defmethod target-unmarshal ((seqdef corba:structdef) target buffer)
  `(struct-read ',(scoped-target-symbol target seqdef) ,buffer))


;;;; Servants

(defmethod target-servant ((def CORBA:IRObject) target)
  (declare (ignore target))
  nil)

(defmethod target-servant ((def CORBA:Container) target)
  (make-progn (map 'list (lambda (contained)
                           (target-servant contained target))
                   (sort (op:contents def :dk_All t)
                         #'<  :key #'target-sort-key ))))


(defmethod target-servant ((idef CORBA:InterfaceDef) target)
  (let ((bases (op:base_interfaces idef))
        (class-symbol (target-servant-class-symbol target idef)))
    `(progn
       (define-corba-class ,class-symbol
         ,(list* (scoped-target-symbol target idef)
                 (target-base-list target bases #'target-servant-class-symbol
                                   (if (target-dynamic-servant target)
                                     'PortableServer:DynamicImplementation
                                     'PortableServer:Servant)))
         :attributes
         (,@(map 'list
                 (lambda (attdef)
                   (list (intern (string-upcase (op:name attdef)) :clorb)
                         (if (eq (op:mode attdef) :attr_readonly)
                           :readonly )))
                 (op:contents idef :dk_attribute t)) ))
       ,(if (target-dynamic-servant target)
          (make-dynamic-servant idef target class-symbol)
          (make-static-servant idef target class-symbol)))))



(defun make-static-servant (idef target class-symbol)
  `(defmethod servant-invoke ((servant ,class-symbol) operation input handler)
     (cond ,@(mapcan #'identity
                     (map 'list
                          (lambda (def) (target-invoke-static def target))
                          (op:contents idef :dk_all t)))
           (t 
            (call-next-method)))))


(defun target-handle-exception-static (exc-def target)
  `(,(scoped-target-symbol target exc-def)
    (exc)
    (declare (ignorable exc))
    (let ((output (funcall handler :user_exception)))
      (marshal-string ,(op:id exc-def) output)
      ,@(map 'list 
             (lambda (member) 
               (target-marshal (op:type_def member) target
                               (list (feature (op:name member)) 'exc)
                               'output))
             (op:members exc-def))
      output )))

(defmethod target-invoke-static ((def CORBA:OperationDef) target)
  (let ((params (coerce (op:params def) 'list))
        (result (if (not (eq :tk_void (op:kind (op:result def))))
                  '(result))))
    (list
     `((string= operation ,(op:name def))
       (handler-case
         (multiple-value-bind (,@result
                               ,@(loop for p in params
                                       unless (eq :param_in (op:mode p))
                                       collect (param-symbol p)))
                              (,(feature (op:name def)) servant
                               ,@(loop for p in params
                                       unless (eq :param_out (op:mode p))
                                       collect (target-unmarshal (op:type_def p) target 'input)))
           
           (let ((output (funcall handler :no_exception)))
             ,(if result
                (target-marshal (op:result_def def) target (car result) 'output))
             ,@(loop for p in params
                     unless (eq :param_in (op:mode p))
                     collect (target-marshal (op:type_def p) target (param-symbol p) 'output))
             output))
         ,@(map 'list
                (lambda (exc-def) (target-handle-exception-static exc-def target))
                (op:exceptions def )))))))

(defmethod target-invoke-static ((def CORBA:AttributeDef) target)
  (let ((name (op:name def)))
    (list*
     `((string= operation ,(getter-name name))
       (let ((result (,(feature name) servant))
             (output (funcall handler :no_exception)))
         ,(target-marshal (op:type_def def) target 'result 'output)
         output)) 
     (if (eq :attr_normal (op:mode def))
       (list `((string= operation ,(setter-name name))
               (setf (,(feature name) servant)
                     ,(target-unmarshal (op:type_def def) target 'input))
               (funcall handler :no_exception)))))))




(defmethod target-invoke-static ((def CORBA:IRObject) target)
  (declare (ignore target))
  nil)

(defun make-dynamic-servant (idef target class-symbol)
  `(define-method "INVOKE" ((servant ,class-symbol) request)
     (let ((op (op:operation request)))
       (cond ,@(remove nil
                       (map 'list
                            (lambda (def) (target-invoke-dynamic def target))
                            (op:contents idef :dk_all t)))
             (t 
              (call-next-method))))))

(defmethod target-invoke-dynamic ((def CORBA:IRObject) target)
  (declare (ignore target))
  nil)

(defmethod target-invoke-dynamic ((def CORBA:OperationDef) target)
  (let ((params (op:params def)))
    `((string= op ,(op:name def))
      (let (,@(map 'list 
                   (lambda (pd)
                     (list (param-symbol pd)
                           `(CORBA:Any :any-typecode ,(target-typecode (op:type_def pd) target))))
                   params))
        (op:arguments request
                      (list ,@(map 'list
                                   (lambda (pd)
                                     `(CORBA:NamedValue :argument ,(param-symbol pd)
                                                        :arg_mode ,(case (op:mode pd)
                                                                     (:param_in 'ARG_IN)
                                                                     (:param_out 'ARG_OUT)
                                                                     (:param_inout 'ARG_INOUT))))
                                   params) ))
        (multiple-value-bind (_res xx)
                             (,(make-target-symbol target (op:name def) :op)
                              ,@(remove nil
                                        (map 'list 
                                             (lambda (pd)
                                               (unless (eq (op:mode pd) :param_out)
                                                 `(any-value ,(param-symbol pd))))
                                             params)))
          (op:set_result request _res))))))



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



(defun gen-skel-file (object filename &key dynamic-servant package-def)
  (with-open-file (*standard-output* filename :direction :output :if-exists :supersede)
    (let* ((target (make-instance 'code-target :dynamic-servant dynamic-servant))
           (code (target-servant object target)))
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

(defun pprint-def2-and-keys (*standard-output* list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-exit-if-list-exhausted)
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
                                    define-struct define-union define-enum
                                    define-alias
                                    static-call))
                     'pprint-def-and-keys)

(set-pprint-dispatch '(cons (member define-interface))
                     'pprint-def2-and-keys)

#||
(load "clorb:src;iop-idl")
(gen-stub-file (lookup-name "IOP") "clorb:x-iop.lisp")
(gen-stub-file (lookup-name "CosNaming") "clorb:x-cosnaming-stub.lisp" :package-def t)
(gen-skel-file (lookup-name "CosNaming") "clorb:x-cosnaming-skel.lisp" :package-def t)

(load "clorb:src;ifr-idl")
(gen-stub-file (lookup-name "CORBA") "clorb:x-ifr-base.lisp")

(gen-stub-file (vsns-get "clorb") "clorb:x-orb-base.lisp")
(gen-stub-file (vsns-get "ir") "clorb:x-ifr-base.lisp")
(gen-stub-file (vsns-get "file.i") "clorb:x-file.lisp")


(load "clorb:src;file-idl")
(gen-stub-file (lookup-name "poa") "clorb:x-file.lisp" :package-def t :dynamic-stubs nil)

(defvar *target* (make-instance 'code-target))
(target-code (lookup-name "CosNaming") *target*)
(target-servant (lookup-name "CosNaming::NamingContext") *target*)
(target-servant (lookup-name "CORBA::SequenceDef") *target*)
||#

;;;; clorb-target.lisp ends here
