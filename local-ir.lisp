(in-package :clorb)


(deftype DefinitionKind  ()
  '(member :dk_none :dk_all
    :dk_Attribute :dk_Constant :dk_Exception :dk_Interface
    :dk_Module :dk_Operation :dk_Typedef
    :dk_Alias :dk_Struct :dk_Union :dk_Enum
    :dk_Primitive :dk_String :dk_Sequence :dk_Array
    :dk_Repository
    :dk_Wstring :dk_Fixed))


;;;; Registry of Ir class for DefinitionKind

(defun register-ir-class (def-kind class)
  (setf (get def-kind 'ir-class) class))

(defun get-ir-class (def-kind)
  (or (get def-kind 'ir-class)
      (error "No class defined for definition kind ~A" def-kind)))


;;;; Generics

(defgeneric addto (container contained)
  (:documentation "Add a contained to a container")
  (:method ((c null) x) x))


;;;; Base Clases

(defmacro define-ir-class (name supers 
                           &rest args 
                           &key id def_kind &allow-other-keys)
  (setq args
    (loop for x on args by #'cddr
        unless (member (car x) '(:id :def_kind))
        nconc (list (car x) (cadr x))))
  `(progn (define-corba-class ,name ,supers ,@args)
          ,@(if def_kind 
                `((define-method def_kind ((x ,name)) ,def_kind)
                  (register-ir-class ,def_kind ',name)))
          ,@(if id
                `((defmethod servant-interface-id ((servant ,name))
                    ,id)))))


(define-ir-class IRObject (auto-servant)
  :id "IDL:omg.org/CORBA/IRObject:1.0"
  :attributes ()
  :defaults ())

(define-ir-class IDLType (IRObject)
  :id "IDL:omg.org/CORBA/IDLType:1.0"
  :attributes ()
  :slots ((type :initarg :type))
  :defaults ())

(defgeneric idltype-tc (idltype)
  (:documentation 
   "Compute the TypeCode that for an IDLType from the 
attributes other than type."))

(define-method op:type ((def IDLType))
  (unless (slot-boundp def 'type)
    (setf (slot-value def 'type)
      (idltype-tc def)))
  (slot-value def 'type))


;;;; Contained

(define-ir-class Contained (irobject)
  :id "IDL:omg.org/CORBA/Contained:1.0"
  :attributes ((id :readonly 
                   :initarg :subject-id
                   :accessor subject-id)
               (name "")
               (version "1.0")
               (defined_in nil :readonly)
               ;;(absolute_name "" :readonly)
               (containing_repository :readonly))
  :slots ((absolute_name :initarg :absolute_name)))

(defmethod print-object ((obj contained) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (slot-value obj 'name))))

(defmethod addto :after (r (c contained))
  (setf (slot-value c 'containing_repository) 
    (op:containing_repository r))
  c)


(define-method absolute_name ((obj contained))
  (unless (and (slot-boundp obj 'absolute_name)
               (not (equal (slot-value obj 'absolute_name)
                           "")))
    (setf (slot-value obj 'absolute_name)
      (let ((parent-name (op:absolute_name (op:defined_in obj))))
        (if parent-name
            (concatenate 'string 
              parent-name "::" (op:name obj))
          (op:name obj)))))
  (slot-value obj 'absolute_name))

(define-method op::describe ((obj contained))
  (make-struct "IDL:omg.org/CORBA/Contained/Description:1.0"
               :kind (op::def_kind obj)
               :value (describe-contained obj)))

(defmethod default-repoid ((obj contained) &optional prefix)
  (let ((names (list ":" (op:version obj))))
    (loop for c = obj then (op:defined_in c)
        while (typep c 'contained)
        unless (eq c obj) do (push "/" names)
        do (push (op:name c) names))
    (when prefix
      (push "/" names)
      (push prefix names))
    (apply 'concatenate 'string
           "IDL:"
           names)))


;;;; Container

(define-ir-class Container (irobject)
  :id "IDL:omg.org/CORBA/Container:1.0"
  :slots ((contents :initarg :contents 
                    :initform '()
                    :accessor contents))
  :defaults ())

(defmethod addto ((c container) (object contained))
  ;;(pushnew value (contents c))
  (setf (contents c)
    (cons object
          (delete-if (lambda (old)
                       (or (equalp (op::name old) (op::name object))
                           (equal (op::id old) (op::id object))))
                     (contents c))))
  (setf (slot-value object 'defined_in) c)
  object)

(define-method op::contents ((obj container) limit-type exclude-inherit)
  (declare (ignore exclude-inherit))
  ;; exclude-inherit only aplicable for operations???
  (loop for contained in (contents obj)
      when (and (or (eql limit-type :dk_all)
                    (eql limit-type (op::def_kind contained))))
      collect contained))

(define-method op::lookup_name
    ((obj container) search-name levels-to-search limit-type  exclude-inherit)
  (loop for contained in (op::contents obj limit-type exclude-inherit)
      when (equal search-name (op::name contained))
      collect contained))

(define-method op::lookup ((obj container) search_name)
  (loop for contained in (op:contents obj :dk_all nil)
      when (equal search_name (op::name contained))
      do (return contained)))


;;;  ModuleDef create_module (
;;;                           in RepositoryId id,
;;;                           in Identifier name,
;;;                           in VersionSpec version

(define-method create_module ((self container) id name version)
  (addto self (make-instance 'module-def
                :id id :name name :version version)))


;;;  ConstantDef create_constant (
;;;                               in RepositoryId id,
;;;                               in Identifier name,
;;;                               in VersionSpec version,
;;;                               in IDLType type,
;;;                               in any value                  )

(define-method create_constant ((self container)
                                id name version type value)
  (addto self (make-instance 'constant-def
                :id id :name name :version version
                :type_def type :value value)))

;;;  StructDef create_struct (
;;;                           in RepositoryId id,
;;;                           in Identifier name,
;;;                           in VersionSpec version,
;;;                           in StructMemberSeq members

(define-method create_struct ((self container)
                              id name version members)
  (let ((obj (make-instance 'struct-def
               :id id :name name :version version)))
    (setf (op:members obj) members)
    (addto self obj)))


;;;  /* This operation is missing in the CORBA2 spec! */
;;;  ExceptionDef create_exception (
;;;                                 in RepositoryId id,
;;;                                 in Identifier name,
;;;                                 in VersionSpec version,
;;;                                 in StructMemberSeq members
;;;                                 );

(define-method create_exception ((self container)
                                 id name version members)
  (addto self (make-instance 'exception-def
                :id id
                :name name
                :version version
                :members members)))



;;;  UnionDef create_union (
;;;                         in RepositoryId id,
;;;                         in Identifier name,
;;;                         in VersionSpec version,
;;;                         in IDLType discriminator_type,
;;;                         in UnionMemberSeq members           );

(define-method create_union ((self container)
                             id name version discriminator_type members)
  (addto self (make-instance 'union-def
                :id id :name name :version version
                :discriminator_type_def discriminator_type
                :members members)))


;;;  EnumDef create_enum (
;;;                       in RepositoryId id,
;;;                       in Identifier name,
;;;                       in VersionSpec version,
;;;                       in EnumMemberSeq members
;;;                       );

(define-method create_enum ((self container)
                            id name version members)
  (addto self (make-instance 'enum-def
                :id id :name name :version version
                :members members)))


;;;  AliasDef create_alias (
;;;                         in RepositoryId id,
;;;                         in Identifier name,
;;;                         in VersionSpec version,
;;;                         in IDLType original_type    );

(define-method create_alias ((self container)
                             id name version original_type)
  (addto (make-instance 'alias-def
                :id id :name name :version version
                :original_type_def original_type)))


;;;  InterfaceDef create_interface (
;;;            in RepositoryId id,
;;;            in Identifier name,
;;;            in VersionSpec version,
;;;            in InterfaceDefSeq base_interfaces );

(define-method create_interface ((self container)
                                 id name version base_interfaces)
  (addto self (make-instance 'interface-def
                :id id :name name :version version
                :base_interfaces base_interfaces)))


;;;; Repository

;;; interface Repository : Container

(define-ir-class Repository (Container)
  :id "IDL:omg.org/CORBA/Repository:1.0"
  :def_kind :dk_Repository
  :defaults ())

;;; Method to simplify computation of absolute_name
(define-method absolute_name ((obj Repository))
  nil)

(define-method containing_repository ((r Repository))
  r)

;;;  Contained lookup_id (in RepositoryId search_id);

(define-method lookup_id ((container Repository) search-id)
  (block lookup
    (let ((content-stack 
           (list (contents container))))
      (loop while content-stack
          do (map nil (lambda (obj) 
                        (if (equal (op:id obj) search-id)
                            (return-from lookup obj)
                          (if (typep obj 'container)
                              (push (contents obj) content-stack))))
                  (pop content-stack)))
      nil)))


;;;  PrimitiveDef get_primitive (in PrimitiveKind kind);

(define-method get_primitive ((container Repository) kind)
  (make-instance 'primitive-def
    :kind kind
    :type (ecase kind
            (:pk_null CORBA:tc_null)
            (:pk_void CORBA:tc_void)
            (:pk_short CORBA:tc_short)
            (:pk_long CORBA:tc_long)
            (:pk_ushort CORBA:tc_ushort)
            (:pk_ulong CORBA:tc_ulong)
            (:pk_float CORBA:tc_float)
            (:pk_double CORBA:tc_double)
            (:pk_boolean CORBA:tc_boolean)
            (:pk_char CORBA:tc_char)
            (:pk_octet CORBA:tc_octet)
            (:pk_any CORBA:tc_any)
            (:pk_TypeCode CORBA:tc_TypeCode)
            ;;(:pk_Principal CORBA:tc_Principal)
            (:pk_string CORBA:tc_string)
            (:pk_objref CORBA:tc_objref)
            (:pk_longlong CORBA:tc_longlong)
            (:pk_ulonglong CORBA:tc_ulonglong)
            (:pk_longdouble CORBA:tc_longdouble)
            (:pk_wchar CORBA:tc_wchar)
            (:pk_wstring CORBA:tc_wstring))))


;;;  StringDef create_string (in unsigned long bound);

(define-method create_string ((container Repository) bound)
  (make-instance 'string-def
    :type (make-typecode :tk_string bound)))

;;;  WstringDef create_wstring (in unsigned long bound);

;;;  SequenceDef create_sequence
;;;     (in unsigned long bound,in IDLType element_type);

(define-method create_sequence ((container Repository) bound element-type)
  (make-instance 'sequence-def
    :bound bound
    :element_type_def element-type
    :type (make-sequence-typecode (op:type element-type) bound)))
  

;;;  ArrayDef create_array
;;;     (in unsigned long length,in IDLType element_type);

(define-method create_array ((container Repository) length element_type)
  (make-instance 'array-def
    :length length
    :element_type_def element_type
    :type (make-array-typecode (op:type element_type) length)))


;;;  FixedDef create_fixed
;;;     (in unsigned short digits,in short scale);

(define-method create_fixed ((container Repository) digits scale)
  (make-instance 'fixed-def
    :digits digits
    :scale scale
    :type (make-typecode :tk_fixed digits scale)))


;;;; ModuleDef

(define-ir-class module-def (container contained)
  :id "IDL:omg.org/CORBA/ModuleDef:1.0"
  :def_kind :dk_module
  :defaults ())

(defparameter *module-description*
    (struct-typecode 
     "IDL:omg.org/CORBA/ModuleDescription:1.0"
     "ModuleDescription"
     "name" CORBA:tc_string             ; Identifier
     "id" CORBA:tc_string               ; RepositoryId
     "defined_in" CORBA:tc_string       ; RepositoryId
     "version" CORBA:tc_string          ; VersionSpec
     #||#))


(defmethod describe-contained ((module module-def))
  (make-struct *module-description*
               :name (op::name module)
               :id (op::id module)
               :defined_in (or (op::defined_in module) "")
               :version (op::version module)))

;;; interface ConstantDef : Contained {
;;;   readonly attribute TypeCode type;
;;;   attribute IDLType type_def;
;;;   attribute any value; };

(define-ir-class constant-def (contained)
  :id "IDL:omg.org/CORBA/ConstantDef:1.0"
  :attributes ((type_def)
               (value))
  :slots ((type))
  :def_kind :dk_Constant)


;;;; InterfaceDef

(define-ir-class interface-def (container contained idltype)
  :id "IDL:omg.org/CORBA/InterfaceDef:1.0"
  :attributes ((base_interfaces))
  :def_kind :dk_Interface
  :defaults ())

(defmethod idltype-tc ((idef interface-def))
  (make-typecode :tk_objref (subject-id idef) (op:name idef)))

(define-method op::is_a ((def interface-def) interface-id)
  ;; FIXME: check base classes
  (equal (subject-id def) interface-id))

(define-method op::describe_interface ((def interface-def))
  (make-struct "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0"
               :name (op::name def)
               :id   (subject-id def)
               :defined_in (subject-id (op::defined_in def)) 
               :version "1.0"
               :operations (map 'list
                             #'operation-description
                             (op::contents def :dk_Operation nil)  )
               :attributes (op::contents def :dk_Attribute nil)
               :base_interfaces (map 'list #'subject-id
                                     (op::base_interfaces def))
               :type (op::type def) ))

(defmethod describe-contained ((def interface-def))
  (op::describe_interface def))

(define-method op::contents ((obj interface-def) limit-type exclude-inherit)
  (if exclude-inherit
      (call-next-method)
    (append (call-next-method)
            (loop for x in (op:base_interfaces obj)
                append (op::contents x limit-type nil)))))

(defmethod find-opdef ((interface interface-def) operation)
  "Find in INTERFACE the OPERATION and return the operation-def object."
  ;; Compatibility with clorb-iir for use in auto-servants.
  (multiple-value-bind (name type)
      (analyze-operation-name operation)
    (let ((def (op:lookup interface name)))
      (case type
        (:setter 
         (assert (eq (op:def_kind def) :dk_Attribute))
         (make-setter-opdef operation def))
        (:getter 
         (assert (eq (op:def_kind def) :dk_Attribute))
         (make-getter-opdef operation def))
        (otherwise
         (assert (eq (op:def_kind def) :dk_Operation))
         def)))))


(defmethod find-opinfo ((interface interface-def) operation)
  (multiple-value-bind (name type)
      (analyze-operation-name operation)
    (let ((def (op:lookup interface name))
          (sym (intern (string-upcase name) :op)))
      (case type
        (:setter 
         (assert (eq (op:def_kind def) :dk_Attribute))
         (values (fdefinition (list 'setf sym))
                 (list (op:type def))
                 nil))
        (:getter 
         (assert (eq (op:def_kind def) :dk_Attribute))
         (values sym
                 nil
                 (list (op:type def))))
        (otherwise
         (assert (eq (op:def_kind def) :dk_Operation))
         (values sym
                 (opdef-inparam-typecodes def)
                 (opdef-outparam-typecodes def)))))))


(defun analyze-operation-name (name)
  (cond 
   ((< (length name) 6) name)
   ((string= name "_get_" :end1 5)
    (values (subseq name 5) :getter))
   ((string= name "_set_" :end1 5)
    (values (subseq name 5) :setter))
   (t name)))

(defun make-getter-opdef (name attdef)
  (make-opdef
   :name name
   :result (op:type attdef)))

(defun make-setter-opdef (name attdef)
  (make-opdef
   :name name
   :params (make-param "" :param_in (op:type attdef))))

;;;  AttributeDef create_attribute (
;;;                                 in RepositoryId id,
;;;                                 in Identifier name,
;;;                                 in VersionSpec version,
;;;                                 in IDLType type,
;;;                                 in AttributeMode mode       );

(define-method create_attribute ((self interface-def)
                                 id name version type mode)
  (addto self (make-instance 'attribute-def
                :id id :name name :version version
                :type_def type :mode mode)))


;;;  OperationDef create_operation (
;;;                                 in RepositoryId id,
;;;                                 in Identifier name,
;;;                                 in VersionSpec version,
;;;                                 in IDLType result,
;;;                                 in OperationMode mode, 
;;;                                 in ParDescriptionSeq params,
;;;                                 in ExceptionDefSeq exceptions,
;;;                                 in ContextIdSeq contexts            );

(define-method create_operation ((self interface-def)
                                 id name version
                                 result mode params exceptions contexts)
  (addto self (make-instance 'operation-def
                :id id :name name :version version
                :result_def result :mode mode
                :params params :exceptions exceptions
                :contexts contexts)))


;;;; AttributeDef

(define-ir-class attribute-def (contained)
  :id "IDL:omg.org/CORBA/AttributeDef:1.0"
  :attributes ((type_def)
               (mode))
  :def_kind :dk_Attribute
  :defaults ())

(define-method type ((adef attribute-def))
  (op:type (slot-value adef 'type_def)))

(defmethod describe-contained ((adef attribute-def))
  (make-struct "IDL:omg.org/CORBA/AttributeDescription:1.0"
               ;; Identifier name
               :name (op:name adef)
               ;; RepositoryId id
               :id (op:id adef)
               ;; RepositoryId defined_in
               :defined_in (subject-id (op:defined_in adef))
               ;; VersionSpec version
               :version (op:version adef)
               ;; TypeCode type
               :type (op:type adef)
               ;; AttributeMode mode
               :mode (op:mode adef)))


;;;; OperationDef

(define-ir-class operation-def (contained)
  :id "IDL:omg.org/CORBA/OperationDef:1.0"
  :attributes ((result :readonly :virtual result)
               (result_def)
               (params)
               (mode ':op_normal)
               (contexts nil)
               (exceptions nil))
  :def_kind :dk_Operation
  :defaults ())

(defmethod result ((opdef operation-def))
  (op:type (slot-value opdef 'result_def)))

(defmethod opdef-inparam-typecodes ((opdef operation-def))
  (loop for param in (op:params opdef)
      unless (eq (op:mode param) :param_out)
      collect (op:type param)))

(defmethod opdef-outparam-typecodes ((opdef operation-def))
  (let ((real-params   
         (loop for param in (op:params opdef)
             unless (eq (op:mode param) :param_in)
             collect (op:type param)))
        (result (op:result opdef)))
    (if (eq :tk_void (typecode-kind result))
        real-params
      (cons result real-params))))


(defun operation-description (opdef)
  ;;  struct OperationDescription 
  (make-struct "IDL:omg.org/CORBA/OperationDescription:1.0"
               ;;  Identifier name; 
               :name (op::name opdef)
               ;;  RepositoryId id; 
               :id   (op::id opdef)
               ;;  RepositoryId defined_in; 
               :defined_in (op::id (op::defined_in opdef))
               ;;  VersionSpec version;
               :version (op::version opdef)
               ;;  TypeCode result; 
               :result (op::result opdef)
               ;;  OperationMode mode; 
               :mode (op::mode opdef)
               ;;  ContextIdSeq contexts; 
               :contexts (op::contexts opdef)
               ;;  ParDescriptionSeq parameters;
               :parameters (op:params opdef)
               ;;  ExcDescriptionSeq exceptions;
               :exceptions (op::exceptions opdef)))

(defmethod describe-contained ((obj operation-def))
  (operation-description obj))

;;;; TypedefDef

(define-ir-class typedef-def (contained idltype)
  :id "IDL:omg.org/CORBA/TypedefDef:1.0"
  :def_kind :dk_Typedef
  :defaults ())

(defmethod describe-contained ((obj typedef-def))
  (make-struct
   "IDL:omg.org/CORBA/TypeDescription:1.0"
   ;; Identifier name
   :name (op:name obj)
   ;; RepositoryId id
   :id (op:id obj)
   ;; RepositoryId defined_in
   :defined_in (subject-id (op:defined_in obj))
   ;; VersionSpec version
   :version (op:version obj)
   ;; TypeCode type
   :type (op:type obj)))

;;;; interface AliasDef : TypedefDef
;;  attribute IDLType original_type_def;

(define-ir-class alias-def (typedef-def)
  :id "IDL:omg.org/CORBA/AliasDef:1.0"
  :attributes ((original_type_def))
  :def_kind :dk_Alias
  :defaults ())

(defmethod idltype-tc ((def alias-def))
  (make-typecode :tk_alias 
                 (op:id def) (op:name def)
                 (op:type (op:original_type_def def))))

;;;; interface StructDef : TypedefDef
;; attribute StructMemberSeq members;
;; struct StructMember {
;;  Identifier name;
;;  TypeCode type;
;;  IDLType type_def;

(define-ir-class struct-def (typedef-def)
  :id "IDL:omg.org/CORBA/StructDef:1.0"
  :attributes ((members :virtual members))
  :slots ((member-list :initarg :member-list
                       :accessor member-list))
  :def_kind :dk_Struct
  :defaults ())

(defmethod members ((sdef struct-def))
  (map 'vector
    (lambda (item)
      (make-struct "IDL:omg.org/CORBA/StructMember:1.0"
                   :name (car item)
                   :type_def (cdr item)
                   :type (op:type (cdr item))))
    (member-list sdef)))

(defmethod (setf members) (mlist (sdef struct-def))
  (setf (member-list sdef)
    (map 'vector (lambda (smdef)
                   (cons (struct-get smdef :name)
                         (struct-get smdef :type_def)))
         mlist)))

(defmethod idltype-tc ((def struct-def))
  (make-typecode :tk_struct 
                 (op:id def)
                 (op:name def)
                 (map 'vector
                   (lambda (x)
                     (let ((name (car x))
                           (type (op:type (cdr x))))
                       (list name type)))
                   (member-list def))))

;;;; interface UnionDef : TypedefDef
;;;   readonly attribute TypeCode discriminator_type;
;;;   attribute IDLType discriminator_type_def;
;;;   attribute UnionMemberSeq members;

(define-ir-class union-def (typedef-def)
  :id "IDL:omg.org/CORBA/UnionDef:1.0"
  :attributes ((discriminator_type_def)
               (members))
  :def_kind :dk_Union)

(define-method discriminator_type ((obj union-def))
  (op:type (op:discriminator_type_def obj)))

;;; struct UnionMember {
;;;   Identifier name;
;;;   any label;
;;;   TypeCode type;
;;;   IDLType type_def;
;;; typedef sequence <UnionMember> UnionMemberSeq;


;;;; Enum

(define-ir-class enum-def (typedef-def)
  :id "IDL:omg.org/CORBA/EnumDef:1.0"
  :attributes ((members))
  :def_kind :dk_Enum
  :defaults ())

(defmethod idltype-tc ((obj enum-def))
  (make-typecode :tk_enum
                 (slot-value obj 'id)
                 (slot-value obj 'name)
                 (slot-value obj 'members)))

;;;; interface ExceptionDef : Contained
;;;  readonly attribute TypeCode type;
;;;  attribute StructMemberSeq members;

(define-ir-class exception-def (contained)
  :id "IDL:omg.org/CORBA/ExceptionDef:1.0"
  :def_kind :dk_Exception
  :attributes ((members))
  :slots ((type)))

(defmethod op:members :before ((obj exception-def) &rest x)
  (with-slots (members) obj
    (unless (or (zerop (length members))
                (struct-get (elt members 0) :type))
      (setf members
        (map 'list
          (lambda (m)
            (make-struct (type-id m)
                         :name (struct-get m :name)
                         :type_def (struct-get m :type_def)
                         :type (op:type (struct-get m :type_def))))
          members)))))


(define-method type ((obj exception-def))
  (unless (slot-boundp obj 'type )
    (setf (slot-value obj 'type)
      (make-typecode 
       :tk_except
       (op:id obj)
       (op:name obj)
       (map 'vector
         (lambda (x)
           (list (struct-get x :name)
                 (struct-get x :type)))
         (op:members obj)))))
  (slot-value obj 'type))


(defmethod describe-contained ((obj exception-def))
  (make-struct "IDL:omg.org/CORBA/ExceptionDescription:1.0"
               ;;  Identifier name; 
               :name (op:name obj)
               ;;  RepositoryId id; 
               :id (op:id obj)
               ;;  RepositoryId defined_in; 
               :defined_in (op:id (op:defined_in obj))
               ;;  VersionSpec version;
               :version (op:version obj)
               ;;  TypeCode type; 
               :type (op:type obj)))


;;;; PrimitiveDef

(define-ir-class primitive-def (idltype)
  :id "IDL:omg.org/CORBA/PrimitiveDef:1.0"
  :attributes ((kind))
  :def_kind :dk_Primitive
  :defaults ())


(deftype PrimitiveKind ()
  '(member
    :pk_null :pk_void :pk_short :pk_long :pk_ushort :pk_ulong
    :pk_float :pk_double :pk_boolean :pk_char :pk_octet
    :pk_any :pk_TypeCode :pk_Principal :pk_string :pk_objref
    :pk_longlong :pk_ulonglong :pk_longdouble :pk_wchar :pk_wstring))

(defmethod print-object ((obj primitive-def) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (slot-value obj 'kind))))



;;;; interface StringDef : IDLType
;;;  attribute unsigned long bound;

(define-ir-class string-def (idltype)
  :id "IDL:omg.org/CORBA/StringDef:1.0"
  :attributes ((bound 0))
  :def_kind :dk_String)

(defmethod idltype-tc ((obj string-def))
  (make-typecode :tk_string (op:bound obj)))

;;;; interface WstringDef : IDLType {
;;;  attribute unsigned long bound;

(define-ir-class wstring-def (idltype)
  :id "IDL:omg.org/CORBA/WstringDef:1.0"
  :attributes ((bound 0))
  :def_kind :dk_Wstring)

(defmethod idltype-tc ((obj wstring-def))
  (make-typecode :tk_wstring (op:bound obj)))

;;;; SequenceDef
;;interface SequenceDef : IDLType
;;  attribute unsigned long bound;
;;  readonly attribute TypeCode element_type;
;;  attribute IDLType element_type_def;

(define-ir-class sequence-def (IDLType)
  :id "IDL:omg.org/CORBA/SequenceDef:1.0"
  :def_kind :dk_Sequence
  :attributes ((bound 0)                  ; long
               ;;(element_type :readonly) ; TypeCode
               (element_type_def))      ; IDLType
  :defaults ())

(define-method element_type ((obj sequence-def))
  (op:type (op:element_type_def obj)))

(defmethod idltype-tc ((obj sequence-def))
  (make-sequence-typecode (op:element_type obj)
                          (op:bound obj)))



;;;; Export IR

(defvar *ifr-servant* nil)

(defun setup-ifr ()
  (unless *ifr-servant*
    (setq *ifr-servant* (make-instance 'repository)))
  (with-open-file (out "/tmp/InterfaceRepository"
                   :direction :output 
                   :if-exists :supersede)
    (format out "~A~%" (op:object_to_string (orb_init) *ifr-servant*))))


;;; local-ir.lisp ends here
