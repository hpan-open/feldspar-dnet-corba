;;;; clorb-ifr.lisp -- An Interface Repository Implementation

(in-package :clorb)


;;;; Registry of Ir class for DefinitionKind

(defun register-ir-class (def-kind class)
  (setf (get def-kind 'ir-class) class))

(defun get-ir-class (def-kind)
  (or (get def-kind 'ir-class)
      (error "No class defined for definition kind ~A" def-kind)))


;;;; Generics

(defgeneric addto (container contained)
  (:documentation "Add a contained to a container"))


;;;; Base Clases

(defmacro define-ir-class (name supers
                           &rest args
                           &key id def_kind &allow-other-keys)
  (declare (ignore id))
  (setq args
    (loop for x on args by #'cddr
        unless (member (car x) '(:id :def_kind))
        nconc (list (car x) (cadr x))))
  `(progn (define-corba-class ,name ,supers ,@args)
          ,@(if def_kind
                `((define-method def_kind ((x ,name)) ,def_kind)
                  (register-ir-class ,def_kind ',name)))))


(define-ir-class IRObject (CORBA:IRObject)
  :id "IDL:omg.org/CORBA/IRObject:1.0"
  :attributes ()
  :defaults ())

(defclass irtypecode-mixin ()
  ((type)))

(defgeneric idltype-tc (irtypecode-mixin)
  (:documentation
   "Compute the TypeCode from the attributes other than type."))

(define-method type ((def irtypecode-mixin))
  ;; Get the typecode for an IDL construct.
  ;; The typecode is lazily constructed and recursive typecodes handled.
  ;; The specific typcodes are handled ny the idltype-tc function.
  (cond ((slot-boundp def 'type)
         ;; Either the typecode is computed already or this is a recursive call
         ;; during computation.
         (let ((tc (slot-value def 'type)))
           (cond ((eq t tc)
                  ;; Beeing computed, make a placeholder typecode.
                  ;; This typecode will be filled when the computation is done.
                  (setf (slot-value def 'type) (make-typecode t)))
                 (t
                  tc))))
        (t
         ;; Typecode not defined, start computation
         ;; mark computation with a t in the slot
         (setf (slot-value def 'type) t)
         (let* ((new-tc (idltype-tc def))
                (old-tc (slot-value def 'type)))
           (cond ((eq old-tc t)
                  (setf (slot-value def 'type) new-tc))
                 (t
                  ;; therse was a recurive access during computation
                  (typecode-smash old-tc new-tc)
                  old-tc))))))


(defmethod slot-updated :after ((obj irtypecode-mixin))
  (slot-makunbound obj 'type))


(define-ir-class IDLType (CORBA:IDLType IRObject irtypecode-mixin)
  :id "IDL:omg.org/CORBA/IDLType:1.0"
  :attributes ()
  :defaults ())


;;;; Contained

(define-ir-class Contained (CORBA:Contained irobject)
  :id "IDL:omg.org/CORBA/Contained:1.0"
  :attributes ((id :readonly            ; setter defined below
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

(defmethod (setf op:id) (new-id (obj Contained))
  (let ((repository (op:containing_repository obj))
        (old-id (op:id obj)))
    (unless (equal new-id old-id)
      (register-irobj repository new-id obj)
      (unregister-irobj repository old-id)
      (setf (slot-value obj 'id) new-id))))

(define-method absolute_name ((obj contained))
  (unless (and (slot-boundp obj 'absolute_name)
               (not (equal (slot-value obj 'absolute_name)
                           "")))
    (setf (slot-value obj 'absolute_name)
          (concatenate 'string
                       (op:absolute_name (op:defined_in obj)) "::" (op:name obj))))
  (slot-value obj 'absolute_name))

(defmethod slot-updated :after ((obj contained))
  (slot-makunbound obj 'absolute_name))

(define-method describe ((obj contained))
  (CORBA:Contained/Description
   :kind (op:def_kind obj)
   :value (describe-contained obj)))



;;;; Container

(define-ir-class Container (CORBA:Container irobject)
  :id "IDL:omg.org/CORBA/Container:1.0"
  :slots ((contents :initarg :contents
                    :initform '()
                    :accessor contents))
  :defaults ())

(defmethod addto ((c container) (object contained))
  ;; FIXME: this implements a move semantics, but when not used in move
  ;; it should signal error if duplicated ID or NAME.
  ;; Possibly make the behaviour optional: &key move
  (setf (slot-value object 'containing_repository)
        (op:containing_repository c))
  (let ((id (op:id object))
        (name (op:name object)))
    (setf (contents c)
          (cons object
                (delete-if (lambda (old)
                             (or (equalp (op:name old) name)
                                 (equal (op:id old) id)))
                           (contents c))))
    (unregister-irobj c id)
    (setf (slot-value object 'defined_in) c)
    (register-irobj c id object))
  object)

(define-method contents ((obj container) limit-type exclude-inherit)
  (declare (ignore exclude-inherit))
  ;; exclude-inherit only aplicable for operations???
  (loop for contained in (contents obj)
      when (and (or (eql limit-type :dk_all)
                    (eql limit-type (op:def_kind contained))))
      collect contained))

(define-method lookup_name
    ((obj container) search-name levels-to-search limit-type exclude-inherit)
  (loop for contained in (op:contents obj limit-type exclude-inherit)
      when (equal search-name (op:name contained))
      collect contained into top-level
      when (and (containerp contained)
                (or (> levels-to-search 1) (= levels-to-search -1)))
      collect (op:lookup_name contained search-name
                              (if (= levels-to-search -1)
                                -1
                                (1- levels-to-search))
                              limit-type exclude-inherit)
      into sub-levels
      finally (return (mapcan #'identity (cons top-level sub-levels)))))

(defgeneric containerp (x)
  (:method ((x t)) nil)
  (:method ((x container)) t))


(define-method lookup ((obj null) search_name)
  (declare (ignore search_name))
  nil)

(define-method lookup ((obj container) search_name)
  (cond ((string-starts-with search_name "::")
         (op:lookup (op:containing_repository obj)
                    (subseq search_name 2)))
        (t
         (multiple-value-bind (first-name rest-name)
                              (split-name search_name)
           (let ((contained
                  (find first-name (op:contents obj :dk_all nil)
                        :key #'op:name
                        :test #'string-equal)))
             (cond ((null contained)
                    (op:lookup (op:defined_in obj) search_name))
                   (rest-name
                    (op:lookup contained rest-name))
                   (t
                    contained)))))))

(defun split-name (str)
  (let ((method-end (position #\: str)))
    (if method-end
      (values (subseq str 0 method-end)
              (subseq str (+ method-end 2)))
      str )))


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
  (addto  self (make-instance 'alias-def
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

(define-ir-class Repository (CORBA:Repository Container)
  :id "IDL:omg.org/CORBA/Repository:1.0"
  :def_kind :dk_Repository
  :slots ((idmap :initform (make-hash-table :test #'equal)
                 :reader idmap)
          (primitives-cache
           :initform nil
           :accessor primitives-cache )))


;;; Method to simplify computation of absolute_name
(define-method absolute_name ((obj Repository))
  "")

(define-method containing_repository ((r Repository))
  r)

(defmethod subject-id ((r repository))
  "")

;;;  Contained lookup_id (in RepositoryId search_id);

(define-method lookup_id ((rep repository) id)
  (gethash id (idmap rep)))

(defmethod register-irobj ((r repository) id obj)
  (when (gethash id (idmap r))
    (error 'CORBA:BAD_PARAM :minor 2))
  (setf (gethash id (idmap r)) obj))

(defmethod unregister-irobj ((r repository) id)
  (remhash id (idmap r)))

(defmethod register-irobj ((x irobject) id obj)
  (register-irobj (op:containing_repository x) id obj))

(defmethod unregister-irobj ((x irobject) id)
  (unregister-irobj (op:containing_repository x) id))



;;;  PrimitiveDef get_primitive (in PrimitiveKind kind);

(define-method get_primitive ((container Repository) kind)
  (with-accessors ((cache primitives-cache)) container
    (unless cache
      (setf cache
            (loop for (kind tc) 
                  in '((:pk_null CORBA:tc_null)
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
                       (:pk_wstring CORBA:tc_wstring))
                  collect (make-instance 'primitive-def :kind kind 
                                         :type (symbol-value tc)))))
    (find kind cache :key #'op:kind)))


;;;  StringDef create_string (in unsigned long bound);

(define-method create_string ((container Repository) bound)
  (make-instance 'string-def
    :bound bound))

;;;  WstringDef create_wstring (in unsigned long bound);

;;;  SequenceDef create_sequence
;;;     (in unsigned long bound,in IDLType element_type);

(define-method create_sequence ((container Repository) bound element-type)
  (check-type bound integer)
  (check-type element-type omg.org/corba:idltype)
  (make-instance 'sequence-def
    :bound bound
    :element_type_def element-type))


;;;  ArrayDef create_array
;;;     (in unsigned long length,in IDLType element_type);

(define-method create_array ((container Repository) length element_type)
  (make-instance 'array-def
    :length length
    :element_type_def element_type))


;;;  FixedDef create_fixed
;;;     (in unsigned short digits,in short scale);

(define-method create_fixed ((container Repository) digits scale)
  (make-instance 'fixed-def
    :digits digits
    :scale scale))


;;;; ModuleDef

(define-ir-class module-def (CORBA:ModuleDef container contained)
  :id "IDL:omg.org/CORBA/ModuleDef:1.0"
  :def_kind :dk_module
  :defaults ())


(defmethod describe-contained ((module module-def))
  (CORBA:ModuleDescription
   :name (op:name module)
   :id (op:id module)
   :defined_in (or (op:defined_in module) "")
   :version (op:version module)))

;;; interface ConstantDef : Contained {
;;;   readonly attribute TypeCode type;
;;;   attribute IDLType type_def;
;;;   attribute any value; };

(define-ir-class constant-def (CORBA:ConstantDef contained)
  :id "IDL:omg.org/CORBA/ConstantDef:1.0"
  :attributes ((type_def)
               (value))
  :slots ((type))
  :def_kind :dk_Constant)


;;;; InterfaceDef

(define-ir-class interface-def (CORBA:InterfaceDef container contained idltype)
  :id "IDL:omg.org/CORBA/InterfaceDef:1.0"
  :attributes ((base_interfaces))
  :def_kind :dk_Interface
  :defaults ())

(defmethod idltype-tc ((idef interface-def))
  (make-typecode :tk_objref (subject-id idef) (op:name idef)))

(define-method is_a ((def interface-def) interface-id)
  (or
   (equal (subject-id def) interface-id)
   (equal interface-id (interface-id *object-interface*))
   (some (lambda (b) (equal interface-id (op:id b)))
         (op:base_interfaces def))))

(define-method describe_interface ((def interface-def))
  (CORBA:InterfaceDef/FullInterfaceDescription
   :name (op:name def)
   :id   (subject-id def)
   :defined_in (subject-id (op:defined_in def))
   :version (op:version def)
   :operations (map 'list #'operation-description
                    (op:contents def :dk_Operation nil))
   :attributes (map 'list #'describe-contained
                    (op:contents def :dk_Attribute nil))
   :base_interfaces (map 'list #'subject-id
                         (op:base_interfaces def))
   :type (op:type def) ))

(defmethod describe-contained ((def interface-def))
  (op:describe_interface def))

(define-method contents ((obj interface-def) limit-type exclude-inherit)
  (if exclude-inherit
      (call-next-method)
    (append (call-next-method)
            (loop for x in (coerce (op:base_interfaces obj) 'list)
                append (op:contents x limit-type nil)))))



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

(define-ir-class attribute-def (CORBA:AttributeDef contained)
  :id "IDL:omg.org/CORBA/AttributeDef:1.0"
  :attributes ((type_def)
               (mode))
  :def_kind :dk_Attribute
  :defaults ())

(define-method type ((adef attribute-def))
  (op:type (op:type_def adef)))

(defmethod describe-contained ((adef attribute-def))
  (CORBA:AttributeDescription
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

(define-ir-class operation-def (CORBA:OperationDef contained)
  :id "IDL:omg.org/CORBA/OperationDef:1.0"
  :attributes ((result :readonly :virtual result)
               (result_def)
               (params)
               (mode ':op_normal)
               (contexts nil)
               (exceptions nil))
  :def_kind :dk_Operation )

(defun validate-operation-def (mode result params exceptions)
  (when (eq mode :op_oneway)
    (unless (and (eq :pk_void (op:type result))
                 (zerop (length exceptions))
                 (every (lambda (p)
                          (eq :param_in (op:mode p)))
                        params))
      (error 'omg.org/corba:bad_param
             :minor 31
             :completed :completed_yes))))

(defun validate-operation-def-change (def &key 
                                            (mode (op:mode def))
                                            (result (op:result_def def))
                                            (params (op:params def))
                                            (exceptions (op:exceptions def)))
  (validate-operation-def mode result params exceptions))

(defmethod initialize-instance :before ((def operation-def)
                                        &key result_def mode params exceptions)
  (validate-operation-def mode result_def params exceptions))

(defmethod (setf op:result_def) :before (new (def operation-def)) 
  (validate-operation-def-change def :result new))

(defmethod (setf op:mode) :before (new (def operation-def))
  (validate-operation-def-change def :mode new))               

(defmethod (setf op:params) :before (new (def operation-def))
  (validate-operation-def-change def :params new))               

(defmethod (setf op:exceptions) :before (new (def operation-def))
  (validate-operation-def-change def :exceptions new))               

(defmethod result ((opdef operation-def))
  (op:type (slot-value opdef 'result_def)))

(defmethod op:params :before ((opdef operation-def) &key)
  (doseq (pd (slot-value opdef 'params))
         (setf (op:type pd) (op:type (op:type_def pd)))))


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
  (CORBA:OperationDescription
   ;;  Identifier name;
   :name (op:name opdef)
   ;;  RepositoryId id;
   :id   (op:id opdef)
   ;;  RepositoryId defined_in;
   :defined_in (op:id (op:defined_in opdef))
   ;;  VersionSpec version;
   :version (op:version opdef)
   ;;  TypeCode result;
   :result (op:result opdef)
   ;;  OperationMode mode;
   :mode (op:mode opdef)
   ;;  ContextIdSeq contexts;
   :contexts (op:contexts opdef)
   ;;  ParDescriptionSeq parameters;
   :parameters (op:params opdef)
   ;;  ExcDescriptionSeq exceptions;
   :exceptions (map 'list
                    'describe-contained (op:exceptions opdef))))

(defmethod describe-contained ((obj operation-def))
  (operation-description obj))

;;;; TypedefDef

(define-ir-class typedef-def (CORBA:TypedefDef contained idltype)
  :id "IDL:omg.org/CORBA/TypedefDef:1.0"
  :def_kind :dk_Typedef
  :defaults ())

(defmethod describe-contained ((obj typedef-def))
  (CORBA:TypeDescription
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

(define-ir-class alias-def (corba:AliasDef typedef-def)
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

(define-ir-class struct-def (corba:StructDef typedef-def)
  :id "IDL:omg.org/CORBA/StructDef:1.0"
  :attributes ((members :virtual members))
  :slots ((member-list :initarg :member-list
                       :accessor member-list))
  :def_kind :dk_Struct
  :defaults ())

(defmethod members ((sdef struct-def))
  (map 'vector
    (lambda (item)
      (CORBA:StructMember
       :name (car item)
       :type_def (cdr item)
       :type (op:type (cdr item))))
    (member-list sdef)))

(defmethod (setf members) (mlist (sdef struct-def))
  (setf (member-list sdef)
    (map 'vector (lambda (smdef)
                   (cons (struct-get smdef :name)
                         (struct-get smdef :type_def)))
         mlist))
  (slot-makunbound sdef 'type))

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

;;; struct UnionMember {
;;;   Identifier name;
;;;   any label;
;;;   TypeCode type;
;;;   IDLType type_def;
;;; typedef sequence <UnionMember> UnionMemberSeq;

(define-ir-class union-def (CORBA:UnionDef typedef-def)
  :id "IDL:omg.org/CORBA/UnionDef:1.0"
  :attributes ((discriminator_type_def)
               (members))
  :def_kind :dk_Union)

(define-method discriminator_type ((obj union-def))
  (op:type (op:discriminator_type_def obj)))

(defmethod op:members :before ((self union-def) &rest args)
  (declare (ignore args))
  (doseq (member (slot-value self 'members))
    (setf (op:type member) (op:type (op:type_def member)))))

(defmethod idltype-tc ((obj union-def))
  (flet ((default-label-p (label)
           (and (not (symbolp label))
                (eql :tk_octet (op:kind (any-typecode label)))
                                (= 0 (any-value label)))))
    (create-union-tc (op:id obj) (op:name obj)
                     (op:discriminator_type obj)
                     (map 'list (lambda (m)
                                  (list (if (default-label-p (op:label m))
                                          'default
                                          (op:label m))
                                        (op:name m)
                                        (op:type m)))
                          (op:members obj)))))


;;;; EnumDef

(define-ir-class enum-def (corba:EnumDef typedef-def)
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

(define-ir-class exception-def (corba:ExceptionDef contained irtypecode-mixin)
  :id "IDL:omg.org/CORBA/ExceptionDef:1.0"
  :def_kind :dk_Exception
  :attributes ((members)))

(defmethod op:members :before ((obj exception-def) &rest x)
  (declare (ignore x))
  (doseq (m (slot-value obj 'members))
    (setf (op:type m) (op:type (op:type_def m)))))

(defmethod idltype-tc ((obj exception-def))
  (make-typecode :tk_except
                 (op:id obj)
                 (op:name obj)
                 (map 'vector
                      (lambda (x) (list (op:name x) (op:type x)))
                      (op:members obj))))

(defmethod describe-contained ((obj exception-def))
  (CORBA:ExceptionDescription
   ;;  Identifier name;
   :name (op:name obj)
   ;;  RepositoryId id;
   :id (op:id obj)
   ;;  RepositoryId defined_in;
   :defined_in (subject-id (op:defined_in obj))
   ;;  VersionSpec version;
   :version (op:version obj)
   ;;  TypeCode type;
   :type (op:type obj)))


;;;; PrimitiveDef

(define-ir-class primitive-def (corba:PrimitiveDef idltype)
  :id "IDL:omg.org/CORBA/PrimitiveDef:1.0"
  :attributes ((kind))
  :def_kind :dk_Primitive
  :slots ((type :initarg :type)) ; add initarg
  :defaults ())

(defmethod print-object ((obj primitive-def) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (slot-value obj 'kind))))



;;;; interface StringDef : IDLType
;;;  attribute unsigned long bound;

(define-ir-class string-def (corba:StringDef idltype)
  :id "IDL:omg.org/CORBA/StringDef:1.0"
  :attributes ((bound 0))
  :def_kind :dk_String)

(defmethod idltype-tc ((obj string-def))
  (make-typecode :tk_string (op:bound obj)))

;;;; interface WstringDef : IDLType {
;;;  attribute unsigned long bound;

(define-ir-class wstring-def (corba:WstringDef idltype)
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

(define-ir-class sequence-def (corba:SequenceDef IDLType)
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


;;; ArrayDef : IDLType
;;        attribute unsigned long         length;
;;        readonly attribute TypeCode     element_type;
;;        attribute IDLType               element_type_def;

(define-ir-class array-def (CORBA:ArrayDef idltype)
  :id "IDL:omg.org/CORBA/ArrayDef:1.0"
  :def_kind :dk_Array
  :attributes ((length 0)
               (element_type_def)))

(define-method element_type ((obj array-def))
  (op:type (op:element_type_def obj)))

(defmethod idltype-tc ((obj array-def))
  (make-array-typecode (op:element_type obj)
                       (op:length obj)))



;;;; Export IR

(defvar *ifr-servant* nil)

(defun setup-ifr (&key (ior-file "/tmp/InterfaceRepository"))
  (unless *ifr-servant*
    (setq *ifr-servant* (make-instance 'repository)))
  (with-open-file (out ior-file :direction :output
                       :if-exists :supersede)
    (format out "~A~%" (op:object_to_string (orb_init) *ifr-servant*))))


;;; clorb-ifr.lisp ends here
