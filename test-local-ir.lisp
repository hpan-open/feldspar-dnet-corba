(in-package :clorb)

(define-test-suite "Local IFR test"
  (variables
   (repository (make-instance 'repository))
   (a-ulong (op:get_primitive repository :pk_ulong))
   (a-string (op:get_primitive repository :pk_string)))

  (define-test "General"
    (ensure (typep :dk_Constant 'CORBA:DefinitionKind)))

  (define-test "Contained"
    (let* ((name "foo")
           (id "IDL:foo:1.0")
           (sub-id "IDL:mod/foo:1.0")
           (obj (op:create_enum repository id name "1.0" '("fie" "fum"))))
      (ensure-eql (op:lookup repository name) obj)
      (ensure-eql (op:lookup_id repository id) obj)
      (ensure-equalp (op:id obj) id)
      (ensure-equalp (op:name obj) name)
      (ensure-equalp (op:version obj) "1.0")
      (ensure-eql (op:defined_in obj) repository)
      (ensure-eql (op:containing_repository obj) repository)
      (ensure-equalp (op:absolute_name obj)
                     (concatenate 'string "::" name))
      (let ((desc (op:describe obj)))
        (ensure desc)
        (ensure-equalp (op:kind desc) :dk_enum)
        (setq desc (op:value desc))
        (ensure desc))
      (let* ((module (op:create_module repository "my-module" "mod" "1.1"))
             (obj (op:create_enum module sub-id name "1.0" '("fie" "fum"))))
        (ensure-eql (op:defined_in obj) module)
        (ensure-eql (op:containing_repository obj) repository)
        (ensure-eql (op:lookup_id repository sub-id) obj)
        (ensure-equalp (op:absolute_name obj)
                       (concatenate 'string (op:absolute_name module) "::" name)))
      (let ((new-id "IDL:foob:1.1"))
        (setf (op:id obj) new-id)
        (ensure-equalp (op:id obj) new-id)
        (ensure-eql (op:lookup_id repository new-id) obj)
        (ensure-eql (op:lookup_id repository id) nil)
        (handler-case (setf (op:id obj) sub-id)
          (corba:bad_param (exc)
                           (ensure-eql (op:minor exc) 2))
          (condition ()
                     (ensure nil "Wrong condition type"))
          (:no-error ()
                     (ensure nil "No exception for duplicated RID"))))))


  (define-test "Container"
      (let* ((module (op:create_module repository "my-module" "mod" "1.1"))
             (obj (op:create_enum module "IDL:mod/foo:1.0" "my-enum" "1.0" '("fie" "fum")))
             (obj2 (op:create_enum repository "IDL:foo:1.0" "foo" "1.0" '("fie" "fum"))))
        (ensure-equalp (op:lookup repository "mod") module)
        (ensure-equalp (op:lookup repository "foo") obj2)
        (ensure-equalp (op:lookup module "my-enum") obj)
        (ensure-equalp (op:lookup repository "mod::my-enum") obj)
        (ensure-equalp (op:lookup repository "::mod::my-enum") obj)
        (ensure-equalp (op:lookup module "::mod::my-enum") obj)
        (ensure-equalp (op:lookup module "::foo") obj2)
        (ensure-equalp (op:lookup module "fie") nil)))


  (define-test "UnionDef"
    (let* ((id "IDL:foo/Union:1.1")
           (name "aunion") (version "1.1")
           (desc-type a-ulong)
           (members (list (CORBA:UnionMember :name "aa"
                                             :label 1
                                             :type_def a-string)
                          (CORBA:UnionMember :name "_def_"
                                             :label (CORBA:Any :any-typecode CORBA:tc_octet
                                                               :any-value 0)
                                             :type_def a-ulong )
                          (CORBA:UnionMember :name "bb"
                                             :label 2
                                             :type_def a-ulong)))
           (obj (op:create_union repository id name version desc-type members)))
      (ensure-equalp (op:id obj) id)
      (ensure-equalp (op:version obj) version)
      (ensure-equalp (length (op:members obj)) (length members))
      (ensure-equalp (op:member_count (op:type obj))
                     (length members))
      (ensure-equalp (op:kind (op:discriminator_type obj))
                     :tk_ulong)
      (ensure-equalp (omg.org/features:default_index (op:type obj)) 1)
      ;; update
      (setf (op:discriminator_type_def obj)
            (op:get_primitive repository :pk_ushort))
      (ensure-equalp (op:kind (op:discriminator_type obj))
                     :tk_ushort)))

  (define-test "EnumDef"
    (let ((obj (op:create_enum repository "IDL:foo:1.0" "foo" "1.0" '("fie" "fum"))))
      (ensure-eql (op:lookup repository "foo") obj)
      (let ((tc (op:type obj)))
        (ensure-equalp (op:kind tc) :tk_enum)
        (ensure-equalp (op:member_count tc) 2)
        (ensure-equalp (op:member_name tc 0) "fie"))
      (setf (op:members obj) '("a" "b" "c"))
      (let ((tc (op:type obj)))
        (ensure-equalp (op:member_count tc) 3))))

  (define-test "SequenceDef"
    (let ((obj (op:create_sequence repository 0 a-ulong)))
      (ensure-equalp (op:def_kind obj) :dk_sequence)
      (ensure-equalp (op:bound obj) 0)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_ulong)
      ;; Write interface
      (setf (op:element_type_def obj) a-string)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_string)))

  (define-test "ArrayDef"
    (let ((obj (op:create_array repository 10 a-string)))
      (ensure-equalp (op:def_kind obj) :dk_array)
      (ensure-equalp (op:length obj) 10)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_string)
      ;; Write interface
      (setf (op:element_type_def obj) a-ulong)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_ulong)))

  (define-test "ExceptionDef"
    (let* ((members (list (CORBA:StructMember :name "a"
                                              :type CORBA:tc_void
                                              :type_def a-string)
                          (CORBA:StructMember :name "b"
                                              :type CORBA:tc_void
                                              :type_def a-ulong)))
           (obj (op:create_exception repository "IDL:my/Exception:1.0" "Exception"
                                     "1.0" members)))
      (let ((type (op:type obj)))
        (ensure-equalp (op:kind type) :tk_except)
        (ensure-equalp (op:member_count type) (length members)))
      (let ((m (op:members obj)))
        (ensure-equalp (length m) (length members))
        (ensure-equalp (op:kind (op:type (elt m 0))) :tk_string)
        (ensure-equalp (op:kind (op:type (elt m 1))) :tk_ulong))
      (let ((desc (op:describe obj)))
        (ensure-equalp (op:kind desc) :dk_exception)
        (ensure-typep (op:value desc) 'CORBA:ExceptionDescription)
        (setq desc (op:value desc))
        (ensure-equalp (op:name desc) (op:name obj))
        (ensure-equalp (op:id desc) (op:id obj)))
      ;; Write interface
      (setf (op:members obj) (cdr members))
      (ensure-equalp (length (op:members obj)) (1- (length members)))
      (ensure-equalp (op:member_count (op:type obj)) (1- (length members)))
      (ensure-equalp (op:kind (op:member_type (op:type obj) 0)) :tk_ulong)))

  (define-test "InterfaceDef"
    (let* ((id "IDL:my/Interface:1.1")
           (name "Interface")
           (version "1.1")
           (obj (op:create_interface repository id name version '())))
      (ensure (op:is_a obj id) "isa Self")
      (ensure (op:is_a obj "IDL:omg.org/CORBA/Object:1.0") "isa Object")
      (op:create_attribute obj "IDL:my/a:1.0" "a" "1.0" a-string :attr_normal)
      (let ((desc (op:describe_interface obj)))
        (ensure-typep desc 'CORBA:InterfaceDef/FullInterfaceDescription)
        (ensure-typep (op:operations desc) 'sequence)
        (let ((attrs (op:attributes desc)))
          (ensure-equalp (length attrs) 1)
          (ensure-typep (elt attrs 0) 'omg.org/corba:attributedescription)
        ))))

  (define-test "AttributeDef"
    (let* ((idef (op:create_interface repository "IDL:my/Interface:1.1" "Interface" "1.1" '()))
           (obj (op:create_attribute idef "IDL:my/Att:1.1" "Att" "1.1"
                                     a-string :attr_readonly)))
      (ensure-equalp (op:type_def obj) a-string)
      (ensure-equalp (op:kind (op:type obj)) :tk_string)
      (ensure-equalp (op:mode obj) :attr_readonly)
      (let ((desc (op:describe obj)))
        (ensure-equalp (op:kind desc) :dk_attribute)
        (setq desc (op:value desc))
        (ensure-typep desc 'CORBA:AttributeDescription))
      (setf (op:type_def obj) a-ulong)
      (ensure-equalp (op:kind (op:type obj)) :tk_ulong)))

  (define-test "OperationDef"
    (let* ((idef (op:create_interface repository "IDL:my/Interface:1.1" "Interface" "1.1" '()))
           (id "IDL:my/Interface/Op1:1.1")
           (name "Op1")
           (version "1.1")
           (result a-ulong)
           (mode :OP_NORMAL)
           (params (list (CORBA:ParameterDescription
                          :name "a"
                          :type CORBA:tc_void
                          :type_def a-string
                          :mode :PARAM_IN)))
           (exceptions '())
           (contexts '())
           (obj (op:create_operation idef id name version result mode params exceptions contexts)))
      (ensure-equalp (op:kind (op:result obj)) :tk_ulong)
      (let ((pds (op:params obj)))
        (ensure-equalp (length pds) 1)
        (ensure-equalp (op:kind (op:type (elt pds 0))) :tk_string))
      (let ((desc (op:describe obj)))
        (ensure-equalp (op:kind desc) :dk_operation)
        (setq desc (op:value desc))
        (ensure-typep desc 'CORBA:OperationDescription))
      ;; Write interface
      (setf (op:result_def obj) a-string)
      (ensure-equalp (op:kind (op:result obj)) :tk_string)
      (setf (op:mode obj) :op_normal)
      #|should signal BAD_PARAM (setf (op:mode obj) :op_oneway) |# ))

  (define-test "StringDef"
    (let ((obj (op:create_string repository 10)))
      (ensure-equalp (op:bound obj) 10)
      (ensure-equalp (op:kind (op:type obj)) :tk_string)
      ))
)
