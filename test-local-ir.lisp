(in-package :clorb)

(define-test-suite "Local IFR test"
  (variables
   (repository (make-instance 'repository))
   (a-ulong (op:get_primitive repository :pk_ulong))
   (a-string (op:get_primitive repository :pk_string)))

  (define-test "General"
    (ensure-typep :dk_constant 'CORBA:DefinitionKind))

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
                       (concatenate 'string (op:absolute_name module) "::" name))
        ;; FIXME: test void move (in Container new_container, in Identifier new_name, in VersionSpec new_version);
        )
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
        (ensure-repository
         "mod" module   "foo" obj2   "mod::my-enum" obj   "::mod::my-enum" obj 
         "mod" (def-pattern :dk_module
                 'identity (repository-pattern "my-enum" obj  
                                               "::mod::my-enum" obj "::foo" obj2 )))
        (ensure-equalp (op:lookup module "fie") nil)))


  (define-test "UnionDef"
    (let* ((id "IDL:foo/Union:1.1") (name "aunion") (version "1.1")
           (desc-type a-ulong)
           (members (list (CORBA:UnionMember :name "aa"  :label 1  :type_def a-string)
                          (CORBA:UnionMember :name "_def_"  :type_def a-ulong 
                                             :label (CORBA:Any :any-typecode CORBA:tc_octet
                                                               :any-value 0))
                          (CORBA:UnionMember :name "bb"  :label 2  :type_def a-ulong)))
           (obj (op:create_union repository id name version desc-type members)))
      (ensure-pattern obj (def-pattern :dk_union
                            'op:id id  'op:version version
                            'op:members (sequence-pattern (pattern 'op:name "aa"
                                                              'op:label (pattern 'any-value 1)
                                                              'op:type_def a-string
                                                              'op:type corba:tc_string )
                                                     (pattern 'op:label (pattern 'any-typecode CORBA:tc_octet 'any-value 0)
                                                              'op:type_def a-ulong )
                                                     (pattern 'op:name "bb" ))
                            'op:type (pattern 'op:kind :tk_union
                                              'op:member_count (length members)
                                              'op:default_index 1)
                            'op:discriminator_type (pattern 'op:kind :tk_ulong)))
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

  (define-test "AliasDef"
    (let* ((name "Fie") (ver "1.1")
           (id (format nil "IDL:~A:~A" name ver))
           (alias (op:create_alias repository id name ver a-string)))
      (ensure-pattern 
       alias
       (def-pattern :dk_alias
         'op:name name 'op:version ver 'op:id id
         'op:absolute_name "::Fie"
         'op:type (pattern 'op:kind :tk_alias
                           'op:content_type CORBA:tc_string )
         'op:original_type_def a-string ))
      (ensure-pattern repository (repository-pattern name alias))
      (setf (op:original_type_def alias) a-ulong)
      (ensure-pattern* alias 'op:type (pattern 'op:content_type CORBA:tc_ulong))))

  (define-test "SequenceDef"
    (let ((obj (op:create_sequence repository 0 a-ulong)))
      (ensure-pattern obj (def-pattern :dk_sequence 
                            'op:bound 0 'op:element_type CORBA:tc_ulong
                            'op:type (create-sequence-tc 0 CORBA:tc_ulong)))
      ;; Write interface
      (setf (op:element_type_def obj) a-string)
      (ensure-typecode (op:element_type obj) :tk_string)
      (setf (op:bound obj) 10)
      (ensure-typecode (op:type obj) (create-sequence-tc 10 CORBA:tc_string))))

  (define-test "ArrayDef"
    (let ((obj (op:create_array repository 10 a-string)))
      (ensure-equalp (op:def_kind obj) :dk_array)
      (ensure-equalp (op:length obj) 10)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_string)
      ;; Write interface
      (setf (op:element_type_def obj) a-ulong)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_ulong)
      (setf (op:length obj) 11)
      (ensure-typecode (op:type obj) (create-array-tc 11 corba:tc_ulong))))

  (define-test "ExceptionDef"
    (let* ((members (list (CORBA:StructMember :name "a"
                                              :type CORBA:tc_void :type_def a-string)
                          (CORBA:StructMember :name "b"
                                              :type CORBA:tc_void :type_def a-ulong)))
           (obj (op:create_exception repository "IDL:my/Exception:1.0" "Exception" "1.0" members)))
      (ensure-repository
       "Exception" (def-pattern :dk_exception
                     'identity obj
                     'op:name "Exception"
                     'op:id "IDL:my/Exception:1.0"
                     'op:type (pattern 'op:kind :tk_except 
                                       'op:member_count (length members))
                     'op:members (sequence-pattern (pattern 'op:type corba:tc_string)
                                              (pattern 'op:type corba:tc_ulong))
                     'op:describe (struct-pattern
                                   'struct-class-name 'CORBA:Contained/Description
                                   'op:kind :dk_exception
                                   'op:value (struct-pattern
                                              'struct-class-name 'CORBA:ExceptionDescription
                                              'op:name (op:name obj)
                                              'op:id (op:id obj)))))
      ;; Write interface
      (setf (op:members obj) (cdr members))
      (ensure-equalp (length (op:members obj)) (1- (length members)))
      (ensure-equalp (op:member_count (op:type obj)) (1- (length members)))
      (ensure-equalp (op:kind (op:member_type (op:type obj) 0)) :tk_ulong)
      (ensure-eql (op:member_count (op:type obj)) (1- (length members)))))
  

  (define-test "AttributeDef"
    (let* ((idef (op:create_interface repository "IDL:my/Interface:1.1" "Interface" "1.1" '()))
           (obj (op:create_attribute idef "IDL:my/Att:1.1" "Att" "1.1"
                                     a-string :attr_readonly)))
      (ensure-repository
       "Interface::Att" (def-pattern :dk_attribute
                          'op:name "Att"  'op:mode :attr_readonly
                          'op:type_def a-string  'op:type corba:tc_string
                          'op:describe (struct-pattern 'op:kind :dk_attribute 
                                                       'op:value (struct-pattern 'struct-class-name'CORBA:AttributeDescription))))
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
                          :mode :param_in)))
           (exceptions '())
           (contexts '())
           (obj (op:create_operation idef id name version result mode params exceptions contexts)))
      (ensure-repository
       "Interface::Op1" (def-pattern :dk_operation
                          'identity obj
                          'op:result corba:tc_ulong
                          'op:params (sequence-pattern (pattern 'op:name "a"
                                                           'op:type corba:tc_string))
                          'op:describe (struct-pattern
                                        'op:kind :dk_operation
                                        'op:value (struct-pattern
                                                   'struct-class-name 'CORBA:OperationDescription
                                                   'op:name name 'op:id id))) )
      ;; Write interface
      (setf (op:result_def obj) a-string)
      (ensure-equalp (op:kind (op:result obj)) :tk_string)
      (setf (op:mode obj) :op_normal)
      (ensure-exception
       (setf (op:mode obj) :op_oneway)
       CORBA:BAD_PARAM  'op:minor 31)))


  (define-test "InterfaceDef"
    (let* ((id "IDL:my/Interface:1.1") (id2 "IDL:my/Interface2:1.0")
           (name "Interface")
           (version "1.1")
           (obj (op:create_interface repository id name version '()))
           (obj2 (op:create_interface repository id2 "Interface2" "1.0" '())))
      (ensure (op:is_a obj id) "isa Self")
      (ensure (op:is_a obj "IDL:omg.org/CORBA/Object:1.0") "isa Object")
      (ensure (not (op:is_a obj id2)) "not yet isa base")
      (op:create_attribute obj "IDL:my/a:1.0" "a" "1.0" a-string :attr_normal)
      (ensure-exception 
       (op:create_operation obj "IDL:my/a:1.0" "a" "1.0" a-string :op_normal nil nil nil)
       corba:bad_param 'op:minor 3)
      (setf (op:base_interfaces obj) (list obj2))
      (ensure (op:is_a obj id2) "isa base")
      (ensure-pattern* 
       obj
       'op:describe (pattern 'op:value (struct-pattern
                                        'struct-class-name 'CORBA:InterfaceDescription
                                        'op:name name 'op:id id 'op:version version
                                        'op:base_interfaces (sequence-pattern id2)))
       'op:base_interfaces (sequence-pattern (def-pattern :dk_interface 'op:id id2))
       'op:describe_interface (struct-pattern
                               'struct-class-name 'CORBA:InterfaceDef/FullInterfaceDescription
                               'op:operations (sequence-pattern)
                               'op:attributes (sequence-pattern
                                               (struct-pattern 
                                                'struct-class-name 'CORBA:AttributeDescription
                                                'op:name "a"))
                               'op:type (pattern 'op:kind :tk_objref)))
      (setf (op:base_interfaces obj) nil)
      (op:create_attribute obj2 "IDL:my2/a:1.0" "a" "1.0" a-string :attr_normal)
      (ensure-exception 
       (setf (op:base_interfaces obj) (list obj2))
       corba:bad_param 'op:minor 5)))
  



  (define-test "PrimitiveDef"
    (loop for kind in '(:PK_VOID :PK_SHORT :PK_LONG :PK_USHORT :PK_ULONG :PK_FLOAT :PK_DOUBLE
                        :PK_BOOLEAN :PK_CHAR :PK_OCTET :PK_ANY :PK_TYPECODE ;:PK_PRINCIPAL
                        :PK_STRING :PK_OBJREF :PK_LONGLONG :PK_ULONGLONG :PK_LONGDOUBLE 
                        :PK_WCHAR :PK_WSTRING :PK_VALUE_BASE)
          do (with-sub-test (kind)
               (ensure-pattern* (op:get_primitive repository kind)
                                'op:def_kind :dk_primitive
                                'op:kind kind
                                'op:type (pattern 'struct-class-name 'CORBA:TypeCode)))))



  (define-test "StringDef"
    (ensure-pattern* (op:create_string repository 10)
                     'op:def_kind :dk_string
                     'op:bound 10
                     'op:type (make-typecode :tk_string 10)))

  (define-test "WStringDef"
    (ensure-pattern* (op:create_wstring repository 10)
                     'op:def_kind :dk_wstring
                     'op:bound 10
                     'op:type (create-wstring-tc 10)))
  

  (define-test "FixedDef"
    (let ((obj (op:create_fixed repository 10 2)))
      (ensure-equalp (op:digits obj) 10)
      (ensure-equalp (op:scale obj) 2)
      (ensure-typecode (op:type obj) :tk_fixed)
      (setf (op:scale obj) 3)
      (ensure-equalp (op:fixed_scale (op:type obj)) 3)))

  (define-test "ValueBox"
    (let* ((type a-ulong)
           (id "IDL:my/ValueBox:1.1")
           (name "ValueBox")
           (version "1.1")
           (obj (op:create_value_box repository id name version type)))
      (ensure-pattern obj
                      (def-pattern :dk_valuebox
                        'op:id id 'op:name name 'op:version version
                        'op:original_type_def type
                        'op:type (pattern 'op:kind :tk_value_box
                                          'op:id id)))))

  (define-test "Native"
    (let* ((id "IDL:my/servant:1.1")
           (name "servant")
           (version "1.1")
           (obj (op:create_native repository id name version)))
      (ensure-pattern obj
                      (def-pattern :dk_native
                        'op:id id 'op:name name 'op:version version
                        'op:type (pattern 'op:kind :tk_native
                                          'op:id id)))))

  (define-test "AbstractInterfaceDef"
    (let* ((id "IDL:my/Interface:1.1")
           (name "Interface")
           (version "1.1")
           (obj (op:create_abstract_interface repository id name version '())))
      (ensure (op:is_a obj id) "isa Self")
      (ensure (op:is_a obj "IDL:omg.org/CORBA/AbstractBase:1.0") "isa AbstractBase")
      (op:create_attribute obj "IDL:my/a:1.0" "a" "1.0" a-string :attr_normal)))

  (define-test "LocalInterfaceDef"
    (let* ((id "IDL:my/Interface:1.1")
           (name "Interface")
           (version "1.1")
           (obj (op:create_local_interface repository id name version '())))
      (ensure (op:is_a obj id) "isa Self")
      (ensure (op:is_a obj "IDL:omg.org/CORBA/LocalBase:1.0") "isa LocalBase"))

    ;; Setting the inherited base_interfaces attribute causes a
    ;; BAD_PARAM exception with standard minor code 5 to be raised if
    ;; the name attribute of any object contained by this
    ;; LocalInterfaceDef conflicts with the name attribute of any
    ;; object contained by any of the specified base InterfaceDefs
    ;; (local or otherwise).

    )
    


  (define-test "ValueDef"
    (let* ((name "val") (ver "1.0") (id "IDL:my/val:1.0")
           (name2 "val2") (id2 "IDL:my/val2:1.0")
           (init (list (CORBA:Initializer 
                        :name "make-val"
                        :members (list (CORBA:StructMember :name "name" :type_def a-string)
                                       (CORBA:StructMember :name "size" :type_def a-ulong)))))
           (val (op:create_value repository id name ver nil nil nil nil nil nil init))
           (val2 (op:create_value repository id2 name2 ver nil nil val nil nil nil nil))
           (vm1 (op:create_value_member val "IDL:my/val/a:1.0" "a" "1.0" a-ulong omg.org/corba:private_member))
           (vm2 (op:create_value_member val2 "IDL:my/val/b:1.0" "b" "1.0" a-string omg.org/corba:public_member))
           (a1  (op:create_attribute val "IDL:my/val/at:1.0" "at" ver a-string :attr_readonly))
           (op1 (op:create_operation val "IDL:my/val/op1:1.0" "op1" ver a-ulong :op_normal nil nil nil)))
      (ensure-repository 
       "val" (def-pattern :dk_value  'identity val
               'op:id id 'op:name name 'op:version ver
               'op:is_abstract nil 'op:is_custom nil 'op:is_truncatable nil
               'op:supported_interfaces (sequence-pattern)
               'op:initializers (sequence-pattern (struct-pattern 'op:name "make-val"
                                                                  'op:members (sequence-pattern
                                                                               (struct-pattern 'op:name "name")
                                                                               (struct-pattern 'op:name "size"))))
               'op:base_value nil
               'op:abstract_base_values (sequence-pattern)
               'op:describe_value (struct-pattern
                                   'struct-class-name 'CORBA:ValueDef/FullValueDescription
                                   'op:id id 'op:name name 'op:version ver
                                   'op:is_abstract nil 'op:is_custom nil 'op:is_truncatable nil
                                   'op:operations (sequence-pattern
                                                   (struct-pattern 'op:name "op1"))
                                   'op:initializers (sequence-pattern (car init)))
               'op:describe (pattern
                             'op:value (struct-pattern
                                        'struct-class-name 'CORBA:ValueDescription
                                        'op:id id 'op:name name 'op:version ver
                                        'op:is_abstract nil 'op:is_custom nil 'op:is_truncatable nil )))
       
       "val2" (def-pattern :dk_value 'identity val2
                'op:supported_interfaces (sequence-pattern)
                'op:base_value val))
      (ensure-pattern
       (op:contents val :dk_attribute nil)
       (sequence-pattern a1))
      (ensure-pattern
       (op:contents val :dk_valuemember nil)
       (sequence-pattern vm1))
      (ensure-pattern
       (op:contents val2 :dk_valuemember t)
       (sequence-pattern vm2))
      (ensure-pattern
       (op:contents val2 :dk_valuemember nil)
       (sequence-pattern vm1 vm2))
      (ensure-pattern
       (op:contents val2 :dk_operation nil)
       (sequence-pattern op1))))


)
