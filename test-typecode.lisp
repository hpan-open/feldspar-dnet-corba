(in-package :clorb)

(define-test-suite "Typecodes"
  (variables )

  (define-test "constants"
    (ensure-equalp (op:name CORBA:tc_object) "Object")
    (ensure-equalp (op:id CORBA:tc_object) "IDL:omg.org/CORBA/Object:1.0")
    (ensure-equalp (op:length CORBA:tc_string) 0))

  (define-test "Enum"
    (let* ((id "IDL:enum:1.0")
           (name "enumen")
           (members '("fie" "foe"))
           (tc (create-enum-tc id name members)))
      (ensure-pattern tc (pattern 'op:kind :tk_enum  'op:id id  'op:name name
                                  'op:member_count (length members)))
      (loop for m in members for i from 0 do
            (ensure-equalp (op:member_name tc i) m))))

  (define-test "Alias"
    (let* ((id "IDL:alias:1.0") (name "alias"))
      (ensure-pattern (create-alias-tc id name corba:tc_long)
                      (pattern 'op:kind :tk_alias 'op:id id 'op:name name
                               'op:content_type corba:tc_long))))

  (define-test "Sequence"
    (let* ((el-type corba:tc_string)
           (seq-tc (create-sequence-tc 100 el-type)))
      (ensure-equalp (op:kind seq-tc) :tk_sequence)
      (ensure-equalp (op:length seq-tc) 100)
      (ensure-equalp (op:content_type seq-tc) el-type)
      (ensure (not (op:equal el-type seq-tc)))))

  (define-test "Union"
    (let* ((d-type corba:tc_ushort)
           (e1-type corba:tc_ulong)
           (e2-type corba:tc_string)
           (e3-type (create-sequence-tc 0 corba:tc_octet))
           (members `((10 "e1" ,e1-type)
                      (20 "e2" ,e2-type)
                      (33 "e3" ,e3-type) ))
           (id "IDL:my/Union:1.1")
           (name "Union")
           (tc (make-typecode :tk_union id name d-type 0 members)))
      (ensure-equalp (op:kind tc) :tk_union)
      (ensure-equalp (op:id tc) id)
      (ensure-equalp (op:name tc) name)
      (ensure-equalp (op:member_count tc) 3)
      (ensure-equalp (op:discriminator_type tc) d-type)
      (ensure-equalp (op:default_index tc) 0)
      (loop for m in members and i from 0 do
            (ensure-equalp (op:member_name tc i) (second m))
            (ensure-equalp (op:member_type tc i) (third m))
            (ensure-equalp (op:member_label tc i) (first m)))
      (ensure (op:equal tc (make-typecode :tk_union id name d-type 0 (copy-list members))))
      (ensure (not (op:equal tc (make-typecode :tk_union id name d-type 0 (cdr members)))))))

  (define-test "Recursive"
    (let ((sym (gensym)))
      (set-symbol-typecode sym 
                           (lambda () 
                             (create-struct-tc
                              "IDL:Recursive_1/Node:1.0"
                              "Node"
                              (list (list "children"
                                          (create-sequence-tc 0 (symbol-typecode sym)))))))
      (let ((tc (symbol-typecode sym)))
        (ensure-typep tc 'corba:typecode)
        (ensure-eql (typecode-kind tc) :tk_struct)
        (ensure-eql (op:content_type (op:member_type tc 0))
                    tc))))

  (define-test "Fixed"
    (let ((tc (make-typecode :tk_fixed 10 2)))
      (ensure-equalp (op:fixed_digits tc) 10)
      (ensure-equalp (op:fixed_scale tc) 2)))
  

  (define-test "TypeCodeFactory"
    (let ((factory (make-instance 'CORBA:TYPECODEFACTORY)))
      (ensure-pattern (op:CREATE_ARRAY_TC factory 2 CORBA:tc_short)
                      (pattern 'op:kind :tk_array 'op:length 2 'op:content_type CORBA:tc_short))
      (ensure-pattern (op:CREATE_SEQUENCE_TC factory 0 CORBA:tc_short)
                      (pattern 'op:kind :tk_sequence))
      (ensure-pattern (op:CREATE_FIXED_TC factory 10 2)
                      (pattern 'op:kind :tk_fixed 'op:fixed_digits 10 'op:fixed_scale 2))
      (ensure-pattern (op:CREATE_WSTRING_TC factory 0)
                      (pattern 'op:kind :tk_wstring 'op:length 0))
      (ensure-pattern (op:CREATE_STRING_TC factory 12)
                      (pattern 'op:kind :tk_string 'op:length 12))
      (ensure-pattern (op:CREATE_INTERFACE_TC factory "IDL:I:1.0" "I")
                      (pattern 'op:kind :tk_objref 'op:name "I"))
      (ensure-pattern (op:CREATE_EXCEPTION_TC factory "IDL:e:1.0" "e" nil)
                      (pattern 'op:kind :tk_except 'op:name "e"))
      (ensure-pattern (op:CREATE_ALIAS_TC factory "IDL:a:1.0" "a" CORBA:tc_ushort)
                      (pattern 'op:kind :tk_alias 'op:id "IDL:a:1.0" 'op:name "a" ))
      (ensure-pattern (op:CREATE_ENUM_TC factory "IDL:e:1.0" "e" '("A" "B"))
                      (pattern 'op:kind :tk_enum 'op:name "e" 'omg.org/features:member_count 2))
      (ensure-pattern (op:create_union_tc factory "IDL:u:1.0" "u" omg.org/corba:tc_boolean
                                          (list (omg.org/corba:unionmember
                                                 :name "a" :type corba:tc_string
                                                 :label (any :any-value t :any-typecode omg.org/corba:tc_boolean))
                                                (omg.org/corba:unionmember
                                                 :name "b" :type corba:tc_long
                                                 :label (any :any-value nil :any-typecode omg.org/corba:tc_boolean))))
                      (pattern 'op:kind :tk_union 'op:member_count 2))
      (ensure-pattern (op:CREATE_STRUCT_TC factory "IDL:s:1.0" "s"
                                           (list (corba:structmember :name "a" :type  CORBA:tc_long)
                                                 (corba:structmember :name "b" :type CORBA:tc_long)))
                      (pattern 'op:kind :tk_struct 'op:name "s" 
                               'omg.org/features:member_count 2))
      
      (let ((recursive_tc (op:create_recursive_tc factory "IDL:V:1.0")))
        (let ((tc (op:create_struct_tc factory "IDL:V:1.0" "V" 
                                       (list (CORBA:StructMember
                                              :name "member"
                                              :type recursive_tc)))))
          (ensure-typecode recursive_tc tc)))))




)
