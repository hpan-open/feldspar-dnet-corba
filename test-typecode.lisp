(in-package :clorb)

(define-test-suite "Typecodes"
  (variables
   )

  (define-test "Enum"
    (let* ((id "IDL:enum:1.0")
           (name "enumen")
           (members '("fie" "foe"))
           (tc (make-typecode :tk_enum id name members)))
      (ensure-equalp (op:kind tc) :tk_enum)
      (ensure-equalp (op:id tc) id)
      (ensure-equalp (op:name tc) name)
      (ensure-equalp (op:member_count tc) (length members))
      (loop for m in members 
            for i from 0 do
            (ensure-equalp (op:member_name tc i) m))))
  
  (define-test "Sequence"
    (let* ((el-type corba:tc_string)
           (seq-tc (make-sequence-typecode el-type 100)))
      (ensure-equalp (op:kind seq-tc) :tk_sequence)
      (ensure-equalp (op:length seq-tc) 100)
      (ensure-equalp (op:content_type seq-tc) el-type)
      (ensure (not (op:equal el-type seq-tc)))))

  (define-test "Union"
    (let* ((d-type corba:tc_ushort)
           (e1-type corba:tc_ulong)
           (e2-type corba:tc_string)
           (e3-type (make-sequence-typecode corba:tc_octet 0))
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



)
