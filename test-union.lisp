(in-package :clorb)

(define-test-suite "Test Union" 
  (variables 
   )

  (define-test "Create Union TC"
    (let* ((members (list (list 1 "lt" corba:tc_long)
                          (list 2 "lt" corba:tc_long)
                          (list 3 "name" corba:tc_string)
                          (list t "expr" corba:tc_string)))
           (tc (create-union-tc "IDL:filter:1.0" "filter" corba:tc_long members)))
      
      (ensure-typep tc 'CORBA:TypeCode)
      (ensure-eql (op:kind tc) :tk_union)
      (ensure-equalp (op:name tc) "filter")
      (ensure-equalp (op:id tc) "IDL:filter:1.0")
      (ensure-equalp (op:member_count tc) (length members))
      (ensure-equalp (omg.org/features:default_index tc) (position t members :key #'car))
      (ensure-equalp (omg.org/features:discriminator_type tc) corba:tc_long)
      (loop for i from 0 below (op:member_count tc)
            for m in members
            do 
            (unless (eq i (op:default_index tc))
              (ensure-eql (op:member_label tc i) (first m)))
            (ensure-equalp (op:member_name tc i) (second m))
            (ensure-equalp (op:member_type tc i) (third m)))))
              


)
