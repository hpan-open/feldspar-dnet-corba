(in-package :clorb)

(define-test-suite "IIR test"
  (macrolet
      ((with-repository (&body body)
         `(let ((*repository* (make-hash-table :test #'equal))
                (*typecode-repository* (make-hash-table :test #'equal)))
           ,@body)))

    (define-test "Simplify recursive TC"
      (with-repository
        (let* ((id "IDL:foo:1.0")
               (struct (create-struct-tc id "foo" `(("x" ,corba:tc_null)))))
          (setf (second (elt (tc-members struct) 0)) struct)
          (let ((new-type (simplify-type struct)))
            (let ((from-rep (gethash id *typecode-repository*)))
              (ensure (eq from-rep new-type)
                      "identical typecode in repository")
              (ensure-equalp (op:id new-type) id)
              (ensure-equalp (op:member_count new-type) 1)
              (ensure-equalp (op:member_name new-type 0) "x")
              (let* ((struct (create-struct-tc id "foo" `(("x" ,corba:tc_null)))))
                (setf (second (elt (tc-members struct) 0)) struct)
                (let ((new-type (simplify-type struct)))
                  (ensure (eq from-rep new-type)
                          "identical typecode in repository second time"))))))
        ))))
