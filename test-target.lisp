;;;; test-target

(in-package :clorb)

(define-test-suite "Target Code generator"
  
  (variables
   (repository (make-instance 'repository))
   (module (op:create_module repository "IDL:test/Mod:1.0" "Mod" "1.0"))
   (interface (op:create_interface module "IDL:test/Mod/Intf:1.0" "Intf" "1.0" nil)))
  
  
  (define-test "constant"
    (let ((const-def (op:create_constant repository "IDL:test/a:1.0" "a" "1.0"
                                         (op:get_primitive repository :pk_long)
                                         (CORBA:Any :any-value 192
                                                    :any-typecode CORBA:tc_long))))
      (ensure-pattern (target-code const-def (make-instance 'stub-target))
                      (sexp-pattern `(defconstant omg.org/root::a ,(eval-to 192))))))
  
  (define-test "struct"
    (let* ((members (list (CORBA:StructMember :name "a"
                                              :type_def (op:get_primitive repository :pk_long))))
           (struct-def (op:create_struct repository "IDL:test/s:1.0" "s" "1.0" members)))
      (ensure-pattern (target-code struct-def (make-instance 'stub-target))
                      (sexp-pattern `(define-struct omg.org/root::s
                                       &key
                                       (:id :required "IDL:test/s:1.0")
                                       (:name :required "s")
                                       (:members :required (("a" &any &any)))
                                       (:read :optional )
                                       (:write :optional ) )))))

  (define-test "alias-def"
    (let ((alias-def (op:create_alias repository "IDL:test/a:1.0" "a" "1.0" 
                                      (op:get_primitive repository :pk_long))))
      (ensure-pattern (target-code alias-def (make-instance 'stub-target))
                      (sexp-pattern `(define-alias omg.org/root::a 
                                       &key 
                                       (:id :required "IDL:test/a:1.0")
                                       (:name :required "a")
                                       (:type :required CORBA:Long)
                                       (:typecode :required CORBA:tc_long))))))

  (define-test "exception"
    (let* ((members (list (CORBA:StructMember :name "a"
                                              :type_def (op:get_primitive repository :pk_long))))
           (exc (op:create_exception repository "IDL:test/e:1.0" "e" "1.0" members)))
      (ensure-pattern (target-code exc (make-instance 'stub-target))
                      (sexp-pattern `(define-user-exception omg.org/root::e &key
                                       (:name :required "e")
                                       (:id :required "IDL:test/e:1.0")
                                       (:members :required (("a" CORBA:tc_long))))))))
  
  
  
)
        