(in-package :clorb)

(defmacro debug-macro (form &environment env)
  (let ((expansion (macroexpand-1 form env)))
    ;; Do the expansion first and then bind *print-pretty* in case
    ;; the expansion is sensitive to the binding of that variable.
    (let ((*print-pretty* t))
      (format t "~S~%==> ~S" form expansion))
    ;; Return the original form, not the expansion, in case the caller
    ;; is himself calling MACROEXPAND-1 and wants to have some special
    ;; action based on partial expansions (as happens with SETF, for
    ;; some cases).
    form))

(define-test-suite "POA Test"
  (variables 
   (orb *the-orb*)
   (*last-poaid* 0)
   (*poa-map* (make-hash-table :test #'eql))
   (root-poa (create-POA nil "root" nil nil orb)))
  
  
  (define-test "Policy creation"
    (let ((p1 (op:create_id_assignment_policy root-poa :user_id)))
      (ensure-typep p1 'CORBA:Policy)
      (ensure-typep p1 'omg.org/portableserver:idassignmentpolicy)
      (ensure-eql (op:value p1) :user_id)
      (op:destroy p1))
    (let ((p1 (op:create_policy orb portableserver:id_assignment_policy_id
                                :system_id)))
      (ensure-typep p1 'CORBA:Policy)
      (ensure-typep p1 'omg.org/portableserver:idassignmentpolicy)
      (ensure-eql (op:value p1) :system_id)
      (op:destroy p1))
    ;; Check that correct exception is signalled
    (handler-case
      (progn (op:create_id_assignment_policy root-poa :foo)
             (ensure nil))
      (CORBA:POLICYERROR (v)
                         (ensure-equalp (op:reason v) corba:bad_policy_type))))
  
  (define-test "Create POA"
    (let* ((p1 (op:create_id_assignment_policy root-poa :user_id))
           (p2 (op:create_request_processing_policy root-poa :use_default_servant))
           (name "TestP")
           (poa (op:create_poa root-poa name nil (list p1 p2))))
      (ensure-typep poa 'PortableServer:POA)
      
      ;; Unique POAManager created
      (ensure (not (eql (op:the_poamanager root-poa)
                        (op:the_poamanager poa))))
      
      (ensure-equalp (op:the_name poa) name)
      (ensure-eql (op:the_parent poa) root-poa)
      
      (let ((poa0 (op:find_POA root-poa name nil)))
        (ensure-eql poa0 poa))
      
      (handler-case 
        (progn (op:create_poa root-poa name nil nil)
               (ensure nil))
        (OMG.ORG/PORTABLESERVER:POA/ADAPTERALREADYEXISTS ()))

      (let ((obj (op:create_reference_with_id poa (string-to-oid "Foo") "IDL:if:1.0")))
        (ensure-typep obj 'CORBA:Proxy))))
  
  (define-test "Destroy POA"
    (let* ((poa1 (op:create_poa root-poa "p1" nil nil))
           (id1 (poa-poaid poa1))
           (poa2 (op:create_poa poa1 "p2" nil nil))
           (id2 (poa-poaid poa2)))
      (ensure-eql (gethash id1 *poa-map*) poa1)
      (ensure-eql (gethash id2 *poa-map*) poa2)
      (op:destroy poa1 t t)
      (handler-case
        (progn (op:find_POA root-poa "p1" nil)
               (ensure nil))
        (OMG.ORG/PORTABLESERVER:POA/ADAPTERNONEXISTENT ()))
      (ensure-eql (gethash id1 *poa-map*) nil)
      (ensure-eql (gethash id2 *poa-map*) nil)
      (ensure-exception
       (op:create_POA poa1 "p9" nil nil)
       CORBA:BAD_INV_ORDER 'op:minor (std-minor 17))))

  (define-test "POA Activator"
    (defclass test-activator (PortableServer:ADAPTERACTIVATOR)
      ())
    (define-method "UNKNOWN_ADAPTER" ((OBJ test-activator) _parent _name)
      (op:create_poa _parent _name nil nil )
      t)
    (setf (op:the_activator root-poa) (make-instance 'test-activator))
    (let ((name "foo"))
      (let ((poa (op:find_poa root-poa name t)))
        (ensure-typep poa 'PortableServer:POA)
        (ensure-equalp (op:the_name poa) name))))
  
  
  (define-test "set_servant_manager"
     (handler-case 
       (progn (op:set_servant_manager root-poa nil)
              (ensure nil))
       (omg.org/portableserver:poa/wrongpolicy () nil))
     (let ((poa (op:create_POA root-poa "p" nil
                               (list (op:create_request_processing_policy root-poa :use_servant_manager)
                                     (op:create_servant_retention_policy root-poa :retain)))))
       (ensure-exception
        (op:set_servant_manager poa nil)
        CORBA:OBJ_ADAPTER 'op:minor (std-minor 4))
       (let ((manager (make-instance 'PortableServer:ServantActivator)))
         (op:set_servant_manager poa manager)
         (ensure-exception   ; only set once
          (op:set_servant_manager poa manager)
           CORBA:BAD_INV_ORDER 'op:minor (std-minor 6)))))
   
  (define-test "get_servant"
    (handler-case (progn (op:get_servant root-poa)
                      (ensure nil "get_servant on Wrong Policy POA."))
      (omg.org/portableserver:poa/wrongpolicy ()))
    (let ((poa (op:create_POA root-poa "p" nil
                           (list (op:create_request_processing_policy 
                                       root-poa :use_default_servant)))))
      (ensure-exception (op:get_servant poa)
                        Portableserver:POA/Noservant)))

  (define-test "reference creation"
    (let ((id "IDL:myObj:1.0")
          (user-poa (op:create_POA root-poa "user" nil
                                   (list (op:create_id_assignment_policy root-poa :user_id)))))
      (let ((obj (op:create_reference root-poa id)))
        (ensure-pattern* obj 'proxy-id id 'the-orb orb)
        (ensure-typep obj 'CORBA:Proxy))
      (let* ((oid (string-to-oid "foo"))
             (obj (op:create_reference_with_id user-poa oid id)))
        (ensure-typep obj 'CORBA:Proxy)
        (ensure-equalp (proxy-id obj) id)
        (let ((ior-string (op:object_to_string orb obj)))
          (let ((obj2 (op:string_to_object orb ior-string)))
            (ensure (op:_is_equivalent obj obj2))
            (ensure-equalp (proxy-id obj2) id)))
        (ensure-equalp (op:reference_to_id user-poa obj) oid ))))

#| end suite |# )