(in-package :clorb)


(define-operation test-op-1
  :id "IDL:test/op1:1.0"
  :name "op1"
  :version "1.0"
  :mode :op_oneway
  :parameters (("x" :param_in corba:tc_string)))

(define-operation test-op-2
  :id "IDL:test/op2:1.0"
  :name "op2"
  :version "1.0"
  :mode :op_normal
  :result CORBA:tc_long
  :parameters (("x" :param_inout corba:tc_string)))

(define-attribute test-at-1
  :id "IDL:test/at1:1.0"
  :name "at1"
  :mode :attr_normal
  :type CORBA:tc_string)



(define-test-suite "Client Request"
  (variables
   (orb (CORBA:ORB_init)))

  (define-test "one way static call"
    (setup-test-out)
    (let ((obj (test-object orb))
          (n -129988))
      (let ((*io-event-queue* nil))
        (static-call ("foo" obj)
                     :output ((buffer) (marshal-any-value n buffer))
                     :no-response t))
      (test-read-request 
       :request-keys '((:response 0) (:operation "foo"))
       :args (list n))))
        

  (define-test "DII" 
    (setup-test-out)
    (let* ((obj (test-object orb))
           (a1 "hepp")
           (args (list (CORBA:NamedValue :argument a1 :arg_modes CORBA:ARG_IN)))
           (ret (CORBA:NamedValue :argument (CORBA:Any :any-typecode CORBA:tc_long))))
      (multiple-value-bind (result req)
                           (op:_create_request obj nil "op" args ret 0)
        (declare (ignore result))
        (op:send_deferred req)
        (test-read-request 
         :request-keys '((:response 1) (:operation "op") (:object-key #(17)))
         :args (list a1))
        (test-write-response req '(224412))
        (orb-work orb nil t)
        (assert (op:poll_response req) () "should have gotten the response")
        (op:get_response req)
        (ensure-eql (corba:any-value (op:return_value req)) 224412))))


  (define-test "jit-call oneway"
    (setup-test-out)
    (let* ((obj (test-object orb))
           (*io-event-queue* nil))
      (%jit-call test-op-1 obj "hej")
      (test-read-request
       :request-keys '((:response 0) (:operation "op1") (:object-key #(17)))
       :args '("hej"))))


  (define-test "jit-call normal"
    (setup-test-out)
    (let* ((obj (test-object orb))
           (*io-event-queue* nil))
      (setf (response-func *test-out-conn*)
            (lambda (req)
              (test-read-request :args '("hej"))
              (test-write-response req '(9977 "jolly"))))
      (multiple-value-bind (r1 r2)
                           (%jit-call test-op-2 obj "hej")
        (ensure-equalp r1 9977)
        (ensure-equalp r2 "jolly"))))

  (define-test "jit attr"
    (setup-test-out)
    (let* ((obj (test-object orb))
           (*io-event-queue* nil))
      (setf (response-func *test-out-conn*)
            (lambda (req)
              (test-read-request :request-keys '((:operation "_get_at1")))
              (test-write-response req '("jolly"))))
      (multiple-value-bind (r1) (%jit-get test-at-1 obj)
        (ensure-equalp r1 "jolly"))
      (setf (response-func *test-out-conn*) 
            (lambda (req)
              (test-read-request 
               :request-keys '((:operation "_set_at1"))
               :args '("fnord")) 
              (test-write-response req '())))
      (%jit-set test-at-1 obj "fnord")))


  (define-test "framgmented reply"
    (setup-test-out)
    (setup-outgoing-connection *test-out-conn*)
    (let ((req (create-client-request
                orb :request-id 1)))
      (connection-add-client-request *test-out-conn* req)
      (let ((buffer (get-work-buffer orb)))
        (marshal-giop-header :REPLY buffer giop-1-1 t)
        (marshal-service-context nil buffer) 
        (marshal-ulong 1  buffer)       ;req id
        (marshal :no_exception (symbol-typecode 'GIOP:REPLYSTATUSTYPE) buffer)
        (marshal-giop-set-message-length buffer)
        (let ((octets (buffer-octets buffer)))
          (io-descriptor-set-write *test-response-desc* octets 0
                                   (length octets))))
      (let ((buffer (get-work-buffer orb)))
        (marshal-giop-header :FRAGMENT buffer giop-1-1 nil)
        (dolist (any '(1 "hello"))
          (marshal-any-value any buffer))
        (marshal-giop-set-message-length buffer)
        (let ((octets (buffer-octets buffer)))
          (io-descriptor-set-write *test-response-desc* octets 0
                                   (length octets))))
      (orb-work orb nil t)
      (ensure-pattern* req
                       'request-status :no_exception
                       'request-buffer (pattern 'unmarshal-short 1
                                                'unmarshal-string "hello")) ))




#| end suite |# )
