;;; test-x
(in-package :clorb)


(define-test-suite "Test Server Request Processing"
  (variables
   (*log-level* 5))

  (define-test "Process Cancel Request"
    (setup-test-in)
    
    ;; Create some dummy requests "in processing", we will cancel two of them
    (loop for (id state) in '((0 :wait) (1 :exec) (2 :wait) (3 :wait))
          do (push (create-server-request *the-orb* :request-id id
                                          :state state :response-flags 1)
                   (connection-server-requests *test-in-conn*)))
    ;; Write a Cancel Request message
    (test-write-request
     :message-type 2                   ; CancelRequest
     :message (list (giop:cancelrequestheader :request_id 1)))
    (test-write-request
     :message-type 2                   ; CancelRequest
     :message (list (giop:cancelrequestheader :request_id 2)))
    
    ;; Let the server code process the CancelRequest
    (orb-work *the-orb* nil t)
    
    ;; Check result
    (ensure-equalp 
     (sort (loop for req in (connection-server-requests *test-in-conn*)
                 collect (list (request-id req)
                               (response-expected req)
                               (request-state req)))
           #'< :key #'car)
     '((0 t :wait) (1 nil :exec) (2 nil :canceled) (3 t :wait))))


  (define-test "Simple request"
    (test-request-response
     :request-type 0
     :request (list (giop:requestheader_1_0
                     :request_id 0
                     :service_context nil
                     :response_expected t
                     :object_key #(0)
                     :operation "_non_existent"
                     :requesting_principal #()))
    :response (list :message-type :reply
                    :version giop-1-0
                    'giop:replyheader 
                    (pattern 'op:request_id 0
                             'op:reply_status :system_exception)
                    corba:tc_string 
                    "IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0" )))


  (define-test "Illegal version"
    (test-request-response
     :request (list (giop:messageheader_1_1
                     :magic "GIOP" 
                     :giop_version (giop:version :major 1 :minor 2)
                     :flags 1
                     :message_type 0
                     :message_size 0))
     :response (list :message-type :messageerror
                     :version giop-1-1)))

  (define-test "Respond with same version"
    (test-request-response
     :request-type 0
     :request (list :version giop-1-1
                    (giop:requestheader_1_1
                     :request_id 0
                     :service_context nil
                     :response_expected t
                     :reserved #(0 0 0)
                     :object_key #(0)
                     :operation "_non_existent"
                     :requesting_principal #()))
     :response (list :message-type :reply
                     :version giop-1-1
                     'giop:replyheader 
                     (pattern 'op:request_id 0
                              'op:reply_status :system_exception)
                     corba:tc_string 
                     "IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0" )))

  (define-test "Locate request"
    (test-request-response
     :request-type :locaterequest
     :request (list :version giop-1-1
                    (giop:locaterequestheader
                     :request_id 0
                     :object_key #(0)))
     :response (list :message-type :locatereply
                     :version giop-1-1
                     'giop:locatereplyheader
                     (pattern 'op:request_id 0
                              'op:locate_status :unknown_object))))


#|end|# )

