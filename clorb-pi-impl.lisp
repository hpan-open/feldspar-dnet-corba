;;;; clorb-pi-impl.lisp -- PortableInterceptors implementation

(in-package :clorb)

#|
  local interface RequestInfo {
    readonly attribute unsigned long request_id;
    readonly attribute string operation;
    readonly attribute Dynamic::ParameterList arguments;
    readonly attribute Dynamic::ExceptionList exceptions;
    readonly attribute Dynamic::ContextList contexts;
    readonly attribute Dynamic::RequestContext operation_context;
    readonly attribute any result;
    readonly attribute boolean response_expected;
    //readonly attribute Messaging::SyncScope sync_scope;
    readonly attribute ReplyStatus reply_status;
    readonly attribute Object forward_reference;
    any get_slot (in SlotId id) raises (InvalidSlot);
    IOP::ServiceContext get_request_service_context (in IOP::ServiceId id);
    IOP::ServiceContext get_reply_service_context (in IOP::ServiceId id);
  };

  local interface ClientRequestInfo : RequestInfo {
    readonly attribute Object target;
    readonly attribute Object effective_target;
    readonly attribute IOP::TaggedProfile effective_profile;
    readonly attribute any received_exception;
    readonly attribute CORBA::RepositoryId received_exception_id;
    IOP::TaggedComponent get_effective_component (in IOP::ComponentId id);
    IOP_N::TaggedComponentSeq get_effective_components (in IOP::ComponentId id);
    CORBA::Policy get_request_policy (in CORBA::PolicyType type);
    void add_request_service_context (in IOP::ServiceContext service_context,
                                      in boolean replace);
  };
|#

;; ClientRequestInfo aspect of CORBA:Request 

(define-method request_id ((req client-request)))

(define-method "TARGET" ((self client-request))
   (request-target self))

(define-method "EFFECTIVE_TARGET" ((self client-request))
  (let ((target (request-target self)))
    (or (object-forward target) target)))

(define-method "EFFECTIVE_PROFILE" ((self client-request))
  (let ((target (op:effective_target self)))
    (let ((profile-id (car (rassoc (selected-profile target) 
                                   (object-profiles target)))))
      (assoc profile-id (raw-profiles target)))))


(define-method "RECEIVED_EXCEPTION" ((self client-request))
  (ERROR 'OMG.ORG/CORBA:NO_IMPLEMENT))

(define-method "RECEIVED_EXCEPTION_ID" ((self client-request))
  (ERROR 'OMG.ORG/CORBA:NO_IMPLEMENT))

(define-method "GET_EFFECTIVE_COMPONENT" ((self client-request) _ID)
  (DECLARE (IGNORE _ID))
  (ERROR 'OMG.ORG/CORBA:NO_IMPLEMENT))

(define-method "GET_EFFECTIVE_COMPONENTS" ((self client-request) _ID)
  (DECLARE (IGNORE _ID))
  (ERROR 'OMG.ORG/CORBA:NO_IMPLEMENT))

(define-method "GET_REQUEST_POLICY" ((self client-request)
                                     _TYPE)
  (DECLARE (IGNORE _TYPE))
  (ERROR 'OMG.ORG/CORBA:NO_IMPLEMENT))

(define-method "ADD_REQUEST_SERVICE_CONTEXT" ((self client-request)
                                                    service_context replace)
  (let* ((list (service-context-list self))
         (old (find (op:context_id service_context) list
                    :key #'op:context_id)))
    (when old
      (unless replace
        (error 'omg.org/corba:bad_inv_order :minor 11 :completed :completed_no))
      (setf list (delete old list)))
    (setf (service-context-list self) (cons service_context list))))




(define-method "REQUEST_ID" ((self client-request))
  (request-req-id self))

(define-method "OPERATION" ((self client-request))
  (request-operation self))

(define-method "ARGUMENTS" ((self client-request))
  (map 'list
       (lambda (param)
         (Dynamic:Parameter :argument (op:argument param)
                            :mode (op:arg_modes param)))
       (cdr (request-paramlist self))))

(define-method "EXCEPTIONS" ((self client-request))
   (request-exceptions self))

(define-method "CONTEXTS" ((self client-request))
  nil)

(define-method "OPERATION_CONTEXT" ((self client-request))
  nil)

(define-method "RESULT" ((self client-request))
  (op:return_value self))

(define-method "RESPONSE_EXPECTED" ((self client-request))
  (response-expected self))

(define-method "REPLY_STATUS" ((self client-request))
#| FIXME:
This attribute describes the state of the result of the operation invocation. Its value can
be one of the following:
PortableInterceptor::SUCCESSFUL
PortableInterceptor::SYSTEM_EXCEPTION
PortableInterceptor::USER_EXCEPTION
PortableInterceptor::LOCATION_FORWARD
PortableInterceptor::TRANSPORT_RETRY
|#
  (request-status self))

(define-method "FORWARD_REFERENCE" ((self client-request))
  (request-forward self))

(define-method "GET_SLOT" ((self client-request) _ID)
  (DECLARE (IGNORE _ID))
  (ERROR 'OMG.ORG/CORBA:NO_IMPLEMENT))

(define-method "GET_REQUEST_SERVICE_CONTEXT" ((self client-request) id)
  (or (find id (service-context-list self) :key #'op:context_id)
      (error 'omg.org/corba:bad_param :minor 23)))


(define-method "GET_REPLY_SERVICE_CONTEXT" ((self client-request) id)
  (or (find id (reply-service-context-list self) :key #'op:context_id)
      (error 'omg.org/corba:bad_param :minor 23)))



;;;; Test interceptor

(defun about-to-send-request (req)
  (setf (client-request-interceptors req) nil)
  (dolist (interceptor (client-request-interceptors (the-orb req)))
    (push interceptor (client-request-interceptors req))
    (handler-case
      (op:send_request interceptor req)
      (systemexception (exc)
                       (setf (request-exception req) exc)
                       (about-to-receive-exception req)))))


(defun about-to-receive-exception (req)
  (dolist (interceptor (client-request-interceptors req))
    (handler-case
      (op:receive_exception interceptor req)
      (systemexception (exc) (setf (request-exception req) exc)))))


(defun about-to-receive-reply (req)
  (handler-case
    (do ()
        ((null (client-request-interceptors req)))
      (op:receive_reply (pop (client-request-interceptors req)) req))
    (systemexception (exc)
                     (setf (request-exception req) exc)
                     (about-to-receive-exception req))))


(defclass my-client-interceptor (omg.org/portableinterceptor:clientrequestinterceptor)
  ((name :initarg :name)))

(define-method name ((self my-client-interceptor))
  (slot-value self 'name))

(define-method "SEND_REQUEST" ((self my-client-interceptor) info)
  (mess 3 "SEND_REQUEST: ~S" info)
  (omg.org/features:add_request_service_context 
   info 
   (iop:ServiceContext :context_id 17 :context_data #(1))
   nil))

;; (define-method "SEND_POLL" ((self my-client-interceptor) info)
;;  (declare (ignore info)))


(define-method "RECEIVE_REPLY" ((self my-client-interceptor) info)
  (mess 3 "RECEIVE_REPLY: ~S" (ignore-errors (omg.org/features:get_reply_service_context info 17)))
)

(define-method "RECEIVE_EXCEPTION" ((self my-client-interceptor) info)
  (mess 3 "RECEIVE_EXCEPTION: ~S" (ignore-errors (omg.org/features:get_reply_service_context info 17)))
)

(define-method "RECEIVE_OTHER" ((self my-client-interceptor) info)
  (mess 3 "RECEIVE_OTHER")
)


(defvar *my-interceptor* (make-instance 'my-client-interceptor
                           :name "Test client-interceptor"))

#|
(pushnew *my-interceptor* (client-request-interceptors *the-orb*))
|#