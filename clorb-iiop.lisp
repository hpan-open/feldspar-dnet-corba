;;; clorb-iiop.lisp --- IIOP implementation

(in-package :clorb)

(defvar *request-id-seq* 0)
(defvar *corba-waiting-requests* nil)

(defparameter *iiop-header-size* 12)


;;;; GIOP Module

(define-enum GIOP:MsgType_1_0
  :id "IDL:GIOP/MsgType_1_0:1.0"
  :name "MsgType_1_0"
  :members ("Request" "Reply" "CancelRequest" "LocateRequest" "LocateReply" 
            "CloseConnection" "MessageError"))

(DEFINE-STRUCT GIOP:VERSION
  :id "IDL:GIOP/Version:1.0"
  :name "Version"
  :members (("major" OMG.ORG/CORBA:TC_OCTET MAJOR)
            ("minor" OMG.ORG/CORBA:TC_OCTET MINOR)))

(DEFINE-STRUCT GIOP:MESSAGEHEADER_1_0
  :id "IDL:GIOP/MessageHeader_1_0:1.0"
  :name "MessageHeader_1_0"
  :members (("magic" (create-array-tc 4 OMG.ORG/CORBA:TC_CHAR) MAGIC)
            ("GIOP_version" (SYMBOL-TYPECODE 'GIOP:VERSION) GIOP_VERSION)
            ("byte_order" OMG.ORG/CORBA:TC_BOOLEAN BYTE_ORDER)
            ("message_type" OMG.ORG/CORBA:TC_OCTET MESSAGE_TYPE)
            ("message_size" OMG.ORG/CORBA:TC_ULONG MESSAGE_SIZE)))

(DEFINE-STRUCT GIOP:LOCATEREQUESTHEADER
 :id "IDL:GIOP/LocateRequestHeader:1.0"
 :name "LocateRequestHeader"
 :members (("request_id" OMG.ORG/CORBA:TC_ULONG REQUEST_ID)
           ("object_key" (create-sequence-tc 0 OMG.ORG/CORBA:TC_OCTET) OBJECT_KEY)))

(define-enum GIOP:LocateStatusType
  :id "IDL:GIOP/LocateStatusType:1.0"
  :name "LocateStatusType" 
  :members ("UNKNOWN_OBJECT" "OBJECT_HERE" "OBJECT_FORWARD"))

(DEFINE-STRUCT GIOP:LOCATEREPLYHEADER
 :id "IDL:GIOP/LocateReplyHeader:1.0"
 :name "LocateReplyHeader"
 :members (("request_id" OMG.ORG/CORBA:TC_ULONG REQUEST_ID) 
           ("locate_status" (SYMBOL-TYPECODE 'GIOP:LOCATESTATUSTYPE) LOCATE_STATUS)))

(DEFINE-STRUCT GIOP:CANCELREQUESTHEADER
 :id "IDL:GIOP/CancelRequestHeader:1.0"
 :name "CancelRequestHeader"
 :members (("request_id" OMG.ORG/CORBA:TC_ULONG REQUEST_ID)))

(DEFINE-STRUCT GIOP:REQUESTHEADER_1_0
  :id "IDL:GIOP/RequestHeader_1_0:1.0"
  :name "RequestHeader_1_0"
  :members (("service_context" (SYMBOL-TYPECODE 'IOP:SERVICECONTEXTLIST) SERVICE_CONTEXT)
            ("request_id" OMG.ORG/CORBA:TC_ULONG REQUEST_ID)
            ("response_expected" OMG.ORG/CORBA:TC_BOOLEAN RESPONSE_EXPECTED)
            ("object_key" (create-sequence-tc NIL OMG.ORG/CORBA:TC_OCTET) OBJECT_KEY)
            ("operation" OMG.ORG/CORBA:TC_STRING OPERATION)
            ("requesting_principal" (SYMBOL-TYPECODE 'GIOP:PRINCIPAL) REQUESTING_PRINCIPAL)))

(define-enum GIOP:REPLYSTATUSTYPE 
  :id "IDL:GIOP/ReplyStatusType:1.0"
  :name "ReplyStatusType"
  :members ("NO_EXCEPTION" "USER_EXCEPTION" "SYSTEM_EXCEPTION" "LOCATION_FORWARD"))

(DEFINE-STRUCT GIOP:REPLYHEADER
  :id "IDL:GIOP/ReplyHeader:1.0"
  :name "ReplyHeader"
  :members (("service_context" (SYMBOL-TYPECODE 'IOP:SERVICECONTEXTLIST) SERVICE_CONTEXT)
            ("request_id" OMG.ORG/CORBA:TC_ULONG REQUEST_ID)
            ("reply_status" (SYMBOL-TYPECODE 'GIOP:REPLYSTATUSTYPE) REPLY_STATUS)))

(define-alias GIOP:Principal
  :id "IDL:GIOP/Principal:1.0"
  :name"Principal" 
  :type sequence
  :typecode (create-sequence-tc 0 OMG.ORG/CORBA:TC_OCTET))



;;;; GIOP (un)marshal extras

(defun marshal-giop-set-message-length (buffer)
  (with-out-buffer (buffer)
    (let ((len pos))
      (setf pos 8)
      (marshal-ulong (- len 12) buffer)
      (setf pos len))))


(defun marshal-service-context (ctx buffer)
  (marshal-sequence ctx #'marshal-tagged-component buffer))

(defun unmarshal-service-context (buffer)
  (unmarshal-sequence-m (buffer) 
    (IOP:ServiceContext :context_id (unmarshal-ulong buffer)
	                :context_data (unmarshal-osequence buffer))))

;;;; Utils

(defun marshal-any-value (any buffer)
  (marshal (any-value any)
           (any-typecode any)
           buffer))

(defun find-waiting-request (request-id)
  (let ((req (find request-id *corba-waiting-requests*
                   :key #'request-req-id)))
    (if req
        (setq *corba-waiting-requests*
              (delete req *corba-waiting-requests*))
        (mess 4 "Unexpected response with request id ~d" request-id))
    req))

(defun find-connection-requests (conn)
  "Find all requests sent on connection."
  (loop for req in *corba-waiting-requests*
        when (eq conn (request-connection req))
        collect req
        else
        collect req into rest
        finally (setq *corba-waiting-requests* rest)))


;;;; Connection Layer

(defvar *default-trace-connection* nil)

(defstruct CONNECTION
  (read-buffer)
  (read-callback)
  (write-buffer)
  (write-callback)
  (io-descriptor)
  (trace *default-trace-connection*)
  (trace-send nil)
  (trace-recv nil))


(defvar *desc-conn* (make-hash-table))

(defun make-associated-connection (desc)
  (let* ((conn (make-connection :io-descriptor desc)))
    (setf (gethash desc *desc-conn*) conn)
    conn))

(defun connection-destroy (conn)
  (let ((desc (connection-io-descriptor conn)))
    (io-descriptor-destroy desc)
    ;; FIXME: or is that remhash??
    (setf (gethash desc *desc-conn*) nil)))

(defun create-connection (host port)
  (let* ((desc (io-create-descriptor))
         (conn (make-associated-connection desc)))
    (io-descriptor-connect desc host port)
    conn))

(defun connection-working-p (conn)
  (let ((desc (connection-io-descriptor conn)))
    (or (null desc)
        (not (eq :broken (io-descriptor-status desc))))))

(defun connection-init-read (conn continue-p n callback)
  (setf (connection-read-callback conn) callback)
  (let ((desc (connection-io-descriptor conn)))
    (if desc
        (let* ((buffer (if continue-p
                           (connection-read-buffer conn)
                           (get-work-buffer)))
               (octets (buffer-octets buffer))
               (start (fill-pointer octets)))
          (unless continue-p
            (setf (connection-read-buffer conn) buffer))
          (when (< (array-total-size octets) n)
            (adjust-array octets n))
          (setf (fill-pointer octets) n)
          (io-descriptor-set-read desc octets start n))

        ;; If no io-descriptor, assume complete
        (if continue-p
            (funcall callback conn)
            (setf (connection-read-buffer conn) nil)))))


(defun write-done (conn)
  (setf (connection-write-buffer conn) nil))

(defun connection-send-buffer (conn buffer)
  (loop while (connection-write-buffer conn)
        do (orb-wait))
  (setf (connection-write-buffer conn) buffer)
  (setf (connection-write-callback conn) #'write-done)
  (let ((desc (connection-io-descriptor conn))
        (octets (buffer-octets buffer)))
    (when desc
      (io-descriptor-set-write desc octets 0 (length octets)))))


;;; Connection events

(defun connection-error (conn)
  ;; Called when there is IO error
  ;; io-descriptor is (usually?) destroyed
  ;; FIXME: minor code?
  (let ((requests (find-connection-requests conn)))
    (loop for req in requests
          do (setf (any-value (op:return_value req))
                   (make-condition 'CORBA:COMM_FAILURE))
          (setf (request-status req) :error ))))

(defun connection-read-ready (conn)
  (when (and (connection-trace conn)
             (> (length (buffer-octets (connection-read-buffer conn))) *iiop-header-size*))
    (push (connection-read-buffer conn) (connection-trace-recv conn)))
  (funcall (connection-read-callback conn) conn))

(defun connection-write-ready (conn)
  (when (connection-trace conn)
    (push (connection-write-buffer conn) (connection-trace-send conn)))
  (funcall (connection-write-callback conn) conn))


;;;; IIOP - Sending request

(defun request-prepare (req object)
  (let* ((forward (or (object-forward object) object))
         (conn (get-object-connection forward)))
    (setf (request-connection req) conn)    
    (setf (request-status req) nil)
    (setf (request-req-id req) (incf *request-id-seq*))
    (values
     (setf (request-forward req) forward)
     (setf (request-buffer req) (get-work-buffer))
     conn)))

(defun locate (obj)
  (let ((req (make-instance 'request :target obj :operation 'locate)))
    (loop
      (multiple-value-bind (object buffer) (request-prepare req obj)
        (marshal-giop-header :locaterequest buffer)    ; LocateRequest
        (marshal-ulong (request-req-id req) buffer)
        (marshal-osequence (object-key object) buffer)
        (request-send-buffer req buffer nil))
      (request-wait-response req)
      (cond ((eql (request-status req) :object_forward)
             (setf (object-forward obj) (unmarshal-object (request-buffer req))))
            (t 
             (return (request-status req)))))))

(defun request-marshal-head (req no-response)
  (multiple-value-bind (object buffer)
                       (request-prepare req (request-target req))
    (marshal-giop-header :request buffer)
    (marshal-service-context *service-context* buffer)
    (marshal-ulong (request-req-id req) buffer)
    (marshal-octet (if no-response 0 1) buffer)
    (marshal-osequence (object-key object) buffer)
    (marshal-string (request-operation req) buffer)
    (marshal-osequence *principal* buffer)
    buffer))

(defun request-send-buffer (req buffer no-response)
  (marshal-giop-set-message-length buffer)
  (setf (request-buffer req) nil)
  (setf (request-status req) nil)
  (connection-send-buffer (request-connection req) buffer)
  (unless no-response
    (push req *corba-waiting-requests*)))



;;;; IIOP - Response handling

(defparameter +message-types+
  '#(:REQUEST :REPLY :CANCELREQUEST :LOCATEREQUEST :LOCATEREPLY
              :CLOSECONNECTION :MESSAGEERROR))

(defun make-iiop-version (major minor)
  (or (if (eql major 1)
        (case minor 
          (0 :iiop_1_0) (1 :iiop_1_1)))
      :iiop_unknown_version))
  

(defun unmarshal-giop-header (buffer)
  (unless (loop for c in '(#\G #\I #\O #\P)
		always (eql c (unmarshal-char buffer)))
    (error "Not a GIOP message: ~/net.cddr.clorb::stroid/"
           (buffer-octets buffer)))
  (let ((major (unmarshal-octet buffer))
        (minor (unmarshal-octet buffer))
        (byte-order (unmarshal-octet buffer))
        (msgtype (aref +message-types+ (unmarshal-octet buffer))))
    (setf (buffer-byte-order buffer) byte-order)
    (values msgtype (make-iiop-version major minor))))

(defun marshal-giop-header (type buffer)
  (with-out-buffer (buffer)
    #.(cons 'progn (loop for c across "GIOP" collect `(put-octet ,(char-code c))))
    (put-octet 1)				;Version 
    (put-octet 0)
    (put-octet 1)				;byte-order
    (put-octet (cond ((numberp type) type)
                     ((eq type 'request) 0)
                     ((eq type 'reply) 1)
                     (t 
                      (let ((n (position type +message-types+)))
                        (or n (error "Message type ~S" type))))))
    ;; Place for message length to be patched in later
    (incf pos 4)))


(defun get-response-0 (conn)
  (let* ((buffer (connection-read-buffer conn))
         (msgtype (unmarshal-giop-header buffer))
         (handler
          (ecase msgtype
            ((:reply) #'get-response-reply)
            ((:locatereply) #'get-response-locate-reply)
            ((:closeconnection)
             (mess 3 "Connection closed")
             ;; FIXME: should initiated cleaning up conn...
             ;; all wating requests get some system exception
             (connection-error conn)
             nil)
            ((:messageerror)
             (mess 6 "CORBA: Message error")
             nil)))
         (size (+ (unmarshal-ulong buffer) *iiop-header-size*)))
    (if handler
        (connection-init-read conn t size handler)
        ;; prehaps it is better to close it....
        (setup-outgoing-connection conn))))

(defun get-response-reply (conn)
  (let* ((buffer (connection-read-buffer conn))
         (service-context (unmarshal-service-context buffer))
         (request-id (let ((id (unmarshal-ulong buffer)))
                       ;;(break "id=~d" id)
                       id ))
         (req (find-waiting-request request-id))
         (status (unmarshal (load-time-value (SYMBOL-TYPECODE 'GIOP:REPLYSTATUSTYPE))
                            buffer)))
    (setup-outgoing-connection conn)
    (when req
      (setf (request-service-context req) service-context)
      (setf (request-status req) status)
      (setf (request-buffer req) buffer))))

(defun get-response-locate-reply (conn &aux (buffer (connection-read-buffer conn)))
  (setup-outgoing-connection conn)
  (let* ((request-id (unmarshal-ulong buffer))
         (status (unmarshal (symbol-typecode 'giop:locatestatustype) buffer))
         (req (find-waiting-request request-id)))
    (when req
      (setf (request-status req) status)
      (setf (request-buffer req) buffer)
      req)))

(defun request-wait-response (req)
  (loop
   while (not (request-status req))
   do (orb-wait)))



;;;; Event loop (orb-wait)

(defvar *new-connection-callback*
  (lambda (desc)
    (io-descriptor-destroy desc)))

(defvar *shortcut-in* (make-connection))

(defvar *shortcut-out* (make-connection))

(defun shortcut-transfer (from to)
  (when (and (connection-write-buffer from)
             (null (connection-read-buffer to)))
    (setf (connection-read-buffer to)
          (connection-write-buffer from))
    (connection-write-ready from)
    (connection-read-ready to)
    t))


(defun orb-wait ()
  (declare (optimize (debug 3)))
  ;; Check special shortcut connection
  (when (or (shortcut-transfer *shortcut-out* *shortcut-in*)
            (shortcut-transfer *shortcut-in* *shortcut-out*))
    (return-from orb-wait))

  (multiple-value-bind (event desc) (io-driver)
    (when event
      (mess 2 "io-event: ~S ~S" event (type-of desc)))
    (let ((conn (gethash desc *desc-conn*)))
      (case event
        (:read-ready
         ;;(io-descriptor-set-read desc nil 0 0)
         (connection-read-ready conn))
        (:write-ready
         (io-descriptor-set-write desc nil 0 0)
         (connection-write-ready conn))
        (:new
         (funcall *new-connection-callback* desc))
        (:connected
         ;; Not implemented yet..; for outgoing connections setup
         nil)
        (:error
         (mess 4 "Error: ~A" (io-descriptor-error desc))
         (io-descriptor-destroy desc)
         (connection-error conn))
        ((nil)
         (mess 1 "time out"))))))


;;;; IIOP - Manage outgoing connections

(defvar *iiop-connections* nil
  "All active client sockets.
Organized as two levels of a-lists:
  ( (host . ((port . socket) ...)) ...)
Where host is a string and port an integer.")


(defun setup-outgoing-connection (conn)
  (connection-init-read conn nil *iiop-header-size* #'get-response-0))


(defun get-connection-holder (host port)
  ;; a cons cell where cdr is for connection
  (let* ((host-list				; A host-ports pair
	  (assoc host *iiop-connections* :test #'equal))
	 (holder				; A port-socket pair
	  (assoc port (cdr host-list))))
    (unless holder
      (unless host-list
        (setq host-list (cons host nil))
	(push host-list *iiop-connections*))
      (setq holder (cons port nil))
      (push holder (cdr host-list)))
    holder))

(defun setup-shortcut-out (host port &optional (conn-out *shortcut-out*))
  (let ((holder (get-connection-holder host port)))
    (let ((conn (cdr holder)))
      (when conn (connection-destroy conn)))
    (setf (cdr holder) conn-out)
    (setup-outgoing-connection conn-out)))

(defun get-connection (host port)
  (let* ((holder (get-connection-holder host port))
         (conn   (cdr holder)))
    (unless (and conn (connection-working-p conn))
      (when conn (connection-destroy conn))
      (setq conn (create-connection host port))
      (setup-outgoing-connection conn)
      (setf (cdr holder) conn))
    conn))

(defun get-object-connection (proxy)
  ;; get the connection to use for a proxy object.
  (let ((conn (object-connection proxy)))
    (unless (and conn (connection-working-p conn))
      (setf (object-connection proxy) nil)
      (setq conn (connect-object proxy)))
    conn))


(defun connect-object (proxy)
  ;; select a profile and create a connection for that profile
  (dolist (profile (object-profiles proxy))
    (let* ((host (iiop-profile-host profile))
           (port (iiop-profile-port profile))
           (conn (get-connection host port)))
      (when (and conn
                 (connection-working-p conn))
        (setf (object-connection proxy) conn)
        (setf (selected-profile proxy) profile)
        (return conn)))))



;;;; Support for static stubs

(defun start-request (operation object &optional no-response)
  (let ((req (make-instance 'request :target object :operation operation)))
    (values req (request-marshal-head req no-response))))

(defun invoke-request (req)
  (request-send-buffer req (request-buffer req) nil)
  (request-wait-response req)
  (let* ((status (request-status req))
         (buffer (request-buffer req)))
    (case status
      (:location_forward
       (setf (object-forward (request-target req))
             (unmarshal-object buffer))
       (values status))
      (:system_exception
       (let ((condition (unmarshal-systemexception buffer)))
         (if (typep condition 'omg.org/corba:transient)
           (values status)
           (error condition))))
      (otherwise
       (values status buffer)))))
                 
(defun process-exception (buffer legal-exceptions)
  (let ((id (unmarshal-string buffer)))
    (loop for exc in legal-exceptions
          when (string= id (symbol-ifr-id exc))
          do (error (exception-read exc buffer)))
    (error 'corba:unknown :minor 0 :completed :completed_yes)))


;;;; CORBA:Request - deferred operations

(define-method send_oneway ((req request))
  (request-send req :no-response))

(define-method send_deferred ((req request))
  (request-send req))

(defun request-send (req &optional no-response)
  (let ((buffer (request-marshal-head req no-response)))
    (request-marshal-arguments req buffer)
    (request-send-buffer req buffer no-response)))

(defun request-marshal-arguments (req buffer)
  (loop for nv in (request-paramlist req)
        when (/= 0 (logand ARG_IN (op:arg_modes nv)))
        do (marshal-any-value (op:argument nv) buffer)))

(defun request-unmarshal-result (req buffer)
  (loop for nv in (request-paramlist req)
        when (/= 0 (logand ARG_OUT (op:arg_modes nv)))
        do (setf (any-value (op:argument nv))
                 (unmarshal (any-typecode (op:argument nv)) buffer))))

(defun request-unmarshal-userexception (req buffer)
  (let* ((id (unmarshal-string buffer))
         (tc (request-exception-typecode req id))
         (retval (op:return_value req) ))
    (if tc
      (setf (any-value retval) (unmarshal tc buffer)
            (any-typecode retval) tc)
      (setf (any-value retval) (make-condition 'corba:unknown
                                               :completed :completed_yes)
            (any-typecode retval) nil))))

(defun request-unmarshal-systemexception (req buffer)
  (let ((retval (op:return_value req)))
    (setf (any-value retval) (unmarshal-systemexception buffer)
          (any-typecode retval) nil)))

(define-method get_response ((req request))
  (loop 
    (request-wait-response req)
    (let ((buffer (request-buffer req)))
      (unless buffer (return))
      (let ((status (request-status req)))
        (case status
          (:location_forward
           (setf (object-forward (request-target req))
                 (unmarshal-object buffer))
           (request-send req))
          (otherwise
           (ecase status
             (:no_exception (request-unmarshal-result req buffer))
             (:user_exception (request-unmarshal-userexception req buffer))
             (:system_exception (request-unmarshal-systemexception req buffer)))
           (setf (request-buffer req) nil)
           (return))))))
    (values))

(define-method poll_response ((req request))
  ;; FIXME: possibly (orb-wait ...) if without timeout
  (not (null (request-status req))))

(defun request-exception-typecode (request id)
  (find id (request-exceptions request)
        :key #'op:id :test #'equal))
