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
  :members (("major" OMG.ORG/CORBA:TC_OCTET)
            ("minor" OMG.ORG/CORBA:TC_OCTET)))

(DEFINE-STRUCT GIOP:MESSAGEHEADER_1_0
  :id "IDL:GIOP/MessageHeader_1_0:1.0"
  :name "MessageHeader_1_0"
  :members (("magic" (create-array-tc 4 OMG.ORG/CORBA:TC_CHAR))
            ("GIOP_version" (SYMBOL-TYPECODE 'GIOP:VERSION))
            ("byte_order" OMG.ORG/CORBA:TC_BOOLEAN)
            ("message_type" OMG.ORG/CORBA:TC_OCTET)
            ("message_size" OMG.ORG/CORBA:TC_ULONG)))

(DEFINE-STRUCT GIOP:MESSAGEHEADER_1_1
 :ID "IDL:omg.org/GIOP/MessageHeader_1_1:1.0"
 :NAME "MessageHeader_1_1"
 :MEMBERS (("magic" (CREATE-ARRAY-TC 4 OMG.ORG/CORBA:TC_CHAR))
           ("GIOP_version" (SYMBOL-TYPECODE 'GIOP:VERSION))
           ("flags" OMG.ORG/CORBA:TC_OCTET)
           ("message_type" OMG.ORG/CORBA:TC_OCTET)
           ("message_size" OMG.ORG/CORBA:TC_ULONG)))

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
  (marshal-sequence ctx 
                    (lambda (service-context buffer)
                      ;;(struct-write service-context 'IOP:SERVICECONTEXT buffer)
                      (marshal-ulong (op:context_id service-context) buffer)
                      (marshal-osequence (op:context_data service-context) buffer))
                    buffer))

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

(defclass Connection ()
  ((the-orb     :initarg :orb         :accessor the-orb)
   (read-buffer :initarg :read-buffer :accessor connection-read-buffer)
   (read-callback :initarg :read-callback :accessor connection-read-callback)
   (write-buffer :initarg :write-buffer :accessor connection-write-buffer
                 :initform nil)
   (write-callback :initarg :write-callback :accessor connection-write-callback)
   (io-descriptor :initarg :io-descriptor :initform nil :accessor connection-io-descriptor)
   (trace :initarg :trace :initform *default-trace-connection* :accessor connection-trace)
   (trace-send :initform nil :accessor connection-trace-send)
   (trace-recv :initform nil :accessor connection-trace-recv)))


(defvar *desc-conn* (make-hash-table))

(defun make-associated-connection (orb desc)
  (let* ((conn (make-instance 'Connection :orb orb :io-descriptor desc)))
    (setf (gethash desc *desc-conn*) conn)
    conn))

(defun connection-destroy (conn)
  (let ((desc (connection-io-descriptor conn)))
    (io-descriptor-destroy desc)
    ;; FIXME: or is that remhash??
    (setf (gethash desc *desc-conn*) nil)))

(defun create-connection (orb host port)
  (let ((desc (io-create-descriptor)))
    (handler-case
      (progn (io-descriptor-connect desc host port)
             (make-associated-connection orb desc))
      (error (err)
             (mess 4 "(connect ~S ~S): ~A" host port err)
             (setf (io-descriptor-error desc) err)
             (setf (io-descriptor-status desc) :broken)
             (io-descriptor-destroy desc)
             nil))))


(defun connection-working-p (conn)
  (io-descriptor-working-p (connection-io-descriptor conn)))


(defun connection-init-read (conn continue-p n callback)
  (setf (connection-read-callback conn) callback)
  (let ((desc (connection-io-descriptor conn)))
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
      (io-descriptor-set-read desc octets start n))))


(defun write-done (conn)
  (setf (connection-write-buffer conn) nil))

(defun connection-send-buffer (conn buffer)
  (when (connection-write-buffer conn)
    (orb-wait (lambda (conn) (not (connection-write-buffer conn))) conn))
  (setf (connection-write-buffer conn) buffer)
  (setf (connection-write-callback conn) #'write-done)
  (let ((desc (connection-io-descriptor conn))
        (octets (buffer-octets buffer)))
    (io-descriptor-set-write desc octets 0 (length octets))))


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



;;;; IIOP - Profiles

(defmethod decode-ior-profile ((tag (eql 0)) encaps)
  (unmarshal-encapsulation encaps #'unmarshal-iiop-profile-body))

(defun unmarshal-iiop-profile-body (buffer)
  (make-iiop-profile
   :version (cons (unmarshal-octet buffer) 
                  (unmarshal-octet buffer))
   :host (unmarshal-string buffer)
   :port (unmarshal-ushort buffer)
   :key (unmarshal-osequence buffer)))

(defmethod raw-profiles ((objref CORBA:Proxy))
  (or (object-raw-profiles objref)
      (setf (object-raw-profiles objref)
            (map 'list
                 (lambda (p)
                   (cons iop:tag_internet_iop
                         (marshal-make-encapsulation
                          (lambda (buffer)
                            (let ((version (iiop-profile-version p)))
                              (marshal-octet (car version) buffer)
                              (marshal-octet (cdr version) buffer))
                            (marshal-string (iiop-profile-host p) buffer)
                            (marshal-ushort (iiop-profile-port p) buffer)
                            (marshal-osequence (iiop-profile-key p) buffer)))))
                 (object-profiles objref)))))



;;;; IIOP - Sending request

(defun request-prepare (req object)
  (let* ((conn (get-object-connection object)))
    (unless conn
      (raise-system-exception 'omg.org/corba:comm_failure))
    (setf (request-connection req) conn)    
    (setf (request-status req) nil)
    (setf (request-req-id req) (incf *request-id-seq*))
    (values
     (or (object-forward object) object)
     (setf (request-buffer req) (get-work-buffer (the-orb req)))
     conn)))

(defun locate (obj)
  (let ((req (make-instance 'client-request :target obj :operation 'locate)))
    (loop
      (multiple-value-bind (object buffer) (request-prepare req obj)
        (marshal-giop-header :locaterequest buffer)    ; LocateRequest
        (marshal-ulong (request-req-id req) buffer)
        (marshal-osequence (object-key object) buffer)
        (send-request req))
      (request-wait-response req)
      (cond ((eql (request-status req) :object_forward)
             (setf (object-forward obj) (unmarshal-object (request-buffer req))))
            (t 
             (return (request-status req)))))))

(defun request-marshal-head (req)
  (multiple-value-bind (object buffer)
                       (request-prepare req (request-target req))
    (will-send-request (the-orb req) req)
    (marshal-giop-header :request buffer)
    (marshal-service-context (service-context-list req) buffer)
    (marshal-ulong (request-req-id req) buffer)
    (marshal-octet (if (response-expected req) 1 0) buffer)
    (marshal-osequence (object-key object) buffer)
    (marshal-string (request-operation req) buffer)
    (marshal-osequence *principal* buffer)
    buffer))

(defun send-request (req)
  (let ((buffer (request-buffer req)))
    (marshal-giop-set-message-length buffer)
    (setf (request-buffer req) nil)
    (setf (request-status req) nil)
    (connection-send-buffer (request-connection req) buffer))
  (when (response-expected req)
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
  (with-in-buffer (buffer)
    (unless (loop for c in '#.(mapcar #'char-code '(#\G #\I #\O #\P))
		  always (eql c (get-octet)))
      (error "Not a GIOP message: ~/net.cddr.clorb::stroid/" octets))
    (let ((major (get-octet))
          (minor (get-octet))
          (byte-order (get-octet))
          (msgtype (aref +message-types+ (get-octet))))
      (setf (buffer-byte-order buffer) byte-order)
      (values msgtype (make-iiop-version major minor)))))

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
      (setf (reply-service-context req) service-context)
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
  (orb-wait #'request-status req))



;;;; Event loop (orb-wait)

(defvar *new-connection-callback*
  (lambda (desc)
    (io-descriptor-destroy desc)))


(defun orb-wait (&optional wait-func &rest wait-args)
  (if *running-orb*
    (if wait-func
      (loop until (apply wait-func wait-args) do (orb-work))
      (orb-work))
    (if wait-func
      (apply #'process-wait "orb-wait" wait-func wait-args)
      (sleep 0.1))))


(defun orb-work ()
  (declare (optimize (debug 3)))
  (multiple-value-bind (event desc) (io-driver)
    (when event
      (mess 2 "io-event: ~S ~A" event (io-descriptor-stream desc)))
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


(defun get-connection (orb host port)
  (let* ((holder (get-connection-holder host port))
         (conn   (cdr holder)))
    (unless (and conn (connection-working-p conn))
      (when conn (connection-destroy conn))
      (setq conn (create-connection orb host port))
      (when conn (setup-outgoing-connection conn))
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
  (or
   (let ((forward (object-forward proxy)))
     (if forward
       (cond ((setf (object-connection proxy) (connect-object forward))
              (setf (object-forward-reset proxy) nil)
              (object-connection proxy))
             ((object-forward-reset proxy)
              (setf (object-forward proxy) nil)
              (warn "Object forwarding fail")
              (return-from connect-object nil))
             (t
              (setf (object-forward-reset proxy) t)
              (setf (object-forward proxy) nil)))))
   (dolist (profile (object-profiles proxy))
     (let* ((host (iiop-profile-host profile))
            (port (iiop-profile-port profile))
            (conn (get-connection (the-orb proxy) host port)))
       (when (and conn
                  (connection-working-p conn))
         (setf (object-connection proxy) conn)
         (setf (selected-profile proxy) profile)
         (return conn))))))



;;;; Support for static stubs

(defun start-request (operation object &optional no-response)
  (let ((req (create-client-request 
              (the-orb object)
              :target object :operation operation
              :response-expected (not no-response))))
    (values req (request-marshal-head req))))

(defun invoke-request (req)
  (send-request req)
  (cond ((response-expected req)
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
                (setf (request-exception req) condition)
                (has-received-exception (the-orb req) req)
                (setq condition (request-exception req))
                (if (typep condition 'omg.org/corba:transient)
                  (values status)
                  (error condition))))
             (otherwise
              (has-received-reply (the-orb req) req)
              (values status buffer)))))
        (t
         :no_exception)))
                 
(defun process-exception (buffer legal-exceptions)
  (let ((id (unmarshal-string buffer)))
    (loop for exc in legal-exceptions
          when (string= id (symbol-ifr-id exc))
          do (error (exception-read exc buffer)))
    (raise-system-exception 'corba:unknown 1 :completed_yes)))


;;;; CORBA:Request - deferred operations

(define-method send_oneway ((req client-request))
  (setf (response-expected req) nil)  
  (request-marshal req)
  (send-request req))

(define-method send_deferred ((req client-request))
  (request-marshal req)
  (send-request req))


(defun request-marshal (req)
  (let ((buffer (request-marshal-head req)))
    (loop for nv in (request-paramlist req)
          when (/= 0 (logand ARG_IN (op:arg_modes nv)))
          do (marshal-any-value (op:argument nv) buffer))))

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
      (setf (any-value retval) (system-exception 'corba:unknown 1 :completed_yes)
            (any-typecode retval) nil))))

(defun request-unmarshal-systemexception (req buffer)
  (let ((retval (op:return_value req)))
    (setf (any-value retval) (unmarshal-systemexception buffer)
          (any-typecode retval) nil)))

(define-method get_response ((req client-request))
  (loop 
    (request-wait-response req)
    (let ((buffer (request-buffer req)))
      (unless buffer (return))
      (let ((status (request-status req)))
        (case status
          (:location_forward
           (setf (object-forward (request-target req))
                 (unmarshal-object buffer))
           (request-marshal req)
           (send-request req))
          (otherwise
           (ecase status
             (:no_exception 
              (request-unmarshal-result req buffer)
              (has-received-reply (the-orb req) req))
             (:user_exception 
              (request-unmarshal-userexception req buffer)
              (has-received-exception (the-orb req) req))
             (:system_exception
              (request-unmarshal-systemexception req buffer)
              (has-received-exception (the-orb req) req)))
           (setf (request-buffer req) nil)
           (return))))))
    (values))

(define-method poll_response ((req client-request))
  ;; FIXME: possibly (orb-wait ...) if without timeout
  (not (null (request-status req))))

(defun request-exception-typecode (request id)
  (find id (request-exceptions request)
        :key #'op:id :test #'equal))
