;;; clorb-iiop.lisp --- IIOP implementation
;; $Id: clorb-iiop.lisp,v 1.38 2005/02/07 22:49:06 lenst Exp $


(in-package :clorb)

(defconstant +iiop-header-size+ 12)


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
 :members (("request_id" OMG.ORG/CORBA:TC_ULONG)
           ("object_key" (create-sequence-tc 0 OMG.ORG/CORBA:TC_OCTET))))

(define-enum GIOP:LocateStatusType
  :id "IDL:GIOP/LocateStatusType:1.0"
  :name "LocateStatusType" 
  :members ("UNKNOWN_OBJECT" "OBJECT_HERE" "OBJECT_FORWARD"))

(DEFINE-STRUCT GIOP:LOCATEREPLYHEADER
 :id "IDL:GIOP/LocateReplyHeader:1.0"
 :name "LocateReplyHeader"
 :members (("request_id" OMG.ORG/CORBA:TC_ULONG REQUEST_ID) 
           ("locate_status" (SYMBOL-TYPECODE 'GIOP:LOCATESTATUSTYPE))))

(DEFINE-STRUCT GIOP:CANCELREQUESTHEADER
 :id "IDL:GIOP/CancelRequestHeader:1.0"
 :name "CancelRequestHeader"
 :members (("request_id" OMG.ORG/CORBA:TC_ULONG)))

(DEFINE-STRUCT GIOP:REQUESTHEADER_1_0
  :id "IDL:GIOP/RequestHeader_1_0:1.0"
  :name "RequestHeader_1_0"
  :members (("service_context" (SYMBOL-TYPECODE 'IOP:SERVICECONTEXTLIST))
            ("request_id" OMG.ORG/CORBA:TC_ULONG)
            ("response_expected" OMG.ORG/CORBA:TC_BOOLEAN)
            ("object_key" (create-sequence-tc NIL OMG.ORG/CORBA:TC_OCTET))
            ("operation" OMG.ORG/CORBA:TC_STRING)
            ("requesting_principal" (SYMBOL-TYPECODE 'GIOP:PRINCIPAL))))

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
                      (marshal-ulong (op:context_id service-context) buffer)
                      (marshal-osequence (op:context_data service-context) buffer))
                    buffer))

(defun unmarshal-service-context (buffer)
  (unmarshal-sequence-m (buffer) 
    (IOP:ServiceContext :context_id (unmarshal-ulong buffer)
	                :context_data (unmarshal-osequence buffer))))


;;;; Version

(defun make-iiop-version (major minor)
  (or (if (eql major 1)
        (case minor 
          (0 '(1 . 0))  (1 '(1 . 1)) (2 '(1 . 2))))
      :iiop_unknown_version))

(defun iiop-version-major (version) (car version))
(defun iiop-version-minor (version) (cdr version))


(defun make-giop-version (major minor)
  (or (if (eql major 1)
        (case minor 
          (0 :giop_1_0) (1 :giop_1_1) (2 :giop_1_2)))
      :giop_unknown_version))
  


;;;; IIOP - Profiles


(defstruct IIOP-PROFILE
  (version (make-iiop-version 1 0))
  (host    nil)
  (port    0    :type fixnum)
  (key     nil)
  (components nil))


(defmethod profile-short-desc ((profile IIOP-PROFILE) stream)
  (format stream "~A:~A" 
          (iiop-profile-host profile)
          (iiop-profile-port profile)))

(defmethod profile-equal ((profile1 iiop-profile) (profile2 iiop-profile))
  (and (equal (iiop-profile-host profile1) (iiop-profile-host profile2))
       (equal (iiop-profile-port profile1) (iiop-profile-port profile2))
       (equalp (iiop-profile-key profile1) (iiop-profile-key profile2))))

(defmethod profile-hash ((profile iiop-profile))
  (sxhash (list* (iiop-profile-host profile)
                 (iiop-profile-port profile)
                 (coerce (iiop-profile-key profile) 'list))))

(defmethod decode-ior-profile ((tag (eql 0)) encaps)
  (unmarshal-encapsulation encaps #'unmarshal-iiop-profile-body))


(defun unmarshal-iiop-componets (buffer)
  (let ((len (unmarshal-ulong buffer)))
    (loop repeat len collect 
          (let ((tag (unmarshal-ulong buffer)))
            (cond ((= tag IOP:TAG_ORB_TYPE)
                   (cons tag (with-encapsulation buffer (unmarshal-ulong buffer))))
                  (t 
                   (cons tag (unmarshal-osequence buffer))))))))


(defun unmarshal-iiop-profile-body (buffer)
  (let ((major (unmarshal-octet buffer))
        (minor (unmarshal-octet buffer)))
    (make-iiop-profile
     :version (make-iiop-version major minor)
     :host (unmarshal-string buffer)
     :port (unmarshal-ushort buffer)
     :key (unmarshal-osequence buffer)
     :components (if (> minor 0)
                   (unmarshal-iiop-componets buffer)))))


(defmethod raw-profiles ((objref CORBA:Proxy))
  (or (object-raw-profiles objref)
      (setf (object-raw-profiles objref)
            (map 'list
                 (lambda (p)
                   (cons iop:tag_internet_iop
                         (marshal-make-encapsulation
                          (lambda (buffer)
                            (let ((version (iiop-profile-version p)))
                              (marshal-octet (iiop-version-major version) buffer)
                              (marshal-octet (iiop-version-minor version) buffer))
                            (marshal-string (iiop-profile-host p) buffer)
                            (marshal-ushort (iiop-profile-port p) buffer)
                            (marshal-osequence (iiop-profile-key p) buffer))
                          (the-orb objref))))
                 (object-profiles objref)))))




;;;; IIOP - Connection request preparation


(defun connection-start-locate-request (conn req-id profile)
  (let ((buffer (get-work-buffer (the-orb conn))))
    (marshal-giop-header :locaterequest buffer)
    (marshal-ulong req-id buffer)
    (marshal-osequence (iiop-profile-key profile) buffer)
    buffer))


(defun connection-start-request (conn req-id service-context response-expected
                                          effective-profile operation)
  (let ((buffer (get-work-buffer (the-orb conn))))
    (marshal-giop-header :request buffer)
    (marshal-service-context service-context buffer)
    (marshal-ulong req-id buffer)
    (marshal-octet (if response-expected 1 0) buffer)
    (marshal-osequence 
     (iiop-profile-key effective-profile)
     buffer)
    (marshal-string operation buffer)
    (marshal-osequence *principal* buffer)
    buffer))


(defun connection-send-request (conn buffer req)
  (marshal-giop-set-message-length buffer)
  (connection-send-buffer conn buffer)
  (when req
    (connection-add-client-request conn req)))

(defun connection-send-reply (conn buffer req)
  (marshal-giop-set-message-length buffer)
  (connection-send-buffer conn buffer)
  (when req
    (connection-remove-server-request conn req)))


;;;; IIOP - Response handling

(define-symbol-macro message-types
  '#(:REQUEST :REPLY :CANCELREQUEST :LOCATEREQUEST :LOCATEREPLY
              :CLOSECONNECTION :MESSAGEERROR))


(defun unmarshal-giop-header (buffer)
  (with-in-buffer (buffer)
    (unless (loop for c in '#.(mapcar #'char-code '(#\G #\I #\O #\P))
		  always (eql c (get-octet)))
      (error "Not a GIOP message: ~/net.cddr.clorb::stroid/" octets))
    (let ((major (get-octet))
          (minor (get-octet))
          (byte-order (get-octet))
          (msgtype (aref message-types (get-octet))))
      (setf (buffer-byte-order buffer) byte-order)
      (values msgtype (make-giop-version major minor)))))

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
                      (let ((n (position type message-types)))
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
         (size (+ (unmarshal-ulong buffer) +iiop-header-size+)))
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
         (req (find-waiting-client-request conn request-id))
         (status (%jit-unmarshal (symbol-typecode 'GIOP:ReplyStatusType) buffer)))
    (setup-outgoing-connection conn)
    (when req
      (request-reply req status buffer service-context))))


(defun get-response-locate-reply (conn &aux (buffer (connection-read-buffer conn)))
  (setup-outgoing-connection conn)
  (let* ((request-id (unmarshal-ulong buffer))
         (status (%jit-unmarshal (symbol-typecode 'giop:locatestatustype) buffer))
         (req (find-waiting-client-request conn request-id)))
    (when req
      (request-locate-reply req status buffer))))



;;;; IIOP - Manage outgoing connections

(defvar *iiop-connections* nil
  "All active client sockets.
Organized as two levels of a-lists:
  ( (host . ((port . socket) ...)) ...)
Where host is a string and port an integer.")


(defun setup-outgoing-connection (conn)
  (connection-init-read conn nil +iiop-header-size+ #'get-response-0))


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

(defmethod profile-connection ((profile iiop-profile) orb)
  (let ((host (iiop-profile-host profile))
        (port (iiop-profile-port profile)))
    (get-connection orb host port)))



;;;; Locate

(defun locate (obj)
  (let ((req (create-client-request (the-orb obj)
                                    :target obj :operation 'locate)))
    (loop
      (request-start-request 
       req
       (lambda (conn req-id)
         (setf (request-id req) req-id)
         (let ((buffer (connection-start-locate-request conn req-id
                                                        (request-effective-profile req))))
           (connection-send-request conn buffer req))))
      (request-wait-response req)
      (cond ((eql (request-status req) :object_forward)
             (setf (object-forward obj) (unmarshal-object (request-buffer req))))
            (t 
             (return (request-status req)))))))


;;;; Attribute accessors

(defun get-attribute (obj getter result-tc)
  (static-call (getter obj)
               :output ((buffer))
               :input ((buffer) (unmarshal result-tc buffer))))

(defun set-attribute (obj setter result-tc newval)
  (static-call (setter obj)
               :output ((buffer) (marshal newval result-tc buffer))
               :input ((buffer))))

