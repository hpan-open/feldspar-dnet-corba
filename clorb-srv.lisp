;;;; clorb-srv.lisp --- CORBA server module
;; $Id: clorb-srv.lisp,v 1.28 2004/12/19 23:34:19 lenst Exp $	

(in-package :clorb)


;;;; Default POA (boot objects)

(defvar *default-poa* nil)

(defvar *boot-objects*
  (make-hash-table :test #'equal))

(defclass BOOT-OBJECT-MANAGER (portableserver:servantactivator)
  ())

(defun setup-default-poa (orb)
  (setq *default-poa* 
        (create-POA nil "_default_" (op:the_POAManager *root-poa*) 
                    '(:use_servant_manager :user_id)
                    orb
                    :poaid 0))
  (op:set_servant_manager *default-poa* (make-instance 'boot-object-manager)))

(define-method incarnate ((self boot-object-manager) oid adapter)
  (declare (ignore adapter))
  (let ((name (oid-to-string oid)))
    (let ((obj (gethash name *boot-objects*)))
      (cond (obj
             (signal (omg.org/portableserver:forwardrequest
                      :forward_reference obj)))
            (t
             (raise-system-exception 'corba:object_not_exist 0 :completed_no))))))



;;;; Server proper


(defun setup-server (&optional (orb (CORBA:ORB_init)))
  (multiple-value-bind (port host)
      (io-create-listener (orb-port orb))
    (setf (adaptor orb) t)
    (setf (orb-port orb) port)
    (unless (orb-host orb)
      (setf (orb-host orb) host))
    (setup-shortcut))
  (unless *root-POA*
    (setq *root-poa* (create-POA nil "root" nil nil orb)))
  (setup-default-poa orb))


(defun setup-incoming-connection (conn)
  (setf (connection-error-callback conn) #'comm-failure-handler)
  (connection-init-read conn nil +iiop-header-size+ #'poa-message-handler))


(defun setup-shortcut ()
  (let ((orb (CORBA:ORB_init)))
    (setq *io-loopback-p*
          (lambda (host port)
            (and (equal host (orb-host orb))
                 (= port (orb-port orb)))))))


(defun shortcut-off ()
  (setq *io-loopback-p* nil))


(defun poa-connection-handler (desc)
  (let ((conn (make-associated-connection *the-orb* desc)))
    (setup-incoming-connection conn)))


(defun poa-message-handler (conn)
  (let ((buffer (connection-read-buffer conn)))
    (multiple-value-bind (msgtype) (unmarshal-giop-header buffer)
      (let ((decode-fun
             (ecase msgtype
               ((:request)         #'poa-request-handler)
               ((:cancelrequest)   #'poa-cancelrequest-handler)
               ((:locaterequest)   #'poa-locaterequest-handler)
               ((:closeconnection) #'poa-closeconnection-handler)
               ((:messageerror)    #'poa-messageerror-handler))))
        (let ((size (unmarshal-ulong buffer)))
          (mess 1 "Message type ~A size ~A" msgtype size)
          (if (zerop size)
            (funcall decode-fun conn)
            (connection-init-read conn t (+ size +iiop-header-size+) decode-fun)))))))




;;;; Request Handling


(defun poa-request-handler (conn)
  (let ((buffer (connection-read-buffer conn)))
    (setup-incoming-connection conn)
    (let* ((orb *the-orb*)              ; FIXME: from connection maybe?
           (service-context (unmarshal-service-context buffer))
           (req-id (unmarshal-ulong buffer))
           (response (unmarshal-octet buffer))
           (object-key (unmarshal-osequence buffer))
           (operation (unmarshal-string buffer))
           (principal (unmarshal-osequence buffer))
           (request (create-server-request
                     orb :connection conn
                     :request-id req-id :operation operation
                     :service-context service-context :input buffer
                     :state :wait :response-flags response)))
      (connection-add-server-request conn request)
      (mess 3 "#~D op ~A on '~/clorb:stroid/' from '~/clorb:stroid/'"
            req-id operation object-key principal)
      (loop
        (handler-case
          (let ((exception (request-exception request)))
            (typecase exception
              (null
               (has-received-request-header orb request)
               (multiple-value-bind (reftype poa oid) (decode-object-key-poa object-key)
                 (cond
                  ((and reftype poa)
                   (setf (the-poa request) poa)
                   (poa-invoke poa oid operation buffer request))
                  (t
                   (raise-system-exception 'CORBA:OBJECT_NOT_EXIST 0 :completed_no)))))
              (PortableServer:ForwardRequest 
               (set-request-forward request (op:forward_reference exception)))
              (CORBA:UserException
               (raise-system-exception 'CORBA:UNKNOWN))
              (CORBA:SystemException
               (mess 2 "#~D Exception: ~A" req-id exception)
               (let ((buffer (get-request-response request :system_exception)))
                 (marshal-string (exception-id exception) buffer)
                 (marshal-ulong  (system-exception-minor exception) buffer)
                 (%jit-marshal (system-exception-completed exception) CORBA::TC_completion_status buffer))))
            (return))

          (CORBA:Exception (exception)
                           (set-request-exception request exception))
          (serious-condition (exc) 
                             (set-request-exception request 
                                                    (system-exception 'CORBA:UNKNOWN))
                             (warn "Error in request processing: ~A" exc))))
      (send-response request))))

(defun set-request-forward (req obj)
  (mess 3 "forwarding to ~A" obj)
  (let ((buffer (get-location-forward-response req)))
    (marshal-object obj buffer)
    buffer))

(defun send-response (request)
  (let ((conn (request-connection request)))
    (when (response-expected request)
      (let ((buffer (reply-buffer request))
            (req-id (request-id request)))
        (mess 3 "#~D Sending response (~S)" req-id (reply-status request) )
        (marshal-giop-set-message-length buffer)
        (connection-send-buffer conn buffer)))
    (setf (request-state request) :finished)
    (connection-remove-server-request conn request)))


(defun poa-cancelrequest-handler (conn)
  (let ((buffer (connection-read-buffer conn)))
    (setup-incoming-connection conn)
    (let* ((req-id (unmarshal-ulong buffer)))
      (mess 3 "#~D cancel" req-id)
      (loop for req in (connection-server-requests conn)
            when (eql req-id (request-id req))
            do (return 
                (progn (setf (response-flags req) 0)
                       (when (eql (request-state req) :wait)
                         (setf (request-state req) :canceled)) ))))))



(defun poa-locaterequest-handler (conn)
  (let ((buffer (connection-read-buffer conn)))
    (setup-incoming-connection conn)
    (let* ((orb (the-orb conn))
           (req-id (unmarshal-ulong buffer))
           (object-key (unmarshal-osequence buffer))
           (operation "_locate")
           (request 
            (create-server-request
             orb :operation operation :request-id req-id
             :connection conn))
           (status :UNKNOWN_OBJECT))
      (setq buffer nil)
      ;; FIXME: what if decode-object-key-poa raises an exception?
      (multiple-value-bind (reftype poa oid) (decode-object-key-poa object-key)
        (when (and reftype poa)
          (handler-case
            (setq buffer (poa-invoke poa oid operation buffer request)
                  status :object_here)
            (SystemException () nil))))
      ;; FIXME: forwarding ??
      (send-locate-reply conn req-id status))))


(defun send-reply (conn request-id status service-context result-marshal result-data)
  (let* ((orb (the-orb conn))
         (buffer (get-work-buffer orb)))
    (marshal-giop-header :reply buffer)
    (marshal-service-context service-context buffer) 
    (marshal-ulong request-id buffer)
    (%jit-marshal status (symbol-typecode 'GIOP:REPLYSTATUSTYPE) buffer)
    (funcall result-marshal result-data buffer)
    (marshal-giop-set-message-length buffer)
    (connection-send-buffer conn buffer)))

(defun send-locate-reply (conn req-id status &optional forward-marshal forward-object)
  (let* ((orb (the-orb conn))
         (buffer (get-work-buffer orb)))
    (marshal-giop-header :locatereply buffer)
    (marshal-ulong req-id buffer)
    (marshal-ulong (ecase status
                     (:unknown_object 0)
                     (:object_here 1)
                     ((:object_forward :location_forward) 2))
                   buffer)
    (when forward-marshal
      (funcall forward-marshal forward-object buffer))
    (marshal-giop-set-message-length buffer)
    (connection-send-buffer conn buffer)))



(defun poa-closeconnection-handler (conn)
  (declare (ignore conn))
  ;;FIXME:
  ;; for giop 1.0 this is not expected on the server side
  (error "NYI"))

(defun poa-messageerror-handler (conn)
  (declare (ignore conn))
  ;;FIXME:
  (error "NYI"))


(defun root-poa (&optional (orb (CORBA:ORB_init)))
  (unless (adaptor orb)
    (setup-server orb))
  *root-POA*)

(defun initialize-poa (orb)
  (set-initial-reference orb "RootPOA" #'root-POA)
  (set-initial-reference orb "POACurrent" nil
                         (make-instance 'PortableServer::Current)))


(eval-when (:load-toplevel :execute)
  (setq *new-connection-callback* 'poa-connection-handler)
  (pushnew 'initialize-poa *orb-initializers* ))

(defun main-loop (&optional (orb (CORBA:ORB_init)))
  (root-POA)
  (op:run orb))

(defun recover (&optional (orb (CORBA:ORB_init)))
  (io-reset :listener nil)
  (main-loop orb))


;;; clorb-srv.lisp ends here
