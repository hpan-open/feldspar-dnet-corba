;;;; clorb-srv.lisp --- CORBA server module
;; $Id: clorb-srv.lisp,v 1.16 2003/11/15 17:23:39 lenst Exp $	

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
      (when obj
        (signal (omg.org/portableserver:forwardrequest
                 :forward_reference obj)))
      nil)))


;;;; Server proper

(defun setup-server (&optional (orb (ORB_init)))
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
  (connection-init-read conn nil *iiop-header-size* #'poa-message-handler))

(defun setup-shortcut-in (&optional (conn-in *shortcut-in*))
  (setup-incoming-connection conn-in))

(defun setup-shortcut ()
  (let ((orb (CORBA:ORB_init)))
    (setup-shortcut-out (orb-host orb) (orb-port orb))
    (setup-shortcut-in)))

(defun shortcut-off ()
  (let* ((orb (CORBA:ORB_init))
         (holder (get-connection-holder (orb-host orb) (orb-port orb))))
    (setf (cdr holder) nil)))

(defun poa-connection-handler (desc)
  (let ((conn (make-associated-connection desc)))
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
            (connection-init-read conn t (+ size *iiop-header-size*) decode-fun)))))))

(defun poa-request-handler (conn)
  (let ((buffer (connection-read-buffer conn)))
    (setup-incoming-connection conn)
    (let* ((*service-context* (unmarshal-service-context buffer))
           (req-id (unmarshal-ulong buffer))
           (response (unmarshal-octet buffer))
           (object-key (unmarshal-osequence buffer))
           (operation (unmarshal-string buffer))
           (principal (unmarshal-osequence buffer))
           (handler
            (lambda (status)
               (let ((buffer (get-work-buffer)))
                (marshal-giop-header :reply buffer)
                (marshal-ulong 0 buffer) ; service-context
                (marshal-ulong req-id buffer)
                (marshal status (symbol-typecode 'GIOP:REPLYSTATUSTYPE) buffer)
                buffer))))
      (mess 3 "#~D op ~A on '~/clorb:stroid/' from '~/clorb:stroid/'"
            req-id operation object-key principal)
      (handler-case
        (multiple-value-bind (reftype poa oid)
                             (decode-object-key-poa object-key)
          (cond ((and reftype poa)
                 (mess 2 "Using POA ~A oid '~/clorb:stroid/'" (op:the_name poa) oid)
                 ;; Check if POA is active, holding, discarding...
                 (setq buffer (poa-invoke poa oid operation buffer handler)))
                (t
                 (error 'CORBA:OBJECT_NOT_EXIST :minor 0 :completed :completed_no))))
        (systemexception
         (exception)
         (mess 2 "#~D Exception from servant: ~A" req-id exception)
         (setq buffer (funcall handler :system_exception))
         (marshal-string (exception-id exception) buffer)
         (marshal-ulong  (system-exception-minor exception) buffer)
         (marshal (system-exception-completed exception) OMG.ORG/CORBA::TC_completion_status buffer)))

      ;; Send the response
      (unless (zerop response)
        (mess 3 "#~D Sending response" req-id )
        (marshal-giop-set-message-length buffer)
        (connection-send-buffer conn buffer)))))


(defun poa-cancelrequest-handler (conn)
  (let ((buffer (connection-read-buffer conn)))
    (setup-incoming-connection conn)
    (let* ((req-id (unmarshal-ulong buffer)))
      (mess 3 "#~D cancel" req-id))))


(defun poa-locaterequest-handler (conn)
  (let ((buffer (connection-read-buffer conn)))
    (setup-incoming-connection conn)
    (let* ((req-id (unmarshal-ulong buffer))
           (object-key (unmarshal-osequence buffer))
           (handler (lambda (type)
                      (let ((buffer (get-work-buffer)))
                        (marshal-giop-header :locatereply buffer)
                        (marshal-ulong req-id buffer)
                        (marshal-ulong (ecase type
                                         (:unknown_object 0)
                                         (:object_here 1)
                                         ((:object_forward :location_forward) 2))
                                       buffer)
                        buffer)))
           (status :UNKNOWN_OBJECT))
      (setq buffer nil)
      ;; FIXME: what if decode-object-key-poa raises an exception?
      (multiple-value-bind (reftype poa oid) (decode-object-key-poa object-key)
        (when (and reftype poa)
          (handler-case
            (setq buffer (poa-invoke poa oid "_locate" buffer handler)
                  status :object_here)
            (CORBA:TRANSIENT () (setq status :object_here))
            (SystemException () nil))))
      (unless buffer
        (setq buffer (funcall handler status))))
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


(defun root-poa (&optional (orb (orb_init)))
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

(defun main-loop (&optional (orb (ORB_init)))
  (root-POA)
  (op:run orb))

(defun recover (&optional (orb (ORB_init)))
  (io-reset :listener nil)
  (main-loop orb))


;;; clorb-srv.lisp ends here
