;;;; clorb-srv.lisp --- CORBA server module
;; $Id: clorb-srv.lisp,v 1.30 2005/02/07 22:49:06 lenst Exp $	

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
      ;; FIXME: If the version is larger than we handel, send a error message
      ;; and close connection.
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
      (has-received-request-header orb request)
      (dispatch-request orb request object-key))))



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
             orb :operation operation :request-id req-id :kind :locate
             :response-flags 1
             :connection conn)))
      (dispatch-request orb request object-key))))


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
