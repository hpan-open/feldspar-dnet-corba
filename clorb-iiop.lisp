;;; clorb-iiop.lisp --- IIOP implementation
;; $Id: clorb-iiop.lisp,v 1.2 2001/12/16 22:47:23 lenst Exp $

(in-package :clorb)

(defvar *request-id-seq* 0)
(defvar *corba-waiting-requests* nil)

(defparameter *iiop-header-size* 12)


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

(defstruct connection
  (read-buffer)
  (read-callback)
  (write-buffer)
  (write-callback)
  (io-descriptor))


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
          (setf (request-reply req) t))))

(defun connection-read-ready (conn)
  (funcall (connection-read-callback conn) conn))

(defun connection-write-ready (conn)
  (funcall (connection-write-callback conn) conn))


;;;; IIOP - Sending request

(defun locate (obj)
  (let ((req (make-instance 'request :target obj :operation 'locate)))
    (add-arg req "" ARG_IN)             ; room for result/exception
    (request-send req)
    (request-wait-response req)
    (request-reply req)))

(defun request-send (req &optional no-response)
  (let ((object (request-target req)))
    (setq object (or (object-forward object)
		     object))
    (let ((conn (get-connection (object-host object)
                                (object-port object)))
          (buffer (get-work-buffer)))
      (setf (request-connection req) conn)
      (setf (request-req-id req) (incf *request-id-seq*))
      (setf (request-reply req) nil)
      (request-marshal req object no-response buffer)
      (connection-send-buffer conn buffer)
      (unless no-response
        (push req *corba-waiting-requests*)))))

(defun request-marshal (req object no-response buffer)
  (let ((operation (request-operation req)))
    (cond ((eq operation 'locate)
           (marshal-giop-header 3 buffer) ;LocateRequest
           (marshal-ulong (request-req-id req) buffer)
           (marshal-osequence (object-key object) buffer))
          (t
           (marshal-giop-header 'request buffer)
           (marshal-service-context *service-context* buffer)
           (marshal-ulong (request-req-id req) buffer)
           ;; respons expected-type
           (marshal-octet (if no-response 0 1) buffer)
           (marshal-osequence (object-key object) buffer)
           (marshal-string operation buffer)
           (marshal-osequence *principal* buffer)
           (loop for nv in (request-paramlist req)
               when (/= 0 (logand ARG_IN (op:arg_modes nv)))
                    do (marshal-any-value (op:argument nv) buffer))))
    (marshal-giop-set-message-length buffer)))


;;;; IIOP - Response handling

(defun get-response-0 (conn)
  (let* ((buffer (connection-read-buffer conn))
         (msgtype (unmarshal-giop-header buffer))
         (handler
          (ecase msgtype
            ((1) #'get-response-reply)
            ((4) #'get-response-locate-reply)
            ((5)                        ;Close Connection
             (mess 3 "Connection closed")
             ;; FIXME: should initiated cleaning up conn...
             ;; all wating requests get some system exception
             (connection-error conn)
             nil)
            ((6)
             (mess 6 "CORBA: Message error")
             nil)))
         (size (+ (unmarshal-ulong buffer) *iiop-header-size*)))
    (if handler
        (connection-init-read conn t size handler)
        ;; prehaps it is better to close it....
        (setup-outgoing-connection conn))))

(defun get-response-reply (conn &aux (buffer (connection-read-buffer conn)))
  (setup-outgoing-connection conn)
  (let* ((service-context (unmarshal-service-context buffer))
         (request-id (unmarshal-ulong buffer))
         (req (find-waiting-request request-id))
         (status (unmarshal-ulong buffer)))
    (when req
      (setf (request-service-context req) service-context)
      (cond ((= status 3)               ; Forward
             (setf (object-forward (request-target req))
                   (unmarshal-ior buffer))
             (request-send req))
            ;; FIXME: should decode system exception also,
            ;; to check for .. and then retry without forwarding
            (t
             (setf (request-reply req) (cons status buffer)))))))

(defun get-response-locate-reply (conn &aux (buffer (connection-read-buffer conn)))
  (setup-outgoing-connection conn)
  (let* ((request-id (unmarshal-ulong buffer))
         (status (unmarshal-ulong buffer))
         (req (find-waiting-request request-id)))
    (when req
      (setf (request-reply req) status)
      (cond ((= status 2)
             (setf (object-forward (request-target req))
                   (unmarshal-ior buffer))))
      req)))


(defun request-wait-response (req)
  (loop
   while (not (request-reply req))
   do (orb-wait)))

(defun corba-read-reply (req &optional
                             (reply (request-reply req))
                             (status (car reply))
                             (buffer (cdr reply)))
  (ecase status
    ((0)				; No Exception
     (loop for nv in (request-paramlist req)
           when (/= 0 (logand ARG_OUT (op:arg_modes nv)))
           do (setf (any-value (op:argument nv))
                    (unmarshal (any-typecode (op:argument nv)) buffer)))
     (setf (request-reply req) t))
    ((1)				; User Exception
     (let* ((id (unmarshal-string buffer))
            (tc (get-typecode id))
            (retval (op:return_value req) ))
       (setf (any-value retval) (unmarshal tc buffer)
             (any-typecode retval) tc)))
    
    ((2)				; System Exception
     (let* ((id (unmarshal-string buffer))
	    (minor (unmarshal-ulong buffer))
	    (status (unmarshal CORBA::tc_completion_status buffer))
            (retval (op:return_value req)))
       (setf (any-value retval)
             (make-condition 'corba:systemexception
                             :id id :minor minor :completed status)
             (any-typecode retval) nil)))))

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
      (mess 2 "io-event: ~S ~S~%" event (type-of desc)))
    (let ((conn (gethash desc *desc-conn*)))
      (case event
        (:read-ready
         (io-descriptor-set-read desc nil 0 0)
         (connection-read-ready conn))
        (:write-ready
         (mess 2 "Write ready on desc: ~S" desc)
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

;;;; CORBA:Request - deferred operations

(define-method send_oneway ((req request))
  (request-send req :no-response))

(define-method send_deferred ((req request))
  (request-send req))

(define-method get_response ((req request))
  (request-wait-response req)
  (unless (eq t (request-reply req))
    (corba-read-reply req))
  (values))

(define-method poll_response ((req request))
  ;; FIXME: possibly (orb-wait ...) if without timeout
  (not (null (request-reply req))))
