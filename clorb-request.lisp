;;;; CORBA Requests

(in-package :clorb)


(defconstant ARG_IN 1)
(defconstant ARG_OUT 2)
(defconstant ARG_INOUT 3)


(define-corba-class CORBA::NamedValue ()
  :attributes ((name) 
               (argument) 
               (arg_modes)))

(defun CORBA::NamedValue (&key (name "") argument (arg_modes ARG_IN))
  (make-instance 'CORBA:NamedValue 
    :name name :argument argument :arg_modes arg_modes))


(define-corba-class CORBA:Request ()
  :attributes 
  ((target    nil :readonly  :reader request-target)
   (operation nil :readonly  :reader request-operation)
   ;;(arguments nil :readonly  :virtual)
   ;;(result    nil :readonly  :virtual)
   (ctx       nil))
  ;; env exceptions contexts 
  :slots 
  ((paramlist :initform nil :initarg :paramlist
              :accessor request-paramlist) ;result + arguments
   (req-id :initform nil :accessor request-req-id)
   (client :initform nil :accessor request-client)
   (opdef :initform nil :initarg :opdef
          :accessor request-opdef)
   (reply :initform nil  :accessor request-reply)
   (service-context :initform nil :accessor request-service-context)))

(define-method result ((r request))
  (first (request-paramlist r)))

(define-method set_return_type ((r request) tc)
  (setf (any-typecode (op:argument (first (request-paramlist r)))) tc))

(define-method return_value ((r request))
  (op:argument (first (request-paramlist r))))

(define-method arguments ((r request))
  (cdr (request-paramlist r)))


(defvar *request-id-seq* 0)
(defvar *corba-waiting-requests* nil)



;;;; Sending request

(defun request-send (req &optional flags)
  (let ((object (request-target req)))
    (setq object (or (object-forward object)
		     object))
    (handler-case (request-send-to req object flags)
     ;; FIXME: this is stupid!
     ;; it is when reading the response that the exception will be
     ;; detected! Move the following to the reply handling.
     ;; [lenst/2001-06-01 01:34:26] COMM_FAILURE could happen here.
     (corba:systemexception (exc)
       (setq object (request-target req))
       (cond ((and (object-forward object)
                   (member (exception-id exc)
                           '("IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0"
                             "IDL:omg.org/CORBA/COMM_FAILURE:1.0")
                           :test #'equal))
              (setf (object-forward object) nil)
              (request-send-to req object flags))
             (t
              (error exc)))))))


(defun request-send-to (req object &optional flags)
  (let* ((client (get-connection (object-host object)
				 (object-port object))))
    (handler-case 
        (request-send-to-client req object client flags)
      (file-error (exc)
        (close client)
        (error exc)))))

(defun marshal-any-value (any buffer)
  (marshal (any-value any)
           (any-typecode any)
           buffer))

(defun request-send-to-client (req object stream flags)
  (let ((operation (request-operation req))
        (buffer (get-work-buffer)))
    (setf (request-req-id req) (incf *request-id-seq*))
    (setf (request-client req) stream)
    (setf (request-reply req) nil)
    (setf (fill-pointer (buffer-octets buffer)) 0)
    (cond ((eq operation 'locate)
           (marshal-giop-header 3 buffer) ;LocateRequest
           (marshal-ulong (request-req-id req) buffer)
           (marshal-osequence (object-key object) buffer))
          (t
           (marshal-giop-header 'request buffer)
           (marshal-service-context *service-context* buffer)
           (marshal-ulong (request-req-id req) buffer)
           ;; respons expected-type
           (marshal-octet (if (member :no-response flags) 0 1) buffer)
           (marshal-osequence (object-key object) buffer)
           (marshal-string operation buffer)
           (marshal-osequence *principal* buffer)
           (loop for nv in (request-paramlist req)
               when (/= 0 (logand ARG_IN (op:arg_modes nv)))
                    do (marshal-any-value (op:argument nv) buffer))))
    (marshal-giop-set-message-length buffer)
    (write-sequence (buffer-octets buffer) stream)
    (force-output stream)
    (unless (member :no-response flags)
      (push req *corba-waiting-requests*))))

;;;; Getting reply

(defun corba-read-reply (req buffer)
  (ecase (unmarshal-ulong buffer)
    ((0)				; No Exception
     (loop for nv in (request-paramlist req)
         when (/= 0 (logand ARG_OUT (op:arg_modes nv)))
         do (setf (any-value (op:argument nv))
              (unmarshal (any-typecode (op:argument nv)) buffer)))
     (setf (request-reply req) t)
     t)
    ((1)				; User Exception
     (let* ((id (unmarshal-string buffer)))
       (error (unmarshal (get-typecode id) buffer))))
    ((2)				; System Exception
     (let* ((id (unmarshal-string buffer))
	    (minor (unmarshal-ulong buffer))
	    (status (unmarshal CORBA::tc_completion_status buffer)))
       (error 'corba:systemexception
	       :id id :minor minor :completed status)))
    ((3)				; Forward
     (setf (object-forward (request-target req))
	   (unmarshal-ior buffer))
     (request-send req)
     nil)))


(defun corba-get-next-respons-1 (stream &key wait)
  (unless (or wait (socket-stream-listen stream))
    (return-from corba-get-next-respons-1 nil))
  (let ((buffer (get-work-buffer))
	msgtype)
    ;; Read header
    (setf (fill-pointer (buffer-octets buffer)) 12)
    (read-sequence (buffer-octets buffer) stream 
                   :end (fill-pointer (buffer-octets buffer)))
    (setq msgtype (unmarshal-giop-header buffer))
    (let ((size (+ (unmarshal-ulong buffer) 12)))
      (if (< (array-total-size (buffer-octets buffer)) size)
          (adjust-array (buffer-octets buffer) size))
      (setf (fill-pointer (buffer-octets buffer)) size)
      (read-sequence (buffer-octets buffer) stream :start 12))
    (ecase msgtype
      ((1)                              ; Reply
       (let* ((service-context (unmarshal-service-context buffer))
              (request-id (unmarshal-ulong buffer))
              (req (find request-id *corba-waiting-requests*
                         :key #'request-req-id)))
         (cond (req
                (setq *corba-waiting-requests*
                  (delete req *corba-waiting-requests*))
                (setf (request-service-context req) service-context)
                (and (corba-read-reply req buffer)
                     req))
               (t
                (mess 4 "Unexpected respons for request ~d" request-id)
                nil))))
      ((4)                              ;Locate reply
       (let* ((request-id (unmarshal-ulong buffer))
              (status (unmarshal-ulong buffer))
              (req (find request-id *corba-waiting-requests*
                         :key #'request-req-id)))
         (cond (req
                (setq *corba-waiting-requests*
                  (delete req *corba-waiting-requests*))
                (setf (request-reply req) status)
                (cond ((= status 2)
                       (setf (object-forward (request-target req))
                         (unmarshal-ior buffer))))
                req)
               (t
                (mess 4 "Unexpected respons for request ~d" request-id)
                nil))))
      
      ((5)                              ;Close Connection
       (close stream)
       (error "Connection closed"))
      ((6)
       (format *error-output* "~&CORBA: Message error~%")
       nil))))

(defun get-waiting-request-streams ()
  (let (streams)
    (loop for r in *corba-waiting-requests*
        for s = (request-client r)
        do (when (open-stream-p s)
             (pushnew s streams)))
    streams))

(defun corba-get-next-respons (orb &optional flags)
  (loop
      for stream = (orb-wait orb (get-connections))
      for req = (cond 
                 ((null stream) nil)
                 ((eq stream :cant)
                  (loop for stream in (get-waiting-request-streams)
                      thereis (corba-get-next-respons-1 stream)))
                 (t
                  (corba-get-next-respons-1 stream)))
      until (or req (not (member :no-wait flags)))
      finally (return req)))

(defun request-get-response (req &optional flags)
  (let* ((orb (orb_init)))
    (loop for client = (request-client req)
        while (not (request-reply req))
        when (and (orb-wait orb client) (socket-stream-listen client))
        do (corba-get-next-respons-1 client
                                     :wait (not (member :no-wait flags)))
        until (member :no-wait flags)))
  (request-reply req))


;;;; Invokation

(defun request-invoke (req &optional flags)
  (declare (ignore flags))
  (request-send req)
  (request-get-response req)
  (values-list 
   (loop for nv in (request-paramlist req)
       when (and (/= 0 (logand ARG_OUT (op:arg_modes nv)))
                 (not (eql :tk_void 
                           (typecode-kind 
                            (any-typecode (op:argument nv))))))
       collect (any-value (op:argument nv)))))


;;;; Standard send/repsponse operations 

(define-method invoke ((r request))
  (request-send r)
  (request-get-response r))


;;;; Creation

(define-method _create_request ((obj CORBA:Object)
                                ctx operation arg_list result req_flags)
  (declare (ignorable ctx req_flags))
  (if result
      (setf (op:arg_modes result) ARG_OUT)
    (setq result (CORBA:NamedValue :arg_modes ARG_OUT
                                   :argument (CORBA:Any))))
  (values
   result
   (make-instance 'request
     :target obj
     :operation operation
     :paramlist (cons result (copy-seq arg_list)))))

(defun add-arg (req name mode &optional typecode value)
  (let ((arg (CORBA:Any :any-typecode typecode
                        :any-value value)))
    (setf (request-paramlist req)
      (nconc (request-paramlist req) 
             (list (CORBA:NamedValue
                    :name name
                    :argument arg
                    :arg_modes mode))))
    arg))

(define-method op::add_in_arg ((req request))
  (add-arg req nil ARG_IN))

(define-method op::add_inout_arg ((req request))
  (add-arg req nil ARG_INOUT))

(define-method op::add_out_arg ((req request) &optional (name ""))
  (add-arg req name ARG_OUT))

(define-method op::add_named_out_arg ((req request) name)
  (add-arg req name ARG_OUT))



(defun locate (obj)
  (let ((req (make-instance 'request :target obj :operation 'locate)))
    (request-send req)
    (request-get-response req)
    (request-reply req)))


;;;; The Object Interface

;;;| boolean is_a (in string logical_type_id);
;;; _is_a in lisp mapping (in clorb-request)

(define-method _is_a ((obj object) interface-id)
  (object-is-a obj interface-id))

(defun object-is-a (obj id)
  (cond
   ((equal id (object-id obj)) t)
   (t   
    (multiple-value-bind (result req)
      (op:_create_request
       obj nil "_is_a"  
       (list
        (CORBA:NamedValue
         :argument id
         :arg_modes ARG_IN))
       (CORBA:NamedValue
        :argument (CORBA:Any :any-typecode CORBA:tc_boolean)
        :arg_modes ARG_OUT)
       0)
      (declare (ignore result))
      (request-invoke req)))))

(defun object-narrow (obj id)
  (assert (object-is-a obj id))
  (make-instance (find-proxy-class id)
    :id id
    :host (object-host obj)
    :port (object-port obj)
    :key  (object-key obj)
    :profiles (object-profiles obj)
    :forward (object-forward obj)))

;;;| boolean	non_existent();
;;; _non_existent in lisp mapping

(define-method _non_existent ((obj CORBA:Proxy))
  ;; Should perhaps send a "_non_existent" message to object ?
  (= (locate obj) 0))

