;;;; clorb-request.lisp -- Client Request
;; $Id: clorb-request.lisp,v 1.9 2004/01/21 17:48:32 lenst Exp $

(in-package :clorb)



;;;; CORBA:Request

(defclass CORBA:Request ()
  ())


;;;; Client-Request 

(defclass client-request (CORBA:Request)
  ((the-orb
    :initarg :the-orb
    :reader the-orb)
   (target
    :initarg :target
    :reader request-target)
   (operation
    :initarg :operation
    :reader request-operation)
   (paramlist
    :initarg :paramlist             :initform nil 
    :accessor request-paramlist
    :documentation "result + arguments") 
   (request-id
    :initarg :request-id
    :accessor request-id)
   (connection
    :initarg :connection            :initform nil
    :accessor request-connection)
   (service-context-list
    :initarg :service-context-list  :initform nil
    :accessor service-context-list)
   (reply-service-context
    :initarg :reply-service-context :initform nil
    :accessor reply-service-context)
   (response-expected
    :initarg :response-expected     :initform t
    :accessor response-expected)
   (effective-profile
    :accessor request-effective-profile)
   (status
    :initarg :status                :initform nil
    :accessor request-status)
   (buffer
    :initarg :buffer                :initform nil
    :accessor request-buffer)
   (exception
    :initarg :exception             :initform nil 
    :accessor request-exception
    :documentation "Reply exception")
   (exceptions
    :initarg :exceptions            :initform nil
    :accessor request-exceptions
    :documentation "Valid exceptions")))


(defmethod initialize-instance :before ((req client-request) &key (the-orb nil orb-p))
  (declare (ignore the-orb))
  (unless orb-p (error ":the-orb not supplied to client-request")))

(defgeneric create-client-request (orb &rest initargs))


(define-method target ((r client-request))
  (request-target r))

(define-method operation ((r client-request))
  (request-operation r))

(define-method ctx ((r client-request))
  (raise-system-exception 'CORBA:no_implement))

(define-method result ((r client-request))
  (first (request-paramlist r)))

(define-method set_return_type ((r client-request) tc)
  (setf (any-typecode (op:argument (first (request-paramlist r)))) tc))

(define-method return_value ((r client-request))
  (let ((params (request-paramlist r)))
    (unless params
      (setf params (list (CORBA:NamedValue :argument (CORBA:Any) :arg_modes ARG_OUT)))
      (setf (request-paramlist r) params))
    (let ((result (op:argument (first params))))
      (when (request-exception r)
        (setf (any-value result) (request-exception r)))
      result)))

(define-method arguments ((r client-request))
  (cdr (request-paramlist r)))


(defun add-arg (req name mode &optional typecode value)
  (when (or value typecode)
    (check-type typecode CORBA:TypeCode))
  (let ((arg (CORBA:Any :any-typecode typecode
                        :any-value value)))
    (setf (request-paramlist req)
      (nconc (request-paramlist req) 
             (list (CORBA:NamedValue
                    :name name
                    :argument arg
                    :arg_modes mode))))
    arg))

(define-method add_in_arg ((req client-request))
  (add-arg req nil ARG_IN))

(define-method add_named_in_arg ((req client-request) name)
  (add-arg req name ARG_IN))

(define-method add_inout_arg ((req client-request))
  (add-arg req nil ARG_INOUT))

(define-method add_named_inout_arg ((req client-request) name)
  (add-arg req name ARG_INOUT))

(define-method add_out_arg ((req client-request) &optional (name ""))
  (add-arg req name ARG_OUT))

(define-method add_named_out_arg ((req client-request) name)
  (add-arg req name ARG_OUT))

(defun add-exception (req typecode)
  (push typecode (request-exceptions req)))


;; Used in stubs, shorter code then op:_create_request
(defun request-create (obj operation result-type )
  (create-client-request
   (the-orb obj)
   :target obj
   :operation operation
   :paramlist (list (CORBA:NamedValue :arg_modes ARG_OUT
                                      :argument (CORBA:Any :any-typecode result-type)))))


(defun request-start-request (req)
  (let ((object (request-target req)))
    (let ((conn (get-object-connection object)))
      (unless conn
        (raise-system-exception 'corba:transient 2 :completed_no))
      (setf (request-connection req) conn)    
      (setf (request-status req) nil)
      ;;(setf (request-id req) (next-request-id conn))
      (setf (request-effective-profile req)
            (selected-profile (or (object-forward object) object)))
      (values conn))))


(defun request-marshal-head (req)
  (let ((connection (request-start-request req)))
    (setf (request-id req) (next-request-id connection))
    (will-send-request (the-orb req) req)
    (setf (request-buffer req) 
          (connection-start-request
           connection
           (request-id req)
           (service-context-list req)
           (response-expected req)
           (request-effective-profile req)
           (request-operation req)))))


(defun request-marshal (req)
  (let ((buffer (request-marshal-head req)))
    (loop for nv in (request-paramlist req)
          when (/= 0 (logand ARG_IN (op:arg_modes nv)))
          do (marshal-any-value (op:argument nv) buffer))))



(defun send-request (req)
  (let ((buffer (request-buffer req)))
    (setf (request-buffer req) nil)
    (setf (request-status req) nil)
    (connection-send-request (request-connection req) buffer
                             (if (response-expected req) req))))


;;; void send_oneway ()

(define-method send_oneway ((req client-request))
  (setf (response-expected req) nil)  
  (request-marshal req)
  (send-request req))


;;; void send_deferred ()

(define-method send_deferred ((req client-request))
  (request-marshal req)
  (send-request req))



;;;; response


(defun request-locate-repy (req status buffer)
  (setf (request-status req) status)
  (setf (request-buffer req) buffer))


(defun request-reply (req status buffer service-context)
  (setf (request-status req) status)
  (setf (request-buffer req) buffer)
  (setf (reply-service-context req) service-context))


(defun request-unmarshal-result (req buffer)
  (loop for nv in (request-paramlist req)
        when (/= 0 (logand ARG_OUT (op:arg_modes nv)))
        do (setf (any-value (op:argument nv))
                 (unmarshal (any-typecode (op:argument nv)) buffer))))


(defun request-exception-typecode (request id)
  (find id (request-exceptions request)
        :key #'op:id :test #'equal))


(defun request-unmarshal-userexception (req buffer)
  (let* ((id (unmarshal-string buffer))
         (tc (request-exception-typecode req id)))
    (setf (request-exception req) 
          (if tc 
            (unmarshal tc buffer)
            (system-exception 'corba:unknown 1 :completed_yes)))))


(defun request-unmarshal-systemexception (req buffer)
  (setf (request-exception req) (unmarshal-systemexception buffer)))


(defun request-wait-response (req)
  (orb-wait #'request-status req))



;;; void get_response () raises (WrongTransaction);

;; get_response returns the result of a request. If get_response is
;; called before the request has completed, it blocks until the
;; request has completed. Upon return, the out parameters and return
;; values defined in the Request are set appropriately and they may be
;; treated as if the Request invoke operation had been used to perform
;; the request.

;; A request has an associated transaction context if the thread
;; originating the request had a non-null transaction context and the
;; target object is a transactional object. The get_response operation
;; may raise the WrongTransaction exception if the request has an
;; associated transaction context, and the thread invoking
;; get_response either has a null transaction context or a non-null
;; transaction context that differs from that of the request.

(define-method op::get_response ((req client-request))
  (loop 
    (request-wait-response req)
    (let ((buffer (request-buffer req)))
      (unless buffer (return))
      (let ((status (request-status req)))
        (case status
          (:location_forward
           (setf (object-forward (request-target req))
                 (unmarshal-object buffer))
           (has-received-other (the-orb req) req)
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



;;; void invoke ()
(define-method op::invoke ((req client-request))
  (op:send_deferred req)
  (op:get_response req))


(defun request-funcall (req)
  (op:invoke req)
  (let ((retval (any-value (op:return_value req))))
    (cond ((typep retval 'corba:exception)
           ;;(signal retval)
           (cerror "Ignore" retval)
           ;; or error
           retval)
          (t
           (values-list 
            (loop for nv in (request-paramlist req)
                  when (and (/= 0 (logand ARG_OUT (op:arg_modes nv)))
                            (not (eql :tk_void 
                                      (typecode-kind 
                                       (any-typecode (op:argument nv))))))
                  collect (any-value (op:argument nv))))))))

