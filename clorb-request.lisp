;;;; clorb-request.lisp -- Client Request
;; $Id: clorb-request.lisp,v 1.12 2004/02/08 19:18:57 lenst Exp $

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
    :initarg :status                :initform :initial
    :accessor request-status)
   (buffer
    :initarg :buffer                :initform nil
    :accessor request-buffer)
   (output-func
    :initarg :output-func           :initform 'dii-output-func
    :accessor output-func)
   (input-func
    :initarg :input-func            :initform 'dii-input-func
    :accessor input-func)
   (error-handler
    :initarg :error-handler         :initform 'dii-error-handler
    :accessor error-handler)
   (exception
    :initarg :exception             :initform nil 
    :accessor request-exception
    :documentation "Reply exception")
   (exceptions
    :initarg :exceptions            :initform nil
    :accessor request-exceptions
    :documentation "Valid exceptions")))


(defmethod initialize-instance :before ((req client-request)
                                        &key (the-orb nil orb-p))
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
      (setf params (list (CORBA:NamedValue :argument (CORBA:Any)
                                           :arg_modes ARG_OUT)))
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


;;;; Sending requests


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


(defun static-error-handler (condition)
  (error condition))


(defun do-static-call (obj operation response-expected
                       output-func input-func exceptions)
  (let ((req (create-client-request
              (the-orb obj)
              :target obj
              :operation operation
              :response-expected response-expected
              :exceptions exceptions
              :output-func output-func
              :input-func input-func 
              :error-handler #'static-error-handler)))
    (request-send req)
    (when response-expected
      (request-get-response req))))


(defun request-invoke (req)
  (request-send req)
  (request-get-response req))


(defun request-send (req)
  (setf (request-status req) nil)
  (setf (service-context-list req) nil)
  (let ((connection (request-start-request req)))
    (setf (request-id req) (next-request-id connection))
    (will-send-request (the-orb req) req)
    (let ((buffer (connection-start-request
                   connection
                   (request-id req)
                   (service-context-list req)
                   (response-expected req)
                   (request-effective-profile req)
                   (request-operation req))))
      (setf (request-buffer req) buffer)
      (funcall (output-func req) req buffer) 
      (setf (request-buffer req) nil)
      (connection-send-request connection buffer
                               (if (response-expected req) req)))))


;;; void send_oneway ()

;; Calling send on a request after invoke, send, or
;; send_multiple_requests for that request was called raises
;; BAD_INV_ORDER with standard minor code 10.

(define-method send_oneway ((req client-request))
  (unless (eql (request-status req) :initial)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 10 :completed_no))
  (setf (response-expected req) nil)
  (request-send req))


;;; void send_deferred ()

(define-method send_deferred ((req client-request))
  (unless (eql (request-status req) :initial)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 10 :completed_no))
  (request-send req)
  (add-pending-client-request (the-orb req) req))




;;;; Response handling


(defun request-locate-reply (req status buffer)
  (setf (request-status req) status)
  (setf (request-buffer req) buffer))


(defun request-reply (req status buffer service-context)
  (setf (request-status req) status)
  (setf (request-buffer req) buffer)
  (setf (reply-service-context req) service-context))


(defun request-wait-response (req)
  (orb-wait #'request-status req))


(defun request-handle-error (req)
  (has-received-other (the-orb req) req)
  (setf (request-status req) :returned)
  (funcall (error-handler req) (request-exception req)))


(defun request-get-response (req)
  (request-wait-response req)
  (let ((buffer (request-buffer req)))
    (case (request-status req)
      (:initial (raise-system-exception 'CORBA:BAD_INV_ORDER))
      (:error                             ; Communication error
       (setf (request-status req) :system_exception)
       (request-handle-error req))
      (:user_exception
       (let ((id (unmarshal-string buffer)))
         (let ((tc (find id (request-exceptions req)
                         :key #'op:id :test #'equal)))
         (setf (request-exception req) 
               (cond (tc (unmarshal-userexception id tc buffer))
                     (t
                      (setf (request-status req) :system_exception)
                      (system-exception 'corba:unknown 1 :completed_yes))))))
       (request-handle-error req))
      (:system_exception
       (setf (request-exception req) (unmarshal-systemexception buffer))
       (request-handle-error req))
      (:location_forward
       (setf (object-forward (request-target req))
             (unmarshal-object buffer))
       (has-received-other (the-orb req) req)
       (request-invoke req))
      (:no_exception
       ;; FIXME: the has-received-reply should ideally be done after
       ;; unmarshalling result, but then we can't simply return the
       ;; result as we do here for static calls. The static call would
       ;; have to cons a list of results..
       (has-received-reply (the-orb req) req)
       (setf (request-status req) :returned)
       (funcall (input-func req) req buffer)))))


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
  (unless (remove-pending-request (the-orb req) req)
    ;; FIXME: ??
    (raise-system-exception 'CORBA:BAD_INV_ORDER))
  (request-get-response req)
  (values))


;;; boolean poll_response ();
;;
;; poll_response determines whether the request has completed. A TRUE return indicates
;; that it has; FALSE indicates it has not.
;;
;; Return is immediate, whether the response has completed or not. Values in the request are
;; not changed.

;; Exception BAD_INV_ORDER with minor
;;  * 11 - request not sendt
;;  * 12 - request already returned
;;  * 13 - request invoked

(define-method poll_response ((req client-request))
  ;; FIXME: 13
  (case (request-status req)
    ((:initial) (raise-system-exception 'CORBA:BAD_INV_ORDER 11 :completed_no))
    ((:returned) (raise-system-exception 'CORBA:BAD_INV_ORDER 12 :completed_yes))
    ((nil) nil)
    (otherwise t)))


;;; void invoke ()

(define-method op::invoke ((req client-request))
  (request-send req)
  (request-get-response req)
  (values))


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


(defun dii-output-func (req buffer)
  (loop for nv in (request-paramlist req)
        when (/= 0 (logand ARG_IN (op:arg_modes nv)))
        do (marshal-any-value (op:argument nv) buffer)))


(defun dii-input-func (req buffer)
  (loop for nv in (request-paramlist req)
        when (/= 0 (logand ARG_OUT (op:arg_modes nv)))
        do (setf (any-value (op:argument nv))
                 (unmarshal (any-typecode (op:argument nv)) buffer))))


(defun dii-error-handler (condition)
  (declare (ignore condition)))



;;;; Stub support


(defun symbol-op-info (sym)
  "Returns: name result params mode exc-syms"
  (values (get sym 'ifr-name)
          (get sym 'ifr-result)
          (get sym 'ifr-params)
          (get sym 'ifr-mode)
          (get sym 'ifr-exceptions)))


(defun symbol-attr-info (sym)
  "Returns: name type mode"
  (values (get sym 'ifr-name)
          (get sym 'ifr-type)
          (get sym 'ifr-mode)))


(defun compute-static-call (sym)
  (let (input-func output-func exceptions op response-expected)
    (lambda (obj &rest args)
      (unless input-func
        (multiple-value-bind (name result params mode exc-syms)
                             (symbol-op-info sym)
          (setq input-func
                (let ((ufuns (loop for (nil pmode tc) in params
                                   unless (eql pmode :param_in) collect (unmarshal-function tc))))
                  (typecase result
                    (void-typecode)
                    (t (push (unmarshal-function result) ufuns)))
                  (lambda (req buffer)
                    (declare (ignore req))
                    (values-list (loop for u in ufuns collect (funcall u buffer))))))
          (setq output-func 
                (let ((mfuns (loop for (nil pmode tc) in params
                                   unless (eql pmode :param_out) collect (marshal-function tc))))
                  (lambda (req buffer args)
                    (declare (ignore req))
                    (loop for a in args for m in mfuns do (funcall m a buffer)))))
          (setq exceptions (mapcar #'symbol-typecode exc-syms))
          (setq op name)
          (setq response-expected (eq mode :op_normal))))
      (do-static-call obj op response-expected 
                      (lambda (req buffer) (funcall output-func req buffer args))
                      input-func exceptions))))

(defun compute-static-get (sym)
  (let (op input-func)
    (lambda (obj)
      (unless input-func
        (multiple-value-bind (name type)
                             (symbol-attr-info sym)
          (setq op (getter-name name))
          (setq input-func
                (let ((mfun (unmarshal-function type)))
                  (lambda (req buffer)
                    (declare (ignore req))
                    (funcall mfun buffer))))))
      (do-static-call obj op t (lambda (req buffer) (declare (ignore req buffer)))
                      input-func nil))))


(defun compute-static-set (sym)
  (let (op output-func)
    (lambda (obj value)
      (unless output-func
        (multiple-value-bind (name type mode)
                             (symbol-attr-info sym)
          (assert (eql mode :attr_normal))
          (setq op (setter-name name))
          (setq output-func
                (let ((mfun (marshal-function type)))
                  (lambda (req buffer arg)
                    (declare (ignore req))
                    (funcall mfun arg buffer))))))
      (do-static-call obj op t 
                      (lambda (req buffer)
                        (funcall output-func req buffer value))
                      (lambda (req buffer) (declare (ignore req buffer)))
                      nil))))



;;; clorb-request.lisp ends here
