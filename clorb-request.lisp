;;;; clorb-request.lisp -- Client Request
;; $Id: clorb-request.lisp,v 1.8 2004/01/18 11:13:06 lenst Exp $

(in-package :clorb)



;;;; CORBA:Request

(defclass CORBA:Request ()
  ())


;;;; Client-Request 

(defclass client-request (CORBA:Request)
  ((the-orb   :initarg :the-orb    :reader the-orb)
   (target    :initarg :target     :reader request-target)
   (operation :initarg :operation  :reader request-operation)
   (paramlist :initform nil :initarg :paramlist
              :accessor request-paramlist) ;result + arguments
   (req-id :initform nil :accessor request-req-id)
   (connection :initform nil :accessor request-connection)
   (service-context-list :initform nil :accessor service-context-list)
   (reply-service-context :initform nil :accessor reply-service-context)
   (response-expected :initform t :initarg :response-expected :accessor response-expected)
   ;;(forward :initform nil :accessor request-forward)
   (effective-profile    :accessor request-effective-profile)
   (status :initform nil :accessor request-status)
   (buffer :initform nil :accessor request-buffer)
   (exception  :initform nil :initarg :exception  :accessor request-exception
               :documentation "Reply exception")
   (exceptions :initform nil :initarg :exceptions :accessor request-exceptions
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
    (op:argument (first params))))

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
  (push typecode (request-exceptions  req)))


;;; void send_oneway ()
(define-deferred send_oneway ((req client-request)))

;;; void send_deferred ()
(define-deferred send_deferred ((req client-request)))

;;; void get_response ()
(define-deferred get_response ((req client-request)))

;;; boolean poll_response ()
(define-deferred poll_response ((req client-request)))

;;; void invoke ()
(define-method invoke ((req client-request))
  (op:send_deferred req)
  (op:get_response req))


;; Used in stubs, shorter code then op:_create_request
(defun request-create (obj operation result-type )
  (create-client-request
   (the-orb obj)
   :target obj
   :operation operation
   :paramlist (list (CORBA:NamedValue :arg_modes ARG_OUT
                                      :argument (CORBA:Any :any-typecode result-type)))))

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


(defun request-start-request (req)
  (let ((object (request-target req)))
    (let ((conn (get-object-connection object)))
      (unless conn
        (raise-system-exception 'corba:comm_failure))
      (setf (request-connection req) conn)    
      (setf (request-status req) nil)
      ;;(setf (request-req-id req) (next-request-id conn))
      (setf (request-effective-profile req)
            (selected-profile (or (object-forward object) object)))
      (values conn))))




;;;; response

(defun request-locate-repy (req status buffer)
  (setf (request-status req) status)
  (setf (request-buffer req) buffer))

(defun request-reply (req status buffer service-context)
  (setf (request-status req) status)
  (setf (request-buffer req) buffer)
  (setf (reply-service-context req) service-context))
