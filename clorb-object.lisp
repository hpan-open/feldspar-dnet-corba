;;;; clorb-object.lisp --- CORBA:Object and other pseudo objects
;; $Id: clorb-object.lisp,v 1.5 2002/05/30 06:41:49 lenst Exp $

(in-package :clorb)

;;; CORBA Object Interface

;;;| InterfaceDef get_interface ();
;;; Strange that the lisp mapping does not rename this.
;;;| boolean	 is_nil();
;;; _is_nil in lisp mapping, could as well use null?
;;;| Object	 duplicate ();
;;;| void	 release ();
;;; duplicate, release not in lisp mapping
;;;| boolean	is_a (in string logical_type_id);
;;; _is_a in lisp mapping (in clorb-request)
;;;| boolean	non_existent();
;;; _non_existent in lisp mapping
;;;| boolean	is_equivalent (in Object other_object);
;;; _is_equivialent in lisp mapping
;;;| unsigned 	long hash(in unsigned long maximum);
;;; _hash in lisp mapping
;;;
;;;| Status create_request (			
;;; _create_request in lisp mapping
;;;|     in Context	ctx,				
;;;|     in Identifier	operation,				

;;;|     in NVList	arg_list,				
;;;|     inout NamedValue result,				
;;;|     out Request	request,		
;;;|     in Flags        req_flags    );
;;;
;;;| Policy get_policy (in PolicyType policy_type );
;;; _get_policy
;;;| DomainManagersList get_domain_managers ();
;;; _get_domain_managers
;;;| Object set_policy_overrides (in PolicyList policies,
;;;|             in SetOverrideType set_add);
;;; _set_policy_overrides



;;;; CORBA:NamedValue

(defconstant ARG_IN 1)
(defconstant ARG_OUT 2)
(defconstant ARG_INOUT 3)


(define-corba-class CORBA:NamedValue ()
  :attributes ((name) 
               (argument) 
               (arg_modes)))

(defun CORBA:NamedValue (&key (name "") argument (arg_modes ARG_IN))
  (make-instance 'CORBA:NamedValue 
    :name name :argument argument :arg_modes arg_modes))


;;;; CORBA:Object / CORBA:Proxy

(defclass CORBA:Object ()
  ())

(defclass CORBA:Proxy (CORBA:Object)
  ((id :initform nil :initarg :id :accessor object-id)
   (connection :initform nil :accessor object-connection)
   (host :initform nil :initarg :host :accessor object-host)
   (port :initform nil :initarg :port :accessor object-port)
   (key :initform nil :initarg :key :accessor object-key)
   (raw-profiles :initform nil :initarg :raw-profiles
                 :accessor object-raw-profiles)
   (profiles :initform nil :initarg :profiles :accessor object-profiles)
   (forward :initform nil :initarg :forward :accessor object-forward)))


(defmethod print-object ((o corba:proxy) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~S @ ~A:~A"
            (object-id o)
            (object-host o)
            (object-port o))))

(define-method _is_nil ((x null))
  t)

(define-method _is_nil ((x CORBA:Object))
  nil)

;;;| boolean	is_equivalent (in Object other_object);
;;; _is_equivalent in lisp mapping

(define-method _is_equivalent ((obj corba:proxy) other)
  (if (object-host obj)
      (and
       (equal (object-host obj) (object-host other))
       (equal (object-port obj) (object-port other))
       (equalp (object-key obj) (object-key other)))
      (if (object-profiles obj)
          (let ((profile1 (car (object-profiles obj)))
                (profile2 (car (object-profiles other))))
            (and
             (equal (iiop-profile-host profile1) (iiop-profile-host profile2))
             (equal (iiop-profile-port profile1) (iiop-profile-port profile2))
             (equalp (iiop-profile-key profile1) (iiop-profile-key profile2)))))))


;;;| unsigned 	long hash(in unsigned long maximum);
;;; _hash in lisp mapping

(define-method _hash ((obj corba:proxy) maximum)
  (rem (sxhash (list* (object-host obj)
                      (object-port obj)
                      (coerce (object-key obj) 'list)))
       maximum))


;;;| Status create_request (			
;;; _create_request in lisp mapping
;;;|     in Context	ctx,				
;;;|     in Identifier	operation,				
;;;|     in NVList	arg_list,				
;;;|     inout NamedValue result,				
;;;|     out Request	request,		
;;;|     in Flags        req_flags    );

(define-method _create_request ((obj CORBA:Object)
                                ctx operation arg_list result req_flags)
  (declare (ignorable req_flags))
  (if result
      (setf (op:arg_modes result) ARG_OUT)
    (setq result (CORBA:NamedValue :arg_modes ARG_OUT
                                   :argument (CORBA:Any))))
  (values
   result
   (make-instance 'request
     :target obj
     :operation operation
     :paramlist (cons result (copy-seq arg_list))
     :ctx ctx)))


;;;| boolean is_a (in string logical_type_id);
;;; _is_a in lisp mapping (in clorb-request)

(define-method _is_a ((obj object) interface-id)
  (cond
   ((equal interface-id (object-id obj)) t)
   (t   
    (multiple-value-bind (result req)
      (op:_create_request
       obj nil "_is_a"  
       (list
        (CORBA:NamedValue
         :argument interface-id
         :arg_modes ARG_IN))
       (CORBA:NamedValue
        :argument (CORBA:Any :any-typecode CORBA:tc_boolean)
        :arg_modes ARG_OUT)
       0)
      (declare (ignore result))
      (request-funcall req)))))


;;;| boolean	non_existent();
;;; _non_existent in lisp mapping

(define-method _non_existent ((obj CORBA:Proxy))
  ;;FIXME: Should perhaps send a "_non_existent" message to object ?
  (= (locate obj) 0))


;;; Deferred from Any

(defmethod any-value ((obj CORBA:Object))
  obj)



;;;; CORBA:Request

(define-corba-class CORBA:Request ()
  :attributes 
  ((target    nil :readonly  :reader request-target)
   (operation nil :readonly  :reader request-operation)
   ;;(arguments nil :readonly  :virtual)
   ;;(result    nil :readonly  :virtual)
   (ctx       nil))
  ;; env  contexts 
  :slots 
  ((paramlist :initform nil :initarg :paramlist
              :accessor request-paramlist) ;result + arguments
   (req-id :initform nil :accessor request-req-id)
   (connection :initform nil :accessor request-connection)
   (reply :initform nil  :accessor request-reply)
   (service-context :initform nil :accessor request-service-context)
   (exceptions :initform nil :accessor request-exceptions)))


(define-method result ((r request))
  (first (request-paramlist r)))

(define-method set_return_type ((r request) tc)
  (setf (any-typecode (op:argument (first (request-paramlist r)))) tc))

(define-method return_value ((r request))
  (op:argument (first (request-paramlist r))))

(define-method arguments ((r request))
  (cdr (request-paramlist r)))


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

(define-method add_in_arg ((req request))
  (add-arg req nil ARG_IN))

(define-method add_named_in_arg ((req request) name)
  (add-arg req name ARG_IN))

(define-method add_inout_arg ((req request))
  (add-arg req nil ARG_INOUT))

(define-method add_named_inout_arg ((req request) name)
  (add-arg req name ARG_INOUT))

(define-method add_out_arg ((req request) &optional (name ""))
  (add-arg req name ARG_OUT))

(define-method add_named_out_arg ((req request) name)
  (add-arg req name ARG_OUT))

(defun add-exception (request typecode)
  (push typecode (request-exceptions  request)))


;;; void send_oneway ()
(define-deferred send_oneway ((req request)))

;;; void send_deferred ()
(define-deferred send_deferred ((req request)))

;;; void get_response ()
(define-deferred get_response ((req request)))

;;; boolean poll_response ()
(define-deferred poll_response ((req request)))

;;; void invoke ()
(define-method invoke ((req request))
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


;;;; Registry for Proxy classes

(defvar *proxy-classes*
    (make-hash-table :test #'equal))

(defun find-proxy-class (id)
  (gethash id *proxy-classes* 'CORBA:Proxy))

(defun register-proxy-class (id class)
  (setf (gethash id *proxy-classes*) class))

(defun object-narrow (obj id)
  "Return an equivalent proxy with class for the repository id."
  (assert (op:_is_a obj id))
  (make-instance (find-proxy-class id)
    :id id
    :host (object-host obj)
    :port (object-port obj)
    :key  (object-key obj)
    :profiles (object-profiles obj)
    :forward (object-forward obj)
    :profiles (object-profiles obj)
    :raw-profiles (object-raw-profiles obj)))


;;; clorb-object.lisp ends here
