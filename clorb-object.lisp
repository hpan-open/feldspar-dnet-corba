;;;; clorb-object.lisp --- CORBA:Object and other pseudo objects

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

(defconstant arg_in 1)
(defconstant arg_out 2)
(defconstant arg_inout 3)


(define-corba-class CORBA:NamedValue ()
  :attributes ((name) 
               (argument) 
               (arg_modes)))

(defun corba:namedvalue (&key (name "") argument (arg_modes ARG_IN))
  (make-instance 'CORBA:NamedValue 
    :name name :argument argument :arg_modes arg_modes))


;;;; CORBA:Object / CORBA:Proxy

(defgeneric object-is-a (object id)
  (:method-combination or))

(defgeneric object-id (object))

(define-interface CORBA:Object ()
  :id "IDL:omg.org/CORBA/Object:1.0"
  :name "Object")

#+(or)  ;; defined by define-interface
(defmethod object-id ((obj corba:object))
  (or (some #'(lambda (x)
                (let ((n (class-name x)))
                  (and (symbolp n) (symbol-ifr-id n))))
            (%SYSDEP "class precedence list"
                     #+ccl (ccl:class-precedence-list (class-of obj))
                     (list (class-of obj))))
      "IDL:omg.org/Object:1.0"))


(defclass CORBA:PROXY (corba:object)
  ((id :initform nil :initarg :id :accessor proxy-id)
   (connection :initform nil :accessor object-connection)
   (raw-profiles :initform nil :initarg :raw-profiles
                 :accessor object-raw-profiles)
   (profiles :initform nil :initarg :profiles :accessor object-profiles)
   (selected-profile :initform nil :accessor selected-profile)
   (forward :initform nil :initarg :forward :accessor object-forward)))

(defmethod print-object ((o corba:proxy) stream)
  (print-unreadable-object (o stream :type t)
    (when (eql (class-of o) (find-class 'corba:proxy))
      (format stream "~S @" (object-id o)))
    (let ((profile (loop with x = o
                         while (object-forward x)
                         do (setf x (object-forward x))
                         finally (return (or (selected-profile x)
                                             (first (object-profiles x)))))))
      (if profile
        (format stream "~A:~A" 
                (iiop-profile-host profile)
                (iiop-profile-port profile))
        (format stream "--" )))))



(defun object-key (proxy)
  "Return a key for the proxy."
  ;; FIXME: this is insufficient in the long run,
  ;; there need to be two accessors one that always gives the same key
  ;; and one that gives the key for the selected profile.
  (iiop-profile-key (first (object-profiles proxy))))


(define-method _is_nil ((x null))
  t)

(define-method _is_nil ((x CORBA:Object))
  nil)

(defstruct IIOP-PROFILE
  (version '(1 . 0))
  (host    nil)
  (port    0    :type fixnum)
  (key     nil))

(defun object-key (object)
  (let ((p (selected-profile object)))
    (and p (iiop-profile-key p))))


;;;| boolean	is_equivalent (in Object other_object);
;;; _is_equivalent in lisp mapping

(define-method _is_equivalent ((obj corba:proxy) other)
  (let ((profile1 (car (object-profiles obj)))
        (profile2 (car (object-profiles other))))
    (and profile1 profile2
         (equal (iiop-profile-host profile1) (iiop-profile-host profile2))
         (equal (iiop-profile-port profile1) (iiop-profile-port profile2))
         (equalp (iiop-profile-key profile1) (iiop-profile-key profile2)))))


;;;| unsigned 	long hash(in unsigned long maximum);
;;; _hash in lisp mapping

(define-method _hash ((obj corba:proxy) maximum)
  (rem (if (object-profiles obj)
         (iiop-profile-hash (first (object-profiles obj)))
         0)
       maximum))

(defun iiop-profile-hash (profile)
  (sxhash (list* (iiop-profile-host profile)
                 (iiop-profile-port profile)
                 (coerce (iiop-profile-key profile) 'list))))



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

(define-method _is_a ((obj CORBA:Object) interface-id)
  (object-is-a obj interface-id))

(define-method _is_a ((obj CORBA:Proxy) interface-id)
  (or (object-is-a obj interface-id)
      (static-call ("_is_a" obj)
                   :output ((buffer) (marshal-string interface-id buffer))
                   :input ((buffer) (unmarshal-bool buffer)))))


;;;| boolean	non_existent();
;;; _non_existent in lisp mapping

(define-method _non_existent ((obj CORBA:Proxy))
  ;;FIXME: Should perhaps send a "_non_existent" message to object ?
  (eq (locate obj) :unknown_object ))


;;; Deferred from Any

(defmethod any-value ((obj corba:object))
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
   ;;(reply :initform nil  :accessor request-reply)
   (forward :initform nil :accessor request-forward)
   (status :initform nil :accessor request-status)
   (buffer :initform nil :accessor request-buffer)
   (service-context :initform nil :accessor request-service-context)
   (exceptions :initform nil :initarg :exceptions :accessor request-exceptions)))


(define-method result ((r request))
  (first (request-paramlist r)))

(define-method set_return_type ((r request) tc)
  (setf (any-typecode (op:argument (first (request-paramlist r)))) tc))

(define-method return_value ((r request))
  (op:argument (first (request-paramlist r))))

(define-method arguments ((r request))
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


;; Used in stubs, shorter code then op:_create_request
(defun request-create (obj operation result-type )
  (make-instance 'request
    :target obj
    :operation operation
    :paramlist (list (CORBA:NamedValue :arg_modes ARG_OUT
                                       :argument (CORBA:Any :any-typecode result-type)))))

(defun get-attribute (obj getter result-tc)
  (static-call (getter obj)
               :output ((buffer))
               :input ((buffer) (unmarshal result-tc buffer))))

(defun set-attribute (obj setter result-tc newval)
  (static-call (setter obj)
               :output ((buffer) (marshal newval result-tc buffer))
               :input ((buffer))))


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
  (when (symbolp id)
    (setq id (symbol-ifr-id id)))
  (assert (op:_is_a obj id))
  (make-instance (find-proxy-class id)
    :id id
    :forward (object-forward obj)
    :profiles (object-profiles obj)
    :raw-profiles (object-raw-profiles obj)))


;;; clorb-object.lisp ends here
