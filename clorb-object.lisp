;;;; clorb-object.lisp --- CORBA:Object and other pseudo objects

(in-package :clorb)


;;;; Connection forward

(defgeneric profile-connection (profile orb))


;;;; CORBA Object Interface

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


;;;; Generic Functions

#-clisp
(defgeneric object-is-a (object id)
  (:method-combination or))
#+clisp
(defgeneric object-is-a (object id))

#+clisp
(defmethod object-is-a ((object t) id)
  (declare (ignore id))
  nil)

(defgeneric object-id (object))


(define-feature "_THIS"
  :documentation "Used for implicit activation during marshalling.")

(define-method "_THIS" ((object t))
  (raise-system-exception 'CORBA:MARSHAL 4 :completed_no))



;;;; CORBA:Object

(define-interface CORBA:Object ()
  :id "IDL:omg.org/CORBA/Object:1.0"
  :name "Object")


;;;| boolean	 is_nil();

(define-method _is_nil ((x null))
  t)

(define-method _is_nil ((x CORBA:Object))
  nil)


;;;| boolean is_a (in string logical_type_id);

(define-method _is_a ((obj t) interface-id)
  (declare (ignore interface-id))
  (raise-system-exception 'CORBA:no_implement 3 :completed_no))

(define-method _is_a ((obj CORBA:Object) interface-id)
  (object-is-a obj interface-id))



;;;; CORBA:Proxy


(defclass CORBA:PROXY (corba:object)
  ((id :initform nil :initarg :id :accessor proxy-id)
   (the-orb  :initarg :the-orb  :accessor the-orb)
   (connection :initform nil :accessor object-connection)
   (raw-profiles :initform nil :initarg :raw-profiles
                 :accessor object-raw-profiles)
   (profiles :initform nil :initarg :profiles :accessor object-profiles)
   (selected-profile :initform nil :accessor selected-profile)
   (forward :initform nil :initarg :forward :accessor object-forward)
   (forward-reset :initform nil :accessor object-forward-reset)))

(defgeneric profile-short-desc (profile stream)
  (:method ((profile t) stream) (write-string "*" stream)))

(defmethod print-object ((o corba:proxy) stream)
  (print-unreadable-object (o stream :type t)
    (when (eql (class-of o) (find-class 'corba:proxy))
      (format stream "~S @" (proxy-id o)))
    (let ((profile (loop with x = o
                         while (object-forward x)
                         do (setf x (object-forward x))
                         finally (return (or (selected-profile x)
                                             (first (object-profiles x)))))))
      (if profile
        (profile-short-desc profile stream)
        (write-string "--" stream)))))


(defmethod (setf object-forward) :before (val (proxy CORBA:Proxy))
  (declare (ignore val))
  ;; Forget old connection when forwarding
  (setf (object-connection proxy) nil))


#+unused-functions
(defun object-key (object)
  (let ((p (selected-profile object)))
    (and p (iiop-profile-key p))))

(defgeneric raw-profiles (proxy))

(defmethod marshal-object ((objref CORBA:Proxy) buffer)
  (marshal-string (proxy-id objref) buffer)
  (marshal-sequence (raw-profiles objref) #'marshal-tagged-component buffer))


(defun get-object-connection (proxy)
  ;; get the connection to use for a proxy object.
  (let ((conn (object-connection proxy)))
    (unless (and conn (connection-working-p conn))
      (setf (object-connection proxy) nil)
      (setq conn (connect-object proxy)))
    conn))


(defun connect-object (proxy)
  ;; select a profile and create a connection for that profile
  (or
   (let ((forward (object-forward proxy)))
     (if forward
       (cond ((setf (object-connection proxy) (connect-object forward))
              (setf (object-forward-reset proxy) nil)
              (object-connection proxy))
             ((object-forward-reset proxy)
              (setf (object-forward proxy) nil)
              (warn "Object forwarding fail")
              (return-from connect-object nil))
             (t
              (setf (object-forward-reset proxy) t)
              (setf (object-forward proxy) nil)))))
   (dolist (profile (object-profiles proxy))
     (let ((conn (profile-connection profile (the-orb proxy))))
       (when (and conn (connection-working-p conn))
         (setf (object-connection proxy) conn)
         (setf (selected-profile proxy) profile)
         (return conn))))))


;;;| boolean	is_equivalent (in Object other_object);
;;; _is_equivalent in lisp mapping

(define-method _is_equivalent ((obj t) other)
  (eql obj other))

(defgeneric profile-equal (profile1 profile2)
  (:method ((profile1 t) (profile2 t)) (eq profile1 profile2)))

(define-method _is_equivalent ((obj corba:proxy) other)
  (let ((profile1 (car (object-profiles obj)))
        (profile2 (car (object-profiles other))))
    (and profile1 profile2
         (profile-equal profile1 profile2))))


;;;| unsigned 	long hash(in unsigned long maximum);
;;; _hash in lisp mapping

(define-method _hash ((obj t) maximum)
  (rem (sxhash obj)
       maximum))

(defgeneric profile-hash (profile))

(define-method _hash ((obj corba:proxy) maximum)
  (rem (if (object-profiles obj)
         (profile-hash (first (object-profiles obj)))
         0)
       maximum))




;;;| Status create_request (			
;;; _create_request in lisp mapping
;;;|     in Context	ctx,				
;;;|     in Identifier	operation,				
;;;|     in NVList	arg_list,				
;;;|     inout NamedValue result,				
;;;|     out Request	request,		
;;;|     in Flags        req_flags    );

(define-method _create_request ((obj t) ctx operation arg_list result req_flags)
  (declare (ignore ctx operation arg_list result req_flags))
  (raise-system-exception 'CORBA:NO_IMPLEMENT 4 :completed_no))


(define-method _create_request ((obj CORBA:Object)
                                ctx operation arg_list result req_flags)
  (declare (ignorable req_flags))
  (if result
      (setf (op:arg_modes result) ARG_OUT)
    (setq result (CORBA:NamedValue :arg_modes ARG_OUT
                                   :argument (CORBA:Any))))
  (values
   result
   (make-instance 'client-request
     :target obj
     :operation operation
     :paramlist (cons result (copy-seq arg_list))
     :ctx ctx)))



(define-method _is_a ((obj CORBA:Proxy) interface-id)
  (or (object-is-a obj interface-id)
      (static-call ("_is_a" obj)
                   :output ((buffer) (marshal-string interface-id buffer))
                   :input ((buffer) (unmarshal-bool buffer)))))


;;;| boolean	non_existent();
;;; _non_existent in lisp mapping

(define-method _non_existent ((obj t))
  nil)

(define-method _non_existent ((obj CORBA:Proxy))
  ;;FIXME: Should perhaps send a "_non_existent" message to object ?
  (eq (locate obj) :unknown_object ))


;;; Deferred from Any

(defmethod any-value ((obj corba:object))
  obj)



;;;; Registry for Proxy classes

(defvar *proxy-classes*
    (make-hash-table :test #'equal))

(defun find-proxy-class (id)
  (gethash id *proxy-classes* 'CORBA:Proxy))

(defun register-proxy-class (id class)
  (setf (gethash id *proxy-classes*) class))

(defun object-narrow (obj id &optional no-error)
  "Return an equivalent proxy with class for the repository id."
  (when (symbolp id)
    (setq id (symbol-ifr-id id)))
  (cond ((op:_is_a obj id)
         (make-instance (find-proxy-class id)
                        :id id
                        :forward (object-forward obj)
                        :profiles (object-profiles obj)
                        :raw-profiles (object-raw-profiles obj)))
        (no-error nil)
        (t
         (error "Object of wrong type for narrowing"))))

(defun nobject-narrow (obj id &optional no-error)
  "Return an equivalent proxy with class for the repository id.
Might destructivley change the original object."
  (when (symbolp id)
    (setq id (symbol-ifr-id id)))
  (cond ((op:_is_a obj id)
         (setf (proxy-id obj) id)
         (change-class obj (find-proxy-class id)))
        (no-error nil)
        (t
         (error "Object of wrong type for narrowing"))))

(defun auto-narrow (obj)
  ;; Try convert to a type-specific proxy
  (when (eql (class-of obj) (find-class 'CORBA:Proxy))
    (when (equal (proxy-id obj) "")
      (unless (object-forward obj)
        (locate obj))
      (let ((forward (object-forward obj)))
        (when forward
          (auto-narrow forward)
        (unless (eql (class-of forward) (find-class 'CORBA:Proxy))
          (setf (proxy-id obj) (proxy-id forward))
          (change-class obj (class-of forward))))))))


;;; Something like this, but with a clorb specific meta class for generic function
#+(or)
(defmethod no-applicable-method ((generic-function standard-generic-function) &rest args)
  (cond ((and (cdr args)
              (typep (car args) 'CORBA:Proxy)
              (auto-narrow (car args)))
         (apply generic-function args))
        (t 
         (call-next-method))))


;;;; ValueBase

(defclass CORBA:ValueBase ()
  ())


;;;; AbstractBase

(defclass CORBA:AbstractBase (CORBA:Object)
  ())




;;; clorb-object.lisp ends here
