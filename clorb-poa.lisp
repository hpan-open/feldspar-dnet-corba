;;;; clorb-poa.lisp -- Portable Object Adaptor
;; $Id: clorb-poa.lisp,v 1.14 2002/11/20 16:42:07 lenst Exp $

(in-package :clorb)


;;;; Servant manager

(defclass PortableServer:ServantManager () ())

;;  interface ServantActivator : ServantManager {

(defclass PortableServer:ServantActivator (PortableServer:ServantManager) ())

;; Servant incarnate (in ObjectId oid, in POA adapter)
;;    raises (ForwardRequest);

(define-method incarnate ((s PortableServer:ServantActivator) oid adapter)
  ;; Raises ForwardRequest
  (declare (ignore oid adapter))
  nil)

(define-method etherealize ((s PortableServer:ServantActivator)
                            oid adapter servant cleanup-in-progress
                            reamining-activations)
  (declare (ignore oid adapter servant cleanup-in-progress 
                      reamining-activations))
  nil)

(defclass PortableServer:ServantLocator (PortableServer:ServantManager) ())

(deftype PortableServer:ServantLocator/cookie () t)

(define-method preinvoke ((s PortableServer:ServantManager) oid adapter operation)
  (declare (ignore oid adapter operation cookie))
  ;; result Servant, out cookie
  (values nil nil))

(define-method postinvoke ((s PortableServer:ServantManager)
                          oid adapter operation cookie servant)
  (declare (ignore oid adapter operation cookie servant))
  nil)



;;;; interface AdapterActivator

(DEFINE-INTERFACE PortableServer:ADAPTERACTIVATOR (OBJECT)
 :ID "IDL:omg.org/PortableServer/AdapterActivator:1.0"
 :NAME "AdapterActivator")

(DEFINE-METHOD "UNKNOWN_ADAPTER" ((OBJ PortableServer:ADAPTERACTIVATOR)
                                  _PARENT _NAME)
  (declare (ignore _PARENT _NAME)))


;;;; Portable Object Adaptor
;;; Class: POA

(define-corba-class PortableServer:POA ()
  :attributes ((the_name :readonly)
               (the_parent :readonly) 
               (the_POAManager :readonly) 
               (the_activator))
  :slots ((active-servant-map
           :initform (make-hash-table)
           :reader poa-active-servant-map
           :documentation "servant->id")
          (active-object-map 
           :initform (make-trie)
           :reader poa-active-object-map
           :documentation "id->servant")
          (servant-manager :accessor poa-servant-manager)
          (default-servant :accessor poa-default-servant)
          (policies :initarg :policies :accessor poa-policies)
          (poaid :initarg :poaid :accessor poa-poaid)
          (auto-id :accessor poa-auto-id :initform 0)
          (children :initform nil :accessor poa-children)
          (the-orb :accessor the-orb)))

(defmethod print-object ((p PortableServer:POA) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A ~D objects"
            (op:the_name p)
            (dict-count (POA-active-object-map p)))))

(defun poa-name (poa)
  (labels ((name-list (poa parent)
             (if (null parent)
                 nil
               (cons (op:the_name poa) 
                     (name-list parent (op:the_parent parent))))))
    (nreverse (name-list poa (op:the_parent poa)))))


;;;; PortableServer::Current

(defvar *poa-current* nil
  "The current invocation data for the PortableServer::Current object.")

(defun make-poa-current (poa oid) (cons poa oid))
(defun poa-current-poa (poa-current) (car poa-current))
(defun poa-current-object-id (poa-current) (cdr poa-current))


(DEFINE-INTERFACE PortableServer:Current (CORBA:Current)
 :ID "IDL:omg.org/PortableServer/Current:1.0"
 :NAME "Current")

(define-method get_POA ((current PortableServer::Current))
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (poa-current-POA *poa-current*))

(define-method get_object_id ((current PortableServer::Current))
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (poa-current-object-id *poa-current*))


;;; Convenience methods (from java)
;; assuming in context of POA call

(define-method _poa ((servant PortableServer:Servant)) 
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (poa-current-POA *poa-current*))

(define-method _orb ((servant PortableServer:Servant))
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (the-orb (poa-current-POA *poa-current*)))

(define-method _object_id ((servant PortableServer:Servant))
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (poa-current-object-id *poa-current*))



;;;; POA Registry

(defvar *root-POA* nil)

(defvar *last-poaid* 0)

(defvar *poa-map*
  #+cmu (make-hash-table :test #'eql  :weak-p t)
  #+allegro (make-hash-table :test #'eql  :values :weak)
  #-(or cmu allegro) (make-hash-table :test #'eql ))

(defun register-poa (poa)
  (setf (gethash (poa-poaid poa) *poa-map*) poa))

(defun unregister-poa (poa)
  (remhash (poa-poaid poa) *poa-map*))

(defun decode-object-key-poa (objkey)
  (multiple-value-bind (type poaid oid)
      (decode-object-key objkey)
    (let (poa)
      (if (numberp poaid)
          (setq poa (gethash poaid *poa-map*))
          (progn
            (setq poa *root-POA*)
            (handler-case
                (loop for n in poaid
                      do (setq poa (op:find_poa poa n t)))
              (omg.org/PortableServer:poa/adapternonexistent ()
               (setq poa nil)))))
      (values type poa oid))))


;;;; Create, find and destroy 

(defun create-POA (poa name manager policies)
  (let ((policy-groups
         '((:retain :non-retain)
           (:transient :persistent)
           (:system-id :user-id)
           (:unique-id :multiple-id)
           (:use-active-object-map-only :use-default-servant
            :use-servant-manager)
           (:implicit-activation :no-implicit-activation))))
    (loop for p in policies
        for i from 0
        for g = (find p policy-groups :test #'member)
        do (cond (g (setq policy-groups (remove g policy-groups)))
                 (t (error 'PortableServer:POA/InvalidPolicy :index i))))
    (loop for g in policy-groups
        do (push (car g) policies)))
  (when (and poa (find name (POA-children poa)
                       :key #'op:the_name :test #'equal))
    (error 'PortableServer:poa/adapteralreadyexists))
  (let ((newpoa
         (make-instance 'PortableServer:poa
          :the_name name
          :the_parent poa
          :the_POAmanager (or manager (make-instance 'PortableServer:poamanager))
          :policies policies
          :poaid (incf *last-poaid*))))
    (when poa
      (push newpoa (POA-children poa))
      (setf (the-orb newpoa) (the-orb poa)))
    (register-poa newpoa)
    newpoa))


;; POA create_POA(in string adapter_name,
;;   in POAManager a_POAManager,
;;   in CORBA::PolicyList policies)
;; raises (AdapterAlreadyExists, InvalidPolicy);

(define-method create_POA ((poa PortableServer:POA) adapter-name poamanager policies)
  (create-POA poa adapter-name poamanager policies))


(define-method find_POA ((poa PortableServer:POA) name &optional activate-it)
  (or (find name (POA-children poa)
            :key #'op:the_name :test #'equal)
      (cond ((and activate-it (op:the_activator poa))
             (funcall (op:the_activator poa) poa name)
             (find name (POA-children poa)
                   :key #'op:the_name :test #'equal)))
      (error 'PortableServer:poa/adapternonexistent)))


(defun poa-has-policy (poa policy)
  (member policy (POA-policies poa)))


;;     void destroy(	in boolean etherealize_objects,
;;  		        in boolean wait_for_completion);

(define-method destroy ((poa PortableServer:POA) etherealize-objects wait-for-completion)
  (declare (ignore wait-for-completion))
  ;; FIXME: what about the children?
  (let ((parent (op:the_parent poa)))
    (setf (POA-children parent)
          (delete poa (POA-children parent))))
  (unregister-poa poa)
  (when (and etherealize-objects
             (poa-has-policy poa :retain)
             (poa-has-policy poa :use-servant-manager))
    (maptrie (lambda (oid servant)
               (op:etherealize
                (POA-servant-manager poa) 
                oid poa servant t nil))
             (POA-active-object-map poa))))


;;;; some setters and getters

(defun check-policy (poa policy)
  (unless (poa-has-policy poa policy)
    (error 'PortableServer:poa/wrongpolicy)))

;;;  ServantManager get_servant_manager()
;;;    raises (WrongPolicy);

(define-method get_servant_manager ((poa PortableServer:POA))
  (check-policy poa :use-servant-manager)
  (poa-servant-manager poa))

;;;  void set_servant_manager( in ServantManager imgr)
;;;    raises (WrongPolicy);

(define-method set_servant_manager ((poa PortableServer:POA) imgr)
  (check-policy poa :use-servant-manager)
  (setf (poa-servant-manager poa) imgr))

;;;  Servant get_servant()
;;;    raises (NoServant, WrongPolicy);

(define-method get_servant ((poa PortableServer:POA))
  (check-policy poa :use-default-servant)
  (poa-default-servant poa))

;;;  void set_servant(	in Servant p_servant)
;;;    raises (WrongPolicy);

(define-method set_servant ((poa PortableServer:POA) servant)
  (check-policy poa :use-default-servant)
  (setf (poa-default-servant poa) servant))


;; ------------------------------------------------------------------
;;;; Object Activation and Deactivation
;; ------------------------------------------------------------------

(defun generate-id (poa)
  (check-policy poa :system-id)
  (if (poa-has-policy poa :persistent)
      (to-object-id (princ-to-string (get-internal-real-time))) 
    (to-object-id (incf (POA-auto-id poa)))))

(define-method activate_object ((poa PortableServer:POA) servant)
  (op:activate_object_with_id poa (generate-id poa) servant))

(define-method activate_object_with_id ((poa PortableServer:POA) id servant)
  (check-policy poa :retain)
  (setq id (to-object-id id))
  (trie-set id (POA-active-object-map poa) servant)
  (setf (gethash servant (POA-active-servant-map poa)) id))

(define-method deactivate_object ((poa PortableServer:POA) oid)
  (check-policy poa :retain)
  (setq oid (to-object-id oid))
  (let ((servant (trie-get oid (POA-active-object-map poa))))
    (trie-remove oid (POA-active-object-map poa))
    ;; FIXME: what about multiple-id policy
    (remhash servant (POA-active-servant-map poa))
    (when (poa-has-policy poa :use-servant-manager)
      (op:etherealize (POA-servant-manager poa) 
                       oid poa servant nil nil))))


;; ----------------------------------------------------------------------
;;;; Reference creation operations
;; ----------------------------------------------------------------------

;;   Object create_reference ( in CORBA::RepositoryId intf )
;;	raises (WrongPolicy);

(define-method create_reference ((poa PortableServer:POA) intf)
  (op:create_reference_with_id poa (generate-id poa) intf))

;;    Object create_reference_with_id ( in ObjectId oid,
;;				       in CORBA::RepositoryId intf )
;;	raises (WrongPolicy);

(define-method create_reference_with_id ((poa PortableServer:POA) oid intf)
  (make-instance (find-proxy-class intf)
   :id intf
   :profiles (list
              (make-iiop-profile
               :version '(1 . 0)
               :host (orb-host (the-orb poa))
               :port (orb-port (the-orb poa))
               :key (make-object-key (if (poa-has-policy poa :persistent)
                                       :persistent
                                       :transient)
                                     (poa-poaid poa) oid
                                     :poa-name (poa-name poa))))))


;; ----------------------------------------------------------------------
;;;; Identity Mapping Operations
;; ----------------------------------------------------------------------

;;;   ObjectId servant_to_id(in Servant p_servant)
;;;     raises (ServantNotActive, WrongPolicy);

(define-method servant_to_id ((poa PortableServer:POA) servant)
  (multiple-value-bind (id flag)
      (gethash servant (POA-active-servant-map poa))
    (if flag
	id
	(let ((id (generate-id poa)))
	  (op:activate_object_with_id poa id servant)
	  id))))

;;;   Object servant_to_reference(in Servant p_servant)
;;;     raises (ServantNotActive, WrongPolicy);

(define-method servant_to_reference ((poa PortableServer:POA) servant)
  (let ((oid (op:servant_to_id poa servant)))
    (op:create_reference_with_id poa oid (primary-interface servant oid poa))))

;;;   Servant reference_to_servant(in Object reference)
;;;     raises (ObjectNotActive, WrongAdapter, WrongPolicy);

(define-method reference_to_servant ((poa PortableServer:POA) reference)
  (op:id_to_servant poa (op:reference_to_id poa reference)))

;;;   ObjectId reference_to_id(in Object reference)
;;;     raises (WrongAdapter, WrongPolicy);

(define-method reference_to_id ((poa PortableServer:POA) reference)
  (let ((profiles (object-profiles reference)))
    (unless profiles
      (error 'PortableServer:poa/wrongadapter))
    (multiple-value-bind (ref-type refpoa oid)
                         (decode-object-key-poa (iiop-profile-key (first profiles)))
      (declare (ignore ref-type))
      (unless (eql refpoa poa)
        (error 'PortableServer:poa/wrongadapter))
      oid)))

;;;   Servant id_to_servant(in ObjectId oid)
;;;     raises (ObjectNotActive, WrongPolicy);

(define-method id_to_servant ((poa PortableServer:POA) id)
  (or (trie-get (to-object-id id) (POA-active-object-map poa))
      (error 'PortableServer:poa/objectnotactive)))

;;;   Object id_to_reference(in ObjectId oid)
;;;     raises (ObjectNotActive, WrongPolicy);

(define-method id_to_reference ((poa PortableServer:POA) oid)
  (op:servant_to_reference poa
                            (op:id_to_servant poa oid)))


;; ----------------------------------------------------------------------
;;;; Policy creation
;; ----------------------------------------------------------------------

(define-method "CREATE_REQUEST_PROCESSING_POLICY" ((OBJ PortableServer:POA) value)
  (op:create_policy (the-orb obj) PortableServer:REQUEST_PROCESSING_POLICY_ID value))

(define-method "CREATE_SERVANT_RETENTION_POLICY" ((OBJ PortableServer:POA) value)
  (op:create_policy (the-orb obj) PortableServer:SERVANT_RETENTION_POLICY_ID value))

(define-method "CREATE_IMPLICIT_ACTIVATION_POLICY" ((OBJ PortableServer:POA) value)
  (op:create_policy (the-orb obj) PortableServer:IMPLICIT_ACTIVATION_POLICY_ID value))

(define-method "CREATE_ID_ASSIGNMENT_POLICY" ((OBJ PortableServer:POA) value)
  (op:create_policy (the-orb obj) PortableServer:ID_ASSIGNMENT_POLICY_ID value))

(define-method "CREATE_ID_UNIQUENESS_POLICY" ((OBJ PortableServer:POA) value)
  (op:create_policy (the-orb obj) PortableServer:ID_UNIQUENESS_POLICY_ID value))

(define-method "CREATE_LIFESPAN_POLICY" ((OBJ PortableServer:POA) value)
  (op:create_policy (the-orb obj) PortableServer:LIFESPAN_POLICY_ID value))

(define-method "CREATE_THREAD_POLICY" ((OBJ PortableServer:POA) value)
  (op:create_policy (the-orb obj) PortableServer:THREAD_POLICY_ID value))


;; ----------------------------------------------------------------------
;;;; Servant methods depending on POA
;; ----------------------------------------------------------------------

(defun current-primary-interface (servant)
  (primary-interface servant 
                     (poa-current-object-id *poa-current*)
                     (poa-current-poa *poa-current*)))

(define-method _this ((servant PortableServer:servant))
  (if *poa-current*                     ; in context of a request
    (let ((oid (poa-current-object-id *poa-current*))
          (poa (poa-current-poa *poa-current*)))
      (op:create_reference_with_id
       poa oid (primary-interface servant oid poa)))
    (let* ((poa (or (op:_default_POA servant)
		    (root-POA) )))
      ;; FIXME: translate ServantNotActive to WrongPolicy ??
      (op:servant_to_reference poa servant))))

(define-method _is_a ((servant PortableServer:dynamicimplementation) logical-type-id)
  (or (equal logical-type-id (current-primary-interface servant))
      (op:is_a (op:_get_interface servant) logical-type-id)))

(define-method _get_interface ((servant PortableServer:servant))
  (handler-case
      (op:lookup_id (get-ir) (current-primary-interface servant))
    (error ()
      (error 'CORBA:intf_repos))))

;; ----------------------------------------------------------------------
;;;; Request Handling
;; ----------------------------------------------------------------------

(defun poa-invoke (poa oid operation buffer handler)
  (unless (eq :active (op:get_state (op:the_poamanager poa)))
    (error 'CORBA:TRANSIENT :completed :completed_no))
  (let* ((*poa-current* (make-poa-current poa oid))
         (servant (trie-get oid (POA-active-object-map poa)))
         (cookie nil)
         (topost nil))
    (handler-case    
      (progn
        (cond (servant)
              ((poa-has-policy poa :use-servant-manager)
               (cond ((poa-has-policy poa :retain)
                      (setq servant
                            (op:incarnate (POA-servant-manager poa) oid poa))
                      (op:activate_object_with_id poa oid servant))
                     (t
                      (multiple-value-setq (servant cookie)
                        (op:preinvoke (POA-servant-manager poa)
                                      oid poa operation))
                      (setq topost t))))
              ((poa-has-policy poa :use-default-servant)
               (setq servant (POA-default-servant poa)))
              (t
               (error 'CORBA:OBJECT_NOT_EXIST
                      :completed :completed_no)))
        (cond (topost
               (unwind-protect
                 (servant-invoke servant operation buffer handler)
                 (op:postinvoke (POA-servant-manager poa)
                                oid poa operation cookie servant)))
              (t
               (servant-invoke servant operation buffer handler))))
      (ForwardRequest 
       (fwd)
       (mess 3 "forwarding")
       (let ((buffer (funcall handler :location_forward)))
         (marshal-ior (op:forward_reference fwd) buffer)
         buffer)))))



;;;; Policy Implementation objects

(defclass policy-value-mixin (policy-impl)
  ((value :initarg :value)))

(define-method value ((obj policy-value-mixin))
  (slot-value obj 'value))

(defclass requestprocessingpolicy-impl (PortableServer:REQUESTPROCESSINGPOLICY policy-value-mixin)
  ())

(defmethod create-policy ((type (eql PortableServer:REQUEST_PROCESSING_POLICY_ID)) val)
  (unless (typep val 'PortableServer:REQUESTPROCESSINGPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'requestprocessingpolicy-impl :policy_type type :value val))


(defclass servantretentionpolicy-impl (PortableServer:SERVANTRETENTIONPOLICY policy-value-mixin)
  ())

(defmethod create-policy ((type (eql PortableServer:SERVANT_RETENTION_POLICY_ID)) val)
  (unless (typep val 'PortableServer:SERVANTRETENTIONPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'servantretentionpolicy-impl :policy_type type :value val))


(defclass implicitactivationpolicy-impl (PortableServer:IMPLICITACTIVATIONPOLICY policy-value-mixin)
  ())

(defmethod create-policy ((type (eql PortableServer:IMPLICIT_ACTIVATION_POLICY_ID)) val)
  (unless (typep val 'PortableServer:IMPLICITACTIVATIONPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'implicitactivationpolicy-impl :policy_type type :value val))


(defclass idassignmentpolicy-impl (PortableServer:IDASSIGNMENTPOLICY policy-value-mixin)
  ())

(defmethod create-policy ((type (eql PortableServer:ID_ASSIGNMENT_POLICY_ID)) val)
  (unless (typep val 'PortableServer:IDASSIGNMENTPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'idassignmentpolicy-impl :policy_type type :value val))


(defclass iduniquenesspolicy-impl (PortableServer:IDUNIQUENESSPOLICY policy-value-mixin)
  ())

(defmethod create-policy ((type (eql PortableServer:ID_UNIQUENESS_POLICY_ID)) val)
  (unless (typep val 'PortableServer:IDUNIQUENESSPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'iduniquenesspolicy-impl :policy_type type :value val))

(defclass lifespanpolicy-impl (PortableServer:LIFESPANPOLICY policy-value-mixin)
  ())

(defmethod create-policy ((type (eql PortableServer:LIFESPAN_POLICY_ID)) val)
  (unless (typep val 'PortableServer:LIFESPANPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'lifespanpolicy-impl :policy_type type :value val))

(defclass threadpolicy-impl (PortableServer:THREADPOLICY policy-value-mixin)
  ())

(defmethod create-policy ((type (eql PortableServer:THREAD_POLICY_ID)) val)
  (unless (typep val 'PortableServer:THREADPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'threadpolicy-impl :policy_type type :value val))


;;; clorb-poa.lisp ends here
