;;;; clorb-poa.lisp -- Portable Object Adaptor
;; $Id: clorb-poa.lisp,v 1.22 2003/12/08 23:18:45 lenst Exp $

(in-package :clorb)


;;;; Servant manager

(defclass PORTABLESERVER:SERVANTMANAGER () ())

;;  interface ServantActivator : ServantManager {

(defclass PORTABLESERVER:SERVANTACTIVATOR (portableserver:servantmanager) ())

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

(defclass PORTABLESERVER:SERVANTLOCATOR (portableserver:servantmanager) ())

(deftype PortableServer:ServantLocator/cookie () t)

(define-method preinvoke ((s PortableServer:ServantManager) oid adapter operation)
  (declare (ignore oid adapter operation cookie))
  ;; result Servant, out cookie
  (values nil nil))

(define-method postinvoke ((s PortableServer:ServantManager)
                          oid adapter operation cookie servant)
  (declare (ignore oid adapter operation cookie servant))
  nil)



;;;; Interface AdapterActivator

(DEFINE-INTERFACE PortableServer:AdapterActivator (OBJECT)
 :id "IDL:omg.org/PortableServer/AdapterActivator:1.0"
 :name "AdapterActivator")

(DEFINE-METHOD "UNKNOWN_ADAPTER" ((OBJ PortableServer:ADAPTERACTIVATOR)
                                  _PARENT _NAME)
  (declare (ignore _PARENT _NAME)))


;;;; PortableServer::POA

(define-corba-class PortableServer:POA ()
  :attributes ((the_name :readonly)
               (the_parent :readonly) 
               (the_POAManager :readonly) 
               (the_activator nil)
               (the_children nil :readonly))
  :slots ((active-servant-map
           :initform (make-hash-table)
           :reader poa-active-servant-map
           :documentation "servant->id")
          (active-object-map 
           :initform (make-trie)
           :reader poa-active-object-map
           :documentation "id->servant")
          (servant-manager :initform nil :accessor poa-servant-manager)
          (default-servant :accessor poa-default-servant)
          (policies :initarg :policies :accessor poa-policies)
          (poaid :initarg :poaid :accessor poa-poaid)
          (auto-id :accessor poa-auto-id :initform 0)
          (the-orb :initarg :orb :accessor the-orb)
          (state :initform nil :accessor poa-state)))

(defmethod print-object ((p portableserver:poa) stream)
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

(defun poa-effective-state (poa)
  ;; Combine state from POAManager and other operations on the POA.
  (or (poa-state poa)
      (op:get_state (op:the_poamanager poa))))

;;;; PortableServer::Current

(defvar *poa-current* nil
  "The current invocation data for the PortableServer::Current object.")

(defun make-poa-current (poa oid servant) (list* poa oid servant))
(defun poa-current-poa (poa-current) (car poa-current))
(defun poa-current-object-id (poa-current) (cadr poa-current))
(defun poa-current-servant (poa-current) (cddr poa-current))

(DEFINE-INTERFACE PortableServer:Current (CORBA:Current)
 :id "IDL:omg.org/PortableServer/Current:1.0"
 :name "Current")

(define-method get_POA ((current PortableServer::Current))
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (poa-current-POA *poa-current*))

(define-method get_object_id ((current PortableServer::Current))
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (poa-current-object-id *poa-current*))

;;++ CORBA 2.6:
;; Object get_reference
;; Servant get_servant
;;--

(define-method get_servant ((current PortableServer::Current))
  (unless *poa-current* (error 'PortableServer:Current/NoContext))
  (poa-current-servant *poa-current*))


;;;; Convenience methods on servants (from java)
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


;;;; interface POAManager

(defclass PORTABLESERVER:POAMANAGER (corba:object)
  ((state :initform :holding)))


;;; enum State {HOLDING, ACTIVE, DISCARDING, INACTIVE}
(DEFINE-ENUM OMG.ORG/PORTABLESERVER:POAMANAGER/STATE
 :id "IDL:omg.org/PortableServer/POAManager/State:1.0"
 :name "State"
 :members ("HOLDING" "ACTIVE" "DISCARDING" "INACTIVE"))


;;; exception AdapterInactive{};
(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POAMANAGER/ADAPTERINACTIVE
 :id "IDL:omg.org/PortableServer/POAManager/AdapterInactive:1.0"
 :name "AdapterInactive"
 :members NIL)


(defun poamanager-new-state (pm new-state)
  (with-slots (state) pm
    (when (eq state :inactive)
      (error 'POAManager/AdapterInactive))
    (setf state new-state)))


;;; void activate()
;;;	raises(AdapterInactive);
(define-method activate ((pm PortableServer:POAManager))
  (POAManager-new-state pm :active))


;;; void hold_requests(in boolean wait_for_completion)
;;;     raises(AdapterInactive);
(define-method hold_requests ((pm PortableServer:POAManager) wait_for_completion)
  (when wait_for_completion
    ;;(check-not-processing-request )
    (when *poa-current*
      ;; FIXME: check same ORB 
      (error 'corba:bad_inv_order :minor 3)))
  (POAManager-new-state pm :holding))


;;; void discard_requests(in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method discard_requests ((pm PortableServer:POAManager) wait_for_completion)
  (when wait_for_completion
    (when *poa-current*
      (error 'corba:bad_inv_order :minor 3)))
  (POAManager-new-state pm :discarding))


;;; void deactivate(	in boolean etherealize_objects,
;;;                     in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method deactivate ((pm PortableServer:POAManager) etherealize_objects 
                           wait_for_completion)
  (POAManager-new-state pm :inactive))


;;; State get_state ()
(define-method get_state ((pm PortableServer:POAManager))
  (slot-value pm 'state))




;;;; POA Registry

(defvar *root-poa* nil)

(defvar *last-poaid* 0)

(defvar *poa-map*
  (make-hash-table :test #'eql))

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
            (loop for n in poaid 
                  while poa
                  do (setq poa (find-requested-poa poa n t t)))))
      (values type poa oid))))


;;;; Create, find and destroy 

(defun canonical-policy-list (policies)
  (setq policies
        (loop for p in policies 
              for i from 0
              collect (typecase p
                        (symbol p)
                        (CORBA:Policy (op:value p))
                        (t (error 'portableserver:poa/invalidpolicy :index i)))))
  (let ((policy-groups
         '((:retain :non-retain)
           (:transient :persistent)
           (:system_id :user_id)
           (:unique_id :multiple_id)
           (:use_active_object_map_only :use_default_servant :use_servant_manager)
           (:implicit_activation :no_implicit_activation)
           (:orb_ctrl_model :single_thread_model))))
    (loop for p in policies
          for i from 0
          for g = (find p policy-groups :test #'member)
          do (cond (g (setq policy-groups (remove g policy-groups)))
                   (t (error 'PortableServer:POA/InvalidPolicy :index i))))
    (loop for g in policy-groups
          do (push (car g) policies))
    policies))


(defun create-poa (poa name manager policies orb
                       &key poaid )
  (setq policies (canonical-policy-list policies))
  (when (and poa (find name (op:the_children poa)
                       :key #'op:the_name :test #'equal))
    (error 'PortableServer:POA/AdapterAlreadyExists))
  (let ((newpoa
         (make-instance 'PortableServer:POA
           :the_name name
           :the_parent poa
           :the_poamanager (or manager (make-instance 'PortableServer:poamanager))
           :policies policies
           :poaid (or poaid (incf *last-poaid*))
           :orb orb)))
    (when poa
      (push newpoa (slot-value poa 'the_children)))
    (register-poa newpoa)
    newpoa))


;; POA create_POA(in string adapter_name,
;;   in POAManager a_POAManager,
;;   in CORBA::PolicyList policies)
;; raises (AdapterAlreadyExists, InvalidPolicy);

(define-method create_POA ((poa PortableServer:POA) adapter-name poamanager policies)
  (when (poa-state poa)
    ;; Currently only have state when destroying
    (error 'CORBA:BAD_INV_ORDER :minor 17))
  (create-POA poa adapter-name poamanager policies (the-orb poa)))

(defun find-requested-poa (poa name activate-it check-poa-status)
  (flet ((find-child ()
           (find name (op:the_children poa) :key #'op:the_name :test #'equal)))
    (when check-poa-status
      (when (or (not (eql :active (poa-effective-state poa))))
        (error 'CORBA:TRANSIENT)))
    (or (find-child)
        (and activate-it
             (op:the_activator poa)
             (handler-case
               (op:unknown_adapter (op:the_activator poa) poa name)
               (CORBA:SystemException () (error 'CORBA:OBJ_ADAPTER :minor 1 )))
             (find-child)))))

(define-method find_POA ((poa PortableServer:POA) name &optional activate-it)
  (or (find-requested-poa poa name activate-it nil) 
      (error 'PortableServer:POA/AdapterNonexistent)))

(defun poa-has-policy (poa policy)
  (member policy (POA-policies poa)))


;;;    void destroy(	in boolean etherealize_objects,
;;  		        in boolean wait_for_completion);
#| 
This operation destroys the POA and all descendant POAs. All descendant
POAs are destroyed (recursively) before the destruction of the containing
POA. The POA so destroyed (that is, the POA with its name) may be
re-created later in the same process. (This differs from the
POAManager::deactivate operation that does not allow a recreation of its
associated POA in the same process. After a deactivate, re-creation is
allowed only if the POA is later destroyed.) 

When destroy is called the POA behaves as follows: 

...

¥ The POA calls destroy on all of its immediate descendants.

¥ After all descendant POAs have been destroyed and their servants
etherealized, the POA continues to process requests until there are no
requests executing in the POA. At this point, apparent destruction of the
POA has occurred.

¥ After destruction has become apparent, the POA may be re-created via
either an AdapterActivator or a call to create_POA.

¥ If the etherealize_objects parameter is TRUE, the POA has the RETAIN
policy, and a servant manager is registered with the POA, the etherealize
operation on the servant manager is called for each active object in the
Active Object Map. The apparent destruction of the POA occurs before any
calls to etherealize are made. Thus, for example, an etherealize method
that attempts to invoke operations on the POA receives the
OBJECT_NOT_EXIST exception.

¥ If the POA has an AdapterActivator installed, any requests that would
have caused unknown_adapter to be called cause a TRANSIENT exception with
standard minor code 4 to be raised instead.

The wait_for_completion parameter is handled as follows:
...
¥ If wait_for_completion is FALSE, the destroy operation destroys the POA and
its children but waits neither for active requests to complete nor for etherealization
to occur. If destroy is called multiple times before destruction is complete
(because there are active requests), the etherealize_objects parameter applies
only to the first call of destroy. Subsequent calls with conflicting
etherealize_objects settings use the value of etherealize_objects from the first
call. The wait_for_completion parameter is handled as defined above for each
individual call (some callers may choose to block, while others may not).

|#

(define-method destroy ((poa PortableServer:POA) etherealize-objects wait-for-completion)
#|
¥ If wait_for_completion is TRUE and the current thread is in an invocation
context dispatched from some POA belonging to the same ORB as this POA, the
BAD_INV_ORDER system exception with standard minor code 3 is raised and
POA destruction does not occur.
|#
  (when (and wait-for-completion *poa-current*)
    (error 'CORBA:BAD_INV_ORDER :minor 3 :completed :completed_yes))
  (unless (eq :inactive (poa-effective-state poa))
    (setf (poa-state poa) :discarding))
  (dolist (child (op:the_children poa))
    (op:destroy child etherealize-objects wait-for-completion))
  ;; wait for ongoing requests to finnish,
  ;; shouldn't be any as long as we are singel threaded or in recursive call
  (let ((parent (op:the_parent poa)))
    (setf (slot-value parent 'the_children)
          (delete poa (op:the_children parent))))
  (unregister-poa poa)
  (when (and etherealize-objects
             (poa-has-policy poa :retain)
             (poa-has-policy poa :use_servant_manager)
             (POA-servant-manager poa))
    (maptrie (lambda (oid servant)
               (op:etherealize (POA-servant-manager poa) 
                               oid poa servant t nil))
             (POA-active-object-map poa))))


;;;; Some setters and getters

(defun check-policy (poa policy)
  (unless (poa-has-policy poa policy)
    (error 'PortableServer:poa/wrongpolicy)))

;;;  ServantManager get_servant_manager()
;;;    raises (WrongPolicy);

(define-method get_servant_manager ((poa PortableServer:POA))
  (check-policy poa :use_servant_manager)
  (poa-servant-manager poa))

;;;  void set_servant_manager( in ServantManager imgr)
;;;    raises (WrongPolicy);

(define-method set_servant_manager ((poa PortableServer:POA) imgr)
  (check-policy poa :use_servant_manager)
  (unless (typep imgr (if (poa-has-policy poa :retain)
                        'PortableServer:ServantActivator
                        'PortableServer:ServantLocator))
    (error 'CORBA:OBJ_ADAPTER :minor 4))
  (when (poa-servant-manager poa)
    (error 'CORBA:BAD_INV_ORDER :minor 6))
  (setf (poa-servant-manager poa) imgr))

;;;  Servant get_servant()
;;;    raises (NoServant, WrongPolicy);

(define-method get_servant ((poa PortableServer:POA))
  (check-policy poa :use_default_servant)
  (unless (slot-boundp poa 'default-servant)
    (error 'omg.org/portableserver:poa/noservant))
  (poa-default-servant poa))

;;;  void set_servant(	in Servant p_servant)
;;;    raises (WrongPolicy);

(define-method set_servant ((poa PortableServer:POA) servant)
  (check-policy poa :use_default_servant)
  (setf (poa-default-servant poa) servant))


;; ------------------------------------------------------------------
;;;; Object Activation and Deactivation
;; ------------------------------------------------------------------

(defun generate-id (poa)
  (check-policy poa :system_id)
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
    (when (poa-has-policy poa :use_servant_manager)
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
  (if (and *poa-current*                     ; in context of a request
           (eq servant (poa-current-servant *poa-current*)))
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
      (op:lookup_id *internal-interface-repository*
                    (current-primary-interface servant))
    (error (condition)
      (break "_get_interface: ~A" condition)
      (error 'CORBA:intf_repos))))

;; ----------------------------------------------------------------------
;;;; Request Handling
;; ----------------------------------------------------------------------

(defun poa-invoke (poa oid operation buffer handler)
  (unless (eq :active (poa-effective-state poa))
    (error 'CORBA:TRANSIENT :completed :completed_no))
  (let* ((servant (trie-get oid (POA-active-object-map poa)))
         (cookie nil)
         (topost nil))
    (handler-case    
      (progn
        (cond (servant)
              ((poa-has-policy poa :use_servant_manager)
               (cond ((poa-has-policy poa :retain)
                      (setq servant
                            (op:incarnate (POA-servant-manager poa) oid poa))
                      (mess 2 "~A incarnated ~A for '~/clorb:stroid/'"
                            poa servant oid)
                      (op:activate_object_with_id poa oid servant))
                     (t
                      (multiple-value-setq (servant cookie)
                        (op:preinvoke (POA-servant-manager poa)
                                      oid poa operation))
                      (setq topost t))))
              ((poa-has-policy poa :use_default_servant)
               (setq servant (POA-default-servant poa)))
              (t
               (error 'CORBA:OBJECT_NOT_EXIST
                      :completed :completed_no)))
        (let ((*poa-current* (make-poa-current poa oid servant)))
          (cond (topost
                 (unwind-protect
                   (servant-invoke servant operation buffer handler)
                   (op:postinvoke (POA-servant-manager poa)
                                  oid poa operation cookie servant)))
                (t
                 (servant-invoke servant operation buffer handler)))))
      (PortableServer:ForwardRequest
       (fwd)
       (mess 3 "forwarding to ~A" (op:forward_reference fwd))
       (let ((buffer (funcall handler :location_forward)))
         (marshal-object (op:forward_reference fwd) buffer)
         buffer)))))



;;;; Policy implementation objects

(defclass POLICY-VALUE-MIXIN (policy-impl)
  ((value :initarg :value)))

(define-method value ((obj policy-value-mixin))
  (slot-value obj 'value))

(defclass REQUESTPROCESSINGPOLICY-IMPL (portableserver:requestprocessingpolicy policy-value-mixin)
  ())

(defmethod create-policy ((type (eql portableserver:request_processing_policy_id)) val)
  (unless (typep val 'PortableServer:REQUESTPROCESSINGPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'requestprocessingpolicy-impl :policy_type type :value val))


(defclass SERVANTRETENTIONPOLICY-IMPL (portableserver:servantretentionpolicy policy-value-mixin)
  ())

(defmethod create-policy ((type (eql portableserver:servant_retention_policy_id)) val)
  (unless (typep val 'PortableServer:SERVANTRETENTIONPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'servantretentionpolicy-impl :policy_type type :value val))


(defclass IMPLICITACTIVATIONPOLICY-IMPL (portableserver:implicitactivationpolicy policy-value-mixin)
  ())

(defmethod create-policy ((type (eql portableserver:implicit_activation_policy_id)) val)
  (unless (typep val 'PortableServer:IMPLICITACTIVATIONPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'implicitactivationpolicy-impl :policy_type type :value val))


(defclass IDASSIGNMENTPOLICY-IMPL (portableserver:idassignmentpolicy policy-value-mixin)
  ())

(defmethod create-policy ((type (eql portableserver:id_assignment_policy_id)) val)
  (unless (typep val 'PortableServer:IDASSIGNMENTPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'idassignmentpolicy-impl :policy_type type :value val))


(defclass IDUNIQUENESSPOLICY-IMPL (portableserver:iduniquenesspolicy policy-value-mixin)
  ())

(defmethod create-policy ((type (eql portableserver:id_uniqueness_policy_id)) val)
  (unless (typep val 'PortableServer:IDUNIQUENESSPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'iduniquenesspolicy-impl :policy_type type :value val))

(defclass LIFESPANPOLICY-IMPL (portableserver:lifespanpolicy policy-value-mixin)
  ())

(defmethod create-policy ((type (eql portableserver:lifespan_policy_id)) val)
  (unless (typep val 'PortableServer:LIFESPANPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'lifespanpolicy-impl :policy_type type :value val))

(defclass THREADPOLICY-IMPL (portableserver:threadpolicy policy-value-mixin)
  ())

(defmethod create-policy ((type (eql portableserver:thread_policy_id)) val)
  (unless (typep val 'PortableServer:THREADPOLICYVALUE)
    (error (CORBA:policyerror :reason CORBA:bad_policy_type)))
  (make-instance 'threadpolicy-impl :policy_type type :value val))


;;; clorb-poa.lisp ends here
