;;;; clorb-poa.lisp -- Portable Object Adaptor
;; $Id: clorb-poa.lisp,v 1.35 2005/02/06 22:34:10 lenst Exp $

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
#|
(active-servant-map
           :initform (make-hash-table)
           :reader poa-active-servant-map
           :documentation "servant->id")
:reader poa-active-object-map
|#
(define-corba-class PortableServer:POA ()
  :attributes ((the_name :readonly)
               (the_parent :readonly) 
               (the_POAManager :readonly) 
               (the_activator nil)
               (the_children nil :readonly))
  :slots ((active-object-map 
           :initform (make-instance 'object-map)
           :reader active-object-map)
          (servant-manager :initform nil :accessor poa-servant-manager)
          (default-servant :accessor poa-default-servant)
          (policies :initarg :policies :accessor poa-policies)
          (poaid :initarg :poaid :accessor poa-poaid)
          (auto-id :accessor poa-auto-id :initform 0)
          (the-orb :initarg :orb :accessor the-orb)
          (state :initform nil :accessor poa-state)
          (destroy-in-progres-p :initform nil :accessor destroy-in-progres-p)
          (queue :initform nil :accessor poa-request-queue)))

(defmethod print-object ((p portableserver:poa) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A ~D objects"
            (op:the_name p)
            (activation-count (active-object-map p)))))

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


(defmethod wait-for-completion ((poa portableserver:poa))
  ;; Currently we have no way to do this and as long as we are single
  ;; threaded it should not be much of an issue.
  t)


(defmethod poa-new-state ((poa portableserver:poa) new-state)
  (setf (poa-state poa) new-state)
  (case new-state
    ((:active) 
     (loop (multiple-value-bind (req found) (deqf (poa-request-queue poa))
             (unless found (return))
             (poa-dispatch poa req))))
    ((:discarding)
     (loop (multiple-value-bind (req found) (deqf (poa-request-queue poa))
             (unless found (return))
             (discard-request req))))))



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
  ((state         :initform :holding)
   (managed-poas  :initform '()
                  :accessor managed-poas)))


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


(defun in-invocation-context (pm)
  "True if in an invocation context for some POA in the same ORB as PM"
  (when *poa-current*
    (let ((pm-poas (managed-poas pm)))
      (when pm-poas
        (eql (the-orb (poa-current-poa *poa-current*))
             (the-orb (car pm-poas)))))))


(defun poamanager-new-state (pm new-state wait-for-completion
                                    &optional etheralize-objects)
  (when wait-for-completion 
    (when (in-invocation-context pm)
      (raise-system-exception 'CORBA:bad_inv_order 3)))
  (with-slots (state) pm
    (when (eq state :inactive)
      (unless (eq new-state :inactive)
        (error 'POAManager/AdapterInactive)))
    (let ((old-state state))
      (setf state new-state)
      (unless (eql new-state old-state)
        (dolist (poa (managed-poas pm))
          (poa-new-state poa new-state)
          (when etheralize-objects
            (start-etheralize poa))))))
  (when wait-for-completion
    (dolist (poa (managed-poas pm))
      (wait-for-completion poa))))


(defmethod add-poa ((pm PortableServer:POAManager) poa)
  (push poa (managed-poas pm)))

(defmethod remove-poa ((pm PortableServer:POAManager) poa)
  (setf (managed-poas pm) (delete poa (managed-poas pm))))


;;; void activate()
;;;	raises(AdapterInactive);
(define-method activate ((pm PortableServer:POAManager))
  (POAManager-new-state pm :active nil))


;;; void hold_requests(in boolean wait_for_completion)
;;;     raises(AdapterInactive);
(define-method hold_requests ((pm PortableServer:POAManager) wait-for-completion)
  (POAManager-new-state pm :holding wait-for-completion))


;;; void discard_requests(in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method discard_requests ((pm PortableServer:POAManager) wait-for-completion)
  (POAManager-new-state pm :discarding wait-for-completion))



;;; void deactivate(	in boolean etherealize_objects,
;;;                     in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method deactivate ((pm PortableServer:POAManager) etherealize_objects 
                           wait_for_completion)
  (POAManager-new-state pm :inactive wait_for_completion etherealize_objects))


;;; State get_state ()
(define-method get_state ((pm PortableServer:POAManager))
  (slot-value pm 'state))


;;; Printing poamanager
(defmethod print-object ((pm PortableServer:POAManager) stream)
  (print-unreadable-object (pm stream :type t :identity t)
    (format stream "~S ~D" (slot-value pm 'state)
            (length (managed-poas pm)))))



;;;; POA Registry

(defvar *root-poa* nil)

(defvar *last-poaid* 0)

(defvar *poa-map*
  (make-hash-table :test #'eql))

(defun register-poa (poa)
  (setf (gethash (poa-poaid poa) *poa-map*) poa))

(defun unregister-poa (poa)
  (remhash (poa-poaid poa) *poa-map*))

#+unused-function
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
  (setq manager (or manager (make-instance 'PortableServer:poamanager)))
  (let ((newpoa
         (make-instance 'PortableServer:POA
           :the_name name
           :the_parent poa
           :the_poamanager manager
           :policies policies
           :poaid (or poaid (incf *last-poaid*))
           :orb orb)))
    (add-poa manager newpoa)
    (when poa
      (push newpoa (slot-value poa 'the_children)))
    (register-poa newpoa)
    newpoa))


;; POA create_POA(in string adapter_name,
;;   in POAManager a_POAManager,
;;   in CORBA::PolicyList policies)
;; raises (AdapterAlreadyExists, InvalidPolicy);

(define-method create_POA ((poa PortableServer:POA) adapter-name poamanager policies)
  (when (destroy-in-progres-p poa)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 17))
  (create-POA poa adapter-name poamanager policies (the-orb poa)))


(defun find-requested-poa (poa name activate-it check-poa-status)
  (flet ((find-child ()
           (find name (op:the_children poa) :key #'op:the_name :test #'equal)))
    (or (find-child)
        (and activate-it
             (op:the_activator poa)
             ;; if no activator OBJECT_NOT_EXIST systemexceptionwithstandardminorcode2. 
             (handler-case
               (if (or (not check-poa-status)
                       (eql :active (poa-effective-state poa)))
                       (and (op:unknown_adapter (op:the_activator poa) poa name)
                            (find-child))
                       :wait )
               (CORBA:SystemException () (raise-system-exception 'CORBA:OBJ_ADAPTER 1 )))))))


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

* The POA calls destroy on all of its immediate descendants.

* After all descendant POAs have been destroyed and their servants
etherealized, the POA continues to process requests until there are no
requests executing in the POA. At this point, apparent destruction of the
POA has occurred.

* After destruction has become apparent, the POA may be re-created via
either an AdapterActivator or a call to create_POA.

* If the etherealize_objects parameter is TRUE, the POA has the RETAIN
policy, and a servant manager is registered with the POA, the etherealize
operation on the servant manager is called for each active object in the
Active Object Map. The apparent destruction of the POA occurs before any
calls to etherealize are made. Thus, for example, an etherealize method
that attempts to invoke operations on the POA receives the
OBJECT_NOT_EXIST exception.

* If the POA has an AdapterActivator installed, any requests that would
have caused unknown_adapter to be called cause a TRANSIENT exception with
standard minor code 4 to be raised instead.

The wait_for_completion parameter is handled as follows:
...
* If wait_for_completion is FALSE, the destroy operation destroys the POA and
its children but waits neither for active requests to complete nor for etherealization
to occur. If destroy is called multiple times before destruction is complete
(because there are active requests), the etherealize_objects parameter applies
only to the first call of destroy. Subsequent calls with conflicting
etherealize_objects settings use the value of etherealize_objects from the first
call. The wait_for_completion parameter is handled as defined above for each
individual call (some callers may choose to block, while others may not).

|#


(defun start-etheralize (poa)
  (when (and (poa-has-policy poa :retain)
             (poa-has-policy poa :use_servant_manager)
             (POA-servant-manager poa))
    (map-activations 
     (active-object-map poa)
     (lambda (oid servant)
       (op:etherealize (POA-servant-manager poa) 
                       oid poa servant t 
                       (not (null (servant-active-p (active-object-map poa)
                                                    servant))))))))


(define-method destroy ((poa PortableServer:POA) etherealize-objects wait-for-completion)
#|
* If wait_for_completion is TRUE and the current thread is in an invocation
context dispatched from some POA belonging to the same ORB as this POA, the
BAD_INV_ORDER system exception with standard minor code 3 is raised and
POA destruction does not occur.
|#
  (when (and wait-for-completion *poa-current*)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 3 :completed_yes))
  (unless (eq :inactive (poa-effective-state poa))
    (poa-new-state poa :discarding))
  (setf (destroy-in-progres-p poa) t)
  (let ((manager (op:the_poamanager poa)))
    (remove-poa manager poa))
  (dolist (child (op:the_children poa))
    (op:destroy child etherealize-objects wait-for-completion))
  ;; wait for ongoing requests to finnish,
  ;; shouldn't be any as long as we are singel threaded or in recursive call
  (let ((parent (op:the_parent poa)))
    (setf (slot-value parent 'the_children)
          (delete poa (op:the_children parent))))
  (unregister-poa poa)
  (when etherealize-objects
    (start-etheralize poa)))




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
    (raise-system-exception 'CORBA:OBJ_ADAPTER 4))
  (when (poa-servant-manager poa)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 6))
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
  (when (oid-servant (active-object-map poa) id)
    (error (portableserver:poa/objectalreadyactive)))
  (unless (poa-has-policy poa :multiple_id)
    (when (servant-active-p (active-object-map poa) servant)
      (error (portableserver:poa/servantalreadyactive))))
  (add-activation (active-object-map poa) id servant)
  id)

(define-method deactivate_object ((poa PortableServer:POA) oid)
  (check-policy poa :retain)
  (setq oid (to-object-id oid))
  (let ((activation (remove-activation (active-object-map poa) oid)))
    (unless activation
      (error (portableserver:poa/ObjectNotActive)))
    (let ((servant (cdr activation)))
      (when (poa-has-policy poa :use_servant_manager)
        (op:etherealize (POA-servant-manager poa) 
                        oid poa servant nil
                        (servant-active-p (active-object-map poa) servant))))))


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
  (check-type intf string)
  (create-objref
   (the-orb poa)
   :ior-id intf 
   :profiles (list
              (make-iiop-profile
               :version (make-iiop-version 1 0)
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

(defun in-servant-invocation-context (poa servant)
  "Check if in an invocation context for servant (in POA)"
  (and *poa-current*
       (eql poa (poa-current-poa *poa-current*))
       (eql servant (poa-current-servant *poa-current*))))


;;;   ObjectId servant_to_id(in Servant p_servant)
;;;     raises (ServantNotActive, WrongPolicy);

(define-method servant_to_id ((poa PortableServer:POA) servant)
  (cond ((in-servant-invocation-context poa servant)
         (op:_object_id servant))
        ((and (poa-has-policy poa :unique_id)
              (servant-oid (active-object-map poa) servant))) 
        ((poa-has-policy poa :implicit_activation)
         ;;(check-policy poa :retain) should have been checked when poa created
         (let ((id (generate-id poa)))
	   (op:activate_object_with_id poa id servant)
	   id))
        (t
         (or (poa-has-policy poa :use_default_servant)
             (progn (check-policy poa :retain)
                    (or (poa-has-policy poa :unique_id)
                        (check-policy poa :implicit_activation))))
         (error (portableserver:poa/servantnotactive)))))

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
    (multiple-value-bind (ref-type poa-spec oid)
                         (decode-object-key (iiop-profile-key (first profiles)))
      (declare (ignore ref-type))
      (let ((refpoa (poa-locate *root-poa* poa-spec)))
        (unless (eql refpoa poa)
          (error 'PortableServer:poa/wrongadapter)))
      oid)))


;;;   Servant id_to_servant(in ObjectId oid)
;;;     raises (ObjectNotActive, WrongPolicy);

(define-method id_to_servant ((poa PortableServer:POA) id)
  (or (poa-has-policy poa :use_default_servant)
      (check-policy poa :retain))
  (setq id (to-object-id id))
  (or (oid-servant (active-object-map poa) id)
      (if (poa-has-policy poa :use_default_servant)
        (op:get_servant poa))
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
      (raise-system-exception 'CORBA:intf_repos))))


;; ----------------------------------------------------------------------
;;;; Request Handling
;; ----------------------------------------------------------------------


(defun get-servant (poa oid operation)
  (flet ((check-servant (servant) 
           (unless (typep servant 'PortableServer:Servant)
             (raise-system-exception 'CORBA:OBJ_ADAPTER 2 :completed_no))
           servant))
    (cond ((oid-servant (active-object-map poa) oid))
          ((poa-has-policy poa :use_servant_manager)
           (cond ((poa-has-policy poa :retain)
                  (let ((servant (check-servant (op:incarnate (POA-servant-manager poa) oid poa))))
                    (mess 2 "~A incarnated ~A for '~/clorb:stroid/'" poa servant oid)
                    (op:activate_object_with_id poa oid servant)
                    servant))
                 (t
                  (multiple-value-bind (servant cookie)
                                       (op:preinvoke (POA-servant-manager poa) oid poa operation)
                    (check-servant servant)
                    (values servant (lambda ()
                                      (op:postinvoke (POA-servant-manager poa)
                                                     oid poa operation cookie servant)))))))
          ((poa-has-policy poa :use_default_servant)
           (or (POA-default-servant poa)
               (raise-system-exception 'CORBA:OBJ_ADAPTER 3 :completed_no)))
          (t
           (raise-system-exception 'CORBA:OBJECT_NOT_EXIST 0 :completed_no)))))



(defun poa-invoke (poa request)
  (let ((oid (request-object-id request))
        (operation (request-operation request))
        (buffer (request-input request)))
    (handler-case
      (multiple-value-bind (servant postinvoke)
                           (get-servant poa oid operation)
        (let ((*poa-current* (make-poa-current poa oid servant)))
          (setf (request-state request) :exec)
          (unwind-protect
            (servant-invoke servant operation buffer request)
            (when postinvoke (funcall postinvoke)))))
      (PortableServer:ForwardRequest (exception)
       (set-request-forward request (op:forward_reference exception)))
      (CORBA:UserException ()
       (raise-system-exception 'CORBA:UNKNOWN)))
    (server-request-respond request)))


(defun poa-locate (poa poa-spec &optional (check-poa-status t))
  (cond ((numberp poa-spec)
         (values (gethash poa-spec *poa-map*)))
        ((null poa-spec)
         poa)
        (t
         (let ((next-poa (find-requested-poa poa (car poa-spec) t check-poa-status)))
           (cond ((eql next-poa :wait)
                  (values poa poa-spec))
                 ((null next-poa)
                  (raise-system-exception 'CORBA:OBJECT_NOT_EXIST 2 :completed_no))
                 (t
                  (poa-locate next-poa (cdr poa-spec))))))))


(defun poa-dispatch-1 (poa req poa-spec)
  (when poa-spec
    (multiple-value-setq (poa poa-spec) (poa-locate poa poa-spec))
    (setf (poa-spec req) poa-spec))
  (unless poa
    (raise-system-exception 'CORBA:OBJECT_NOT_EXIST 0 :completed_no))
  (let ((state (poa-effective-state poa)))
    (cond ((or (eql state :holding) poa-spec)
           (enqf (poa-request-queue poa) req))
          ((eql state :discarding)
           (raise-system-exception 'CORBA:TRANSIENT 1 :completed_no))
          ((eql state :inactive)
           (raise-system-exception 'CORBA:OBJ_ADAPTER 0 :completed_no))
          (t
           (poa-invoke poa req)))))
    
  
(defun poa-dispatch (poa req)
  (handler-case
    (poa-dispatch-1 poa req (poa-spec req))
    (CORBA:SystemException (exc)
                           (server-request-systemexception-reply req exc))))


(defun dispatch-request (orb req objkey)
  (multiple-value-bind (type poa-spec object-id)
                       (decode-object-key objkey)
    (declare (ignore type))
    (setf (request-object-id req) object-id)
    (setf (poa-spec req) poa-spec)
    (poa-dispatch *root-poa* req)))





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
