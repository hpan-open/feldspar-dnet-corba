(in-package :clorb)

(defvar *locator-poa* nil)

(define-servant locator-servant "Locator::Locator")
(define-servant factory-servant "Locator::Factory")

(defclass loc-manager (ServantManager)
  ((servers
    :initform (make-hash-table :test #'equal)
    :documentation "Map from server-name to servers factory obj"
    :reader managed-servers)))

(defclass locator (locator-servant)
  ((manager :initarg :manager :reader loc-manager)))

(defclass loc-factory (factory-servant)
  ((name :initarg :name :reader server-name)
   (server :initarg :server :accessor server-ref)))


(defun setup-locator-poa ()
  (setq *locator-poa*
    (op:create_poa (root-poa) "locator" (op:the_poamanager (root-poa))
                   '(:use-servant-manager :persistent :user-id)))
  (op:set_servant_manager *locator-poa* (make-instance 'loc-manager)))


(define-method register ((loc locator) name server)
  (let* ((servers (managed-servers (loc-manager loc)))
         (loc-factory (gethash name servers)))
    (unless loc-factory
      (setq loc-factory (make-instance 'loc-factory
                          :name name))
      (setf (gethash name servers) loc-factory))
    (setf (server-ref loc-factory) server)
    loc-factory))


(defun store-loc-info (server-name poa-name oid intf)
  "Store object location info in persistent storage and
return an indentifier for this storage."
  (marshal-make-encapsulation 
   (lambda (buffer)
     (marshal-string server-name  buffer)
     (marshal-sequence poa-name #'marshal-string buffer)
     (marshal-osequence oid buffer)
     (marshal-string intf buffer))))

(defun load-loc-info (store-oid)
  (unmarshal-encapsulation
   store-oid 
   (lambda (buffer)
     (values 
      (unmarshal-string buffer)
      (unmarshal-sequence-m (buffer) (unmarshal-string buffer))
      (unmarshal-osequence buffer)
      (unmarshal-string)))))


(define-method create_reference ((loc-factory loc-factory)
                                 poa-name oid intf)
  (let ((store-oid
         (store-loc-info (server-name loc-factory) poa-name oid intf)))
    (op:create_reference_with_id *locator-poa* store-oid intf)))

(define-method incarnate ((manager loc-manager) store-oid poa)
  (multiple-value-bind (server-name poa-name oid intf)      
      (load-loc-info store-oid)
    (let* ((factory (gethash server-name (managed-servers manager)))
           (object (op:create_reference factory poa-name oid intf)))
      (error (make-condition 'PortableServer:ForwardRequest
               :forward_reference object))))) 

;;;; --- In the poa ---

(defclass my-ref-factory (factory-servant)
  ())

(defvar *my-ref-factory*
    (make-instance 'my-ref-factory))

(defvar *loc-factory* )

(defun server-startup (orb name)
  (let ((loc-server (op:resolve_initial_references orb "Locator")))
    (setq *loc-factory* (op:register loc-server name *my-ref-factory*))))

(defun creating-persistent-ref (poa oid intf)
  (op:create_reference *loc-factory* (poa-name poa) oid intf))

(define-method create_reference ((my-ref-factory my-ref-factory)
                                 poa-name oid intf)
  (let ((poa (root-poa)))
    (doseq (name poa-name)
      (setq poa (op:find_poa poa name t)))
    (real-create-ref poa oid intf)))

