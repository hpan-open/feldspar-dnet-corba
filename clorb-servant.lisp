;;;; clorb-servant.lisp
;; $Id: clorb-servant.lisp,v 1.1 2000/11/14 22:50:25 lenst Exp $

(in-package :clorb)


;;;; Class: PortableServer:Servant

;; IDL/Lisp 6.2
;; System defined:
;;  op:_this SERVANT
;; User defined:
;;  op:_default_POA SERVANT
;; To support the Object interface:
;;  op:_non_existent, op:_is_a, op:_get_interface

(defclass Servant (CORBA:Object) ())

(define-method _default_POA ((servant servant))
  (root-POA))

(defgeneric servant-invoke (servant sreq))
(defgeneric primary-interface (servant oid poa))

(define-method _non_existent ((servant servant))
  nil)

(define-method _is_a ((servant servant) logical-type-id)
  (or (equal logical-type-id "IDL:omg.org/CORBA/Object:1.0")
      (equal logical-type-id (current-primary-interface servant))
      (op::is_a (op::_get_interface servant) logical-type-id)))

(define-method _get_interface ((servant servant))
  (handler-case
      (op:lookup_id (get-ir) (current-primary-interface servant))
    (error ()
      (error 'corba:intf_repos))))

(defun current-primary-interface (servant)
  (primary-interface servant 
                     (poa-current-object-id *poa-current*)
                     (poa-current-poa *poa-current*)))


;;;; Class: DynamicImplementation

(defclass DynamicImplementation (Servant)
  ())

;; IDL/Lisp 6.3 DynamicImplementation
;; User defined:
;;  op:invoke SERVANT SERVERREQUEST
;;  op:primary_interface SERVANT OID POA

(define-method invoke ((servant DynamicImplementation) sreq)
  (declare (ignore sreq))
  (error 'CORBA:NO_IMPLEMENT))

(define-method primary_interface ((servant DynamicImplementation) oid poa)
  (declare (ignore oid poa))
  (error 'CORBA:NO_IMPLEMENT))

(defmethod servant-invoke ((servant DynamicImplementation) sreq)
  (let ((op (op:operation sreq)))
    (cond ((equal op "_is_a")
           (let (repid)
             (op:arguments sreq 
                           (list 
                            (CORBA:NamedValue
                             :argument (setq repid
                                         (CORBA:Any
                                          :any-typecode corba:tc_string)))))
             (op:set_result sreq
                            (CORBA:Any
                             :any-typecode corba:tc_boolean
                             :any-value (op::_is_a 
                                         servant (any-value repid))))))
          
          ((equal op "_non_existent")
           (op:arguments sreq nil)
           (op:set_result sreq
                          (corba:any
                           :any-typecode corba:tc_boolean
                           :any-value (op::_non_existent servant))))
          ((equal op "_interface")
           (op:arguments sreq nil)
           (op:set_result sreq
                          (corba:any
                           :any-typecode corba:tc_objref
                           :any-value (op::_get_interface servant))))
          (t 
           (op::invoke servant sreq)))))


(defmethod primary-interface ((servant DynamicImplementation) oid poa)
  (op::primary_interface servant oid poa))


;;;; Implementation details

;; FIXME: move to clorb-poa ?? 
(define-method op::_this ((servant servant))
  (let* ((poa (or (op:_default_POA servant)
		  (root-POA) ))
	 (oid (if (and (member :multiple-id (POA-policies poa))
                       *poa-current*)
                  (poa-current-object-id *poa-current*)
                (op::servant_to_id poa servant))))
    (op::create_reference_with_id 
     poa oid
     (primary-interface servant oid poa))))

;;; Implicit activation is implemented by this method that allows a
;;; servant to be used as an object reference when calling corba
;;; methods.
(defmethod marshal-ior ((servant servant) buffer)
  (marshal-ior (op:_this servant) buffer))


;;;; Auto-Dynamic servant (CLORB sepecific)

;;; This will use the interface repository or a local interface-def
;;; object to marshal requests parameters and invoke operations.

(defclass auto-servant (servant)
  ())

(defvar *current-server-request* nil
  "While operations are invoked on a servant, this variable will
be bound to the server-request.")

(defmethod servant-interface-id ((servant auto-servant))
  "Override to specifify interface by ID"
  nil)

(defmethod servant-interface ((servant auto-servant))
  "Override this if there is a ready interface-def object."
  (get-interface (servant-interface-id servant)))

(defmethod primary-interface ((servant servant) oid poa)
  (declare (ignore oid poa))
  (or (servant-interface-id servant)
      (op:id (servant-interface servant))))


;;; ----------------------------------------------------------------------

(defmethod servant-get-method ((servant servant) operation)
  (if (equal operation "_interface")
      'op:_get_interface
    (intern (string-upcase operation) :op)))

(defmethod servant-opdef ((servant auto-servant) operation)
  (or (find-opdef *object-interface* operation)
      (find-opdef (servant-interface servant) operation)))

(defun server-request-decoded-args (sreq opdef)
  (if (eq (server-request-argument sreq) :undecoded)
      (setf (server-request-argument sreq)
        (unmarshal-multiple
         (opdef-inparam-typecodes opdef)
         (server-request-buffer sreq))))
  (server-request-argument sreq))

(defun set-request-result (sreq results 
                           &optional (types nil types-p) 
                           &key (status 0))
  (setf (server-request-status sreq) status)
  (setf (server-request-values sreq) results)
  (unless types-p
    (setf types (opdef-outparam-typecodes (server-request-opdef sreq))))
  (setf (server-request-types sreq) types))

;;; ----------------------------------------------------------------------

(defmethod servant-invoke ((servant auto-servant) sreq)
  (let* ((operation (server-request-operation sreq))
         (opdef (servant-opdef servant operation)))
    (mess 3 "servant-invoke ~S ~S" servant operation)
    (mess 1 "sreq: service-context=~S" (server-request-service-context sreq))
    (unless opdef
      (error 'CORBA:BAD_OPERATION :completed :COMPLETED_NO))
    (let ((args (server-request-decoded-args sreq opdef))
          (method (servant-get-method servant operation)))
      (unless (fboundp method) (error 'CORBA:NO_IMPLEMENT))
      (setf (server-request-opdef sreq) opdef)
      (let* ((*current-server-request* sreq)
             (res (multiple-value-list 
                      (apply method servant args))))
        (cond ((equal res '(defer))
               (set-request-result sreq 'defer))
              (t
               (set-request-result sreq res)))))))


(define-method _get_interface ((servant auto-servant))
  (let ((i (servant-interface servant)))
    ;; As long as we still have the IIR with its own interface struct
    (if (or (typep i 'CORBA:Object))
        i
      (call-next-method))))

(define-method _is_a ((servant auto-servant) interface-id)
  (or (equal interface-id (interface-id *object-interface*))
      (op:is_a (servant-interface servant) interface-id)))


;;; clorb-servant.lisp ends here
