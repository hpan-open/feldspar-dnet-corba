;;;; clorb-servant.lisp
;; $Id: clorb-servant.lisp,v 1.5 2002/05/02 20:40:11 lenst Exp $

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
      (op:is_a (op::_get_interface servant) logical-type-id)))

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
           (let ((repid (CORBA:Any :any-typecode corba:tc_string)))
             (op:arguments sreq (list (CORBA:NamedValue :argument repid)))
             (op:set_result sreq
                            (CORBA:Any
                             :any-typecode corba:tc_boolean
                             :any-value (op:_is_a servant (any-value repid))))))
          
          ((equal op "_non_existent")
           (op:arguments sreq nil)
           (op:set_result sreq
                          (CORBA:Any
                           :any-typecode corba:tc_boolean
                           :any-value (op:_non_existent servant))))
          ((equal op "_interface")
           (op:arguments sreq nil)
           (op:set_result sreq
                          (CORBA:Any
                           :any-typecode corba:tc_objref
                           :any-value (op:_get_interface servant))))
          (t 
           (op:invoke servant sreq)))))


(defmethod primary-interface ((servant DynamicImplementation) oid poa)
  (op:primary_interface servant oid poa))


;;;; Implementation details

;; FIXME: move to clorb-poa ?? 
(define-method _this ((servant servant))
  (let* ((poa (or (op:_default_POA servant)
		  (root-POA) ))
	 (oid (if (and (member :multiple-id (POA-policies poa))
                       *poa-current*)
                  (poa-current-object-id *poa-current*)
                (op::servant_to_id poa servant))))
    (op:create_reference_with_id
     poa oid
     (primary-interface servant oid poa))))


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

(defun operation-symbol (string)
  (intern (string-upcase string) :op))

(defun servant-opinfo (servant operation)
  (let ((opdef       
         (find-opdef *object-interface* operation)))
    (if opdef 
        (values 
         (if (equal operation "_interface")
             'op:_get_interface 
           (operation-symbol operation))
         (opdef-inparam-typecodes opdef)
         (opdef-outparam-typecodes opdef))
      (find-opinfo (servant-interface servant) operation))))

(defmethod find-opinfo ((interface interface) operation)
  (let ((opdef (find-opdef interface operation)))
    (when opdef
      (multiple-value-bind (name type)
          (analyze-operation-name operation)
        (values (case type
                  (:setter 
                   (fdefinition (list 'setf (operation-symbol name)))           )
                  (:getter 
                   (operation-symbol name))
                  (otherwise
                   (operation-symbol operation)))
                (opdef-inparam-typecodes opdef)
                (opdef-outparam-typecodes opdef))))))


(defmethod servant-invoke ((servant auto-servant) sreq)
  (let* ((operation (server-request-operation sreq)))
    (mess 3 "servant-invoke ~S ~S" servant operation)
    (mess 1 "sreq: service-context=~S" (server-request-service-context sreq))
    (multiple-value-bind (method inparams outparams)
        (servant-opinfo servant operation)
      (let ((args (unmarshal-multiple inparams (server-request-buffer sreq))))
        (unless (or (functionp method)
                    (fboundp method))
          (error 'CORBA:NO_IMPLEMENT))
        (let* ((*current-server-request* sreq)
               (res (multiple-value-list
                     (apply method servant args))))
          (cond ((equal res '(defer))
                 (set-request-result sreq 'defer))
                (t
                 (set-request-result sreq res outparams))))))))


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
