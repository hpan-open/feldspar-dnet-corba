;;;; clorb-servant.lisp
;; $Id: clorb-servant.lisp,v 1.13 2003/07/03 08:47:44 lenst Exp $

(in-package :clorb)

;;;; Generic functions used by the POA implementation

(defgeneric servant-invoke (servant operation input handler)
  (:documentation "Called by the POA to perform an operation on the object"))

(defgeneric primary-interface (servant oid poa))

;; for the handler

(defgeneric get-normal-response (handler))
(defgeneric get-exception-response (handler))



;;;; Class: PortableServer:Servant

;; IDL/Lisp 6.2
;; System defined:
;;  op:_this SERVANT
;; User defined:
;;  op:_default_POA SERVANT
;; To support the Object interface:
;;  op:_non_existent, op:_is_a, op:_get_interface

(defclass PORTABLESERVER:SERVANT (corba:object) ())

(define-method _default_POA ((servant PortableServer:servant))
  (root-POA))

(define-method _non_existent ((servant PortableServer:servant))
  nil)

(defmethod primary-interface ((servant portableserver:servant) oid poa)
  (declare (ignore oid poa))
  (object-id servant))

(defmethod servant-invoke ((servant portableserver:servant) operation input handler)
  (declare (ignore operation input handler))
  (error 'omg.org/corba:bad_operation
         :completed :completed_no))

(defmethod servant-invoke :around ((servant portableserver:servant) operation input handler)
  (cond ((string= operation "_locate") nil)
        ((string= operation "_is_a")
         (let ((output (get-normal-response handler)))
           (marshal-bool (op:_is_a servant (unmarshal-string input)) output)
           output))
        ((string= operation "_non_existent")
         (let ((output (get-normal-response handler)))
           (marshal-bool (op:_non_existent servant) output)
           output))
        ((or (string= operation "_interface")
             (string= operation "_get_interface"))
         (let ((output (get-normal-response handler)))
           (marshal-ior (op:_get_interface servant) output)
           output))
        (t
         (call-next-method))))


;;;; Class: DynamicImplementation

(defclass PORTABLESERVER:DYNAMICIMPLEMENTATION (portableserver:servant)
  ())

;; IDL/Lisp 6.3 DynamicImplementation
;; User defined:
;;  op:invoke SERVANT SERVERREQUEST
;;  op:primary_interface SERVANT OID POA

(define-method invoke ((servant PortableServer:DynamicImplementation) sreq)
  (declare (ignore sreq))
  (error 'CORBA:NO_IMPLEMENT))

(define-method primary_interface ((servant PortableServer:DynamicImplementation) oid poa)
  (declare (ignore oid poa))
  (error 'CORBA:NO_IMPLEMENT))

(defmethod servant-invoke ((servant portableserver:dynamicimplementation) operation input handler)
  (let ((req (make-instance 'CORBA:ServerRequest
               :operation operation 
               :input input )))
    
    (op:invoke servant req)

    (let (buffer)
      (cond ((request-exception req)
             (setq buffer (get-exception-response handler))
             (marshal-any-value (request-exception req) buffer))
            (t
             (setq buffer (get-normal-response handler))
             (let ((res (request-result req)))
               (when (and res (not (eq :tk_void (op:kind (any-typecode res)))))
                 (marshal-any-value res buffer)))
             (loop for param in (arguments req)
                   unless (zerop (logand ARG_OUT (op:arg_modes param)))
                   do (marshal-any-value (op:argument param) buffer))))
      buffer)))




(defmethod primary-interface ((servant portableserver:dynamicimplementation) oid poa)
  (op:primary_interface servant oid poa))


;;;; ServerRequest
#|
      interface ServerRequest { // PIDL
        readonly attribute  Identifier operation;
        void                arguments    (inout NVList nv);
        Context             ctx();
        void                set_result   (in any val);
        void                set_exception(in any val);
    };
|#


(define-corba-class CORBA:ServerRequest (CORBA:Object)
  :attributes ((operation :readonly))
  :slots ((input :initarg :input :initform nil :accessor request-input) 
          (arguments               :accessor arguments)
          (result  :initform nil   :accessor request-result)
          (exception :initform nil :accessor request-exception)))


(defun check-before-arg (r)
  (when (slot-boundp r 'arguments)
    (error 'omg.org/corba:bad_inv_order )))

(defun check-after-arg (r)
  (unless (slot-boundp r 'arguments)
    (error 'omg.org/corba:bad_inv_order )))

(define-method arguments ((self CORBA:ServerRequest) nv)
  (check-before-arg self)
  (setf (arguments self) nv)
  (loop for param in nv
        unless (zerop (logand ARG_IN (op:arg_modes param)))
        do (setf (any-value (op:argument param))
                 (unmarshal (any-typecode (op:argument param))
                            (request-input self)))
        unless (op:argument param)
        do (setf (op:argument param) (corba:any)))
  nv )

(define-method set_result ((self CORBA:ServerRequest) val)
  (check-after-arg self)
  (setf (request-result self) val))

(define-method set_exception ((self CORBA:ServerRequest) val)
  (check-after-arg self)
  (setf (request-exception self) val))



;;; clorb-servant.lisp ends here
