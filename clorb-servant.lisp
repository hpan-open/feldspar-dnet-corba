;;;; clorb-servant.lisp
;; $Id: clorb-servant.lisp,v 1.9 2002/10/19 02:55:55 lenst Exp $

(in-package :clorb)

;;;; Generic functions used by the POA implementation

(defgeneric servant-invoke (servant sreq)
  (:documentation "Called by the POA to perform an operation on the object"))

(defgeneric primary-interface (servant oid poa))


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

(define-method _non_existent ((servant servant))
  nil)


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


;;; clorb-servant.lisp ends here
