;;;; clorb-object.lisp --- CORBA:Object
;; $Id: clorb-object.lisp,v 1.1 2000/11/14 22:50:16 lenst Exp $

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

(defclass CORBA:Object ()
  ())

(defclass CORBA:Proxy (CORBA:Object)
  ((id :initform nil :initarg :id :accessor object-id)
   (host :initform nil :initarg :host :accessor object-host)
   (port :initform nil :initarg :port :accessor object-port)
   (key :initform nil :initarg :key :accessor object-key)
   (profiles :initform nil :initarg :profiles :accessor object-profiles)
   (forward :initform nil :initarg :forward :accessor object-forward)))


(defmethod print-object ((o corba:proxy) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~S @ ~A:~A"
          (object-id o)
          (object-host o)
          (object-port o))))

(define-method _is_nil ((x null))
  t)

(define-method _is_nil ((x CORBA:Object))
  nil)

;;;| boolean	is_equivalent (in Object other_object);
;;; _is_equivalent in lisp mapping

(define-method _is_equivalent ((obj corba:proxy) other)
  (and
   (equal (object-host obj) (object-host other))
   (equal (object-port obj) (object-port other))
   (equalp (object-key obj) (object-key other))))

;;;| unsigned 	long hash(in unsigned long maximum);
;;; _hash in lisp mapping

(define-method _hash ((obj corba:proxy) maximum)
  (rem (sxhash (list* (object-host obj)
                      (object-port obj)
                      (coerce (object-key obj) 'list)))
       maximum))


;;;; Registry for Proxy classes

(defvar *proxy-classes*
    (make-hash-table :test #'equal))

(defun find-proxy-class (id)
  (gethash id *proxy-classes* 'CORBA:Proxy))

(defun register-proxy-class (id class)
  (setf (gethash id *proxy-classes*) class))


;;; clorb-object.lisp ends here
