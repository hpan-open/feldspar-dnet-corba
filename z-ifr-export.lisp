;; Servant wrapper for local IFR

(in-package :clorb)

(require-idl "CORBA::Repository" 
             :file "clorb:src;ifr-idl.lisp")


;;;; Object indexer
;; (object-index oi obj) => integer
;; (index-object oi integer) => obj
;; --
(defclass object-indexer ()
  ((imap
    :initform (make-hash-table)
    :accessor imap)
   (omap
    :initform (make-hash-table)
    :accessor omap)
   (count
    :initform 0
    :accessor oi-count)))  

(defmethod object-index ((oi object-indexer) object)
  (or (gethash object (omap oi))
      (let ((i (incf (oi-count oi))))
        (setf (gethash object (omap oi)) i)
        (setf (gethash i (imap oi)) object)
        i)))

(defmethod index-object ((oi object-indexer) index)
  (gethash index (imap oi)))
    
;;;;

(defgeneric translate (translator object)
  (:method (x (obj number)) 
           (declare (ignore x))
           obj)
  (:method (x (obj symbol))
           (declare (ignore x))
           obj)
  (:method (x (obj string))
           (declare (ignore x))
           obj)
  (:method (x (obj CORBA:TypeCode))
           (declare (ignore x))
           obj)
  (:method (x (obj vector))
           (map 'vector #'(lambda (o) (translate x o)) obj))
  (:method (x (obj sequence))
           (map 'list #'(lambda (o) (translate x o)) obj))
  (:method (x (obj CORBA:struct))
           (map-struct (lambda (v) (translate x v)) obj)))


  
;;;;

(defun create-operation-list (opdef)
  (map 'list
       (lambda (pd) 
         (CORBA:NamedValue
          :name (op:name pd)
          :argument (CORBA:Any :any-typecode (op:type pd))
          :arg_modes (ecase (op:mode pd)
                       (:param_in ARG_IN)
                       (:param_out ARG_OUT)
                       (:param_inout ARG_INOUT))))
       (op:params opdef)))

;;;;

(defconstant +oid-radix+ 36)

(defun oid-integer (oid)
  (parse-integer (oid-to-string oid) :radix +oid-radix+))

(defun integer-oid (integer)
  (string-to-oid (format nil "~vR" +oid-radix+ integer)))

;;;;

(defclass translator ()
  ((wrapper :initarg :wrapper :reader wrapper)))

(defmethod objmap ((x translator)) (objmap (wrapper x)))
(defmethod the-poa ((x translator)) (the-poa (wrapper x)))

(defclass local-translator (translator) ())

(defmethod translate ((x local-translator) (obj CORBA:Proxy))
  (index-object (objmap x) (oid-integer (op:reference_to_id (the-poa x) obj))))

(defclass remote-translator (translator) ())

(defmethod translate ((x remote-translator) (obj IRObject))
  (op:create_reference_with_id (the-poa x)
                               (integer-oid (object-index (objmap x) obj))
                               (object-id obj)))

;;;;

(defclass servant-wrapper (PortableServer:dynamicimplementation)
  ((orb
    :initarg :orb
    :initform (orb_init)
    :reader the-orb)
   (poa
    :initarg :poa
    :reader the-poa)
   (objmap
    :initform (make-instance 'object-indexer)
    :reader objmap)
   (local-translator)
   (remote-translator)))

(define-method _default_poa ((self servant-wrapper))
  (the-poa self))

(define-method primary_interface ((self servant-wrapper) oid poa)
  (declare (ignore poa))
  (object-id (index-object (objmap self) (oid-integer oid))))

(define-method _is_a ((self servant-wrapper) id)
  (object-is-a (index-object (objmap self) (oid-integer (op:_object_id self)))
               id))

(defmethod local-translator ((self servant-wrapper))
  (if (slot-boundp self 'local-translator)
    (slot-value self 'local-translator)
    (setf (slot-value self 'local-translator) (make-instance 'local-translator :wrapper self))))

(defmethod remote-translator ((self servant-wrapper))
  (if (slot-boundp self 'remote-translator)
    (slot-value self 'remote-translator)
    (setf (slot-value self 'remote-translator) (make-instance 'remote-translator :wrapper self))))


(defun make-remote (wrapper obj)
  (translate (remote-translator wrapper) obj))

(defun make-local (wrapper obj)
  (translate (local-translator wrapper) obj))


(defun wrapper-opinfo (self operation)
  ;; values: func type paramlist result-type
  (let* ((id (op:primary_interface self (op:_object_id self) (op:_poa self)))
         (interface (or (op:lookup_id *idef-repository* id)
                        (error "can't find interface: ~S" id))))
    (multiple-value-bind (name type) (analyze-operation-name operation)
      (let ((def (op:lookup interface name))
            (sym (intern (string-upcase name) :op)))
        (case type
          (:setter
           (assert (eq (op:def_kind def) :dk_Attribute))
           (values (fdefinition (list 'setf sym)) :setter
                   (list (corba:namedvalue :argument (corba:any :any-typecode (op:type def))
                                           :arg_modes ARG_IN))
                   CORBA:tc_void))
          (:getter
           (assert (eq (op:def_kind def) :dk_Attribute))
           (values sym nil nil (op:type def)))
          (otherwise
           (assert (eq (op:def_kind def) :dk_Operation))
           (values sym nil (create-operation-list def) (op:result def))))))))

(define-method invoke ((self servant-wrapper) r)
  (multiple-value-bind (op type args result-type)
                       (wrapper-opinfo self (op:operation r))
    (setq args (op:arguments r args))
    (let* ((local-obj (make-local self (op:_this self)))
           (local-args 
            (loop for arg in args 
                  when (/= 0 (logand ARG_IN (op:arg_modes arg)))
                  collect (make-local self (any-value (op:argument arg)))))
           (res-list 
            (loop for x in (multiple-value-list
                            (if (eq type :setter)
                              (funcall op (first local-args) local-obj)
                              (apply op local-obj local-args)))
                  collect (make-remote self x)))
           (result (pop res-list)))
      (loop for arg in args
            when (/= 0 (logand ARG_OUT (op:arg_modes arg)))
            do (setf (any-value (op:argument arg)) (pop res-list)))
      (op:set_result r (corba:any :any-typecode result-type
                                  :any-value result)))))



;;;; Set up a real thingy

(defvar *z-rep* (make-instance 'repository))
(defvar *z-poa* (let ((rootpoa (op:resolve_initial_references *the-orb* "RootPOA")))
                  (op:create_poa rootpoa "IFWRAP"
                                 nil
                                 '(:use_default_servant
                                   :user_id
                                   :transient))))
(defvar *z-serv* (make-instance 'servant-wrapper
                      :orb *the-orb*
                      :poa *z-poa*))

(op:set_servant *z-poa* *z-serv*)
(op:activate (op:the_poamanager *z-poa*))
(rebind (make-remote *z-serv* *z-rep*) "zrep")

;;(idef-read (idef-write *idef-repository*) *z-rep*)