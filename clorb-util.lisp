;;;; Nice interface to NameService etc.

(in-package :clorb)

(defvar *repositories* '())


;;;; file URL

(defun pathname-url (pathname)
  (format nil "file://~{/~A~}/~A.~A"
          (cdr (pathname-directory pathname))
          (pathname-name pathname)
          (pathname-type pathname)))



;;;; Easy DII

(defun invoke (obj op &rest args)
  (request-funcall
   (object-create-request obj op args)))

(defun corba:funcall (op obj &rest args)
  (request-funcall
   (object-create-request obj op args)))


(defun object-interface (obj)
  (handler-case (interface-from-def-cached (invoke obj "_interface"))
    (corba:systemexception (exc)
      (if (member (exception-id exc)
                  '("IDL:omg.org/CORBA/BAD_OPERATION:1.0"
                    "IDL:CORBA/BAD_OPERATION:1.0" ; -dan / for ORBit
                    "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0"
                    "IDL:omg.org/CORBA/INTF_REPOS:1.0"
                    "IDL:omg.org/CORBA/OBJ_ADAPTER:1.0" ; mico
                    )
                  :test #'equal)
          nil
        (error exc)))))


(defun analyze-operation-name (name)
  (cond
   ((< (length name) 6) name)
   ((string= name "_get_" :end1 5)
    (values (subseq name 5) :getter))
   ((string= name "_set_" :end1 5)
    (values (subseq name 5) :setter))
   (t name)))

(defmethod find-opdef ((interface CORBA:InterfaceDef) operation)
  "Find in INTERFACE the OPERATION and return the opdef object."
  ;; Compatibility with clorb-iir for use in dii and auto-servants.
  (multiple-value-bind (name type)
      (analyze-operation-name operation)
    (let ((def (op:lookup interface name)))
      (case type
        (:setter
         (assert (eq (op:def_kind def) :dk_Attribute))
         (make-setter-opdef operation def))
        (:getter
         (assert (eq (op:def_kind def) :dk_Attribute))
         (make-getter-opdef operation def))
        (otherwise
         (assert (eq (op:def_kind def) :dk_Operation))
         (make-opdef :name operation
                     :params (op:params def)
                     :result (op:result def)
                     :raises (map 'list #'op:type (op:exceptions def))))))))


(defun object-opdef (object op)
  (let ((effective-id (proxy-id object)))
    (or (find-opdef *object-interface* op)
        (find-opdef (or (known-interface effective-id)
                        (loop for rep in *repositories*
                              thereis (op:lookup_id (if (symbolp rep) (symbol-value rep) rep) effective-id))
                        (object-interface object)
                        (get-interface effective-id)) 
                    op))))

#|
This operation, which creates a pseudo-object, is defined in the ORB interface.
void create_operation_list ( // PIDL
in OperationDef oper, // operation
out NVList new_list // argument definitions
);
This operation returns an NVList initialized with the argument descriptions for a given
operation. The information is returned in a form that may be used in Dynamic
Invocation requests. The arguments are returned in the same order as they were defined
for the operation.
The list free operation is used to free the returned information.
|#
(defun object-create-request (object op args)
  (let* ((opdef (object-opdef object op)))
    (unless opdef
      (error "Operation (~A) not defined for interface" op))
    (unless (= (length args)
	       (length (opdef-inparams opdef)))
      (error "Wrong number of arguments to operation"))
    (make-instance 'request
     :target object
     :operation (opdef-name opdef)
     :paramlist
     (cons 
      (CORBA:NamedValue
       :argument (CORBA:Any :any-typecode (opdef-result opdef))
       :arg_modes ARG_OUT)
      (loop for param in (opdef-params opdef)
          collect 
            (CORBA:NamedValue
             :name (op:name param)
             :argument (CORBA:Any :any-typecode (op:type param)
                                  :any-value (if (eq (op:mode param) 
                                                     :param_out)
                                                 nil
                                               (pop args)))
             :arg_modes (ecase (op:mode param)
                          (:param_in ARG_IN)
                          (:param_out ARG_OUT)
                          (:param_inout ARG_INOUT)))))
     :exceptions (opdef-raises opdef))))


;;;; Easy name service access

(defun ns-name* (names)
  (mapcan #'ns-name names))

(defun ns-name (namestring)
  (let ((name nil)
        (id (make-array 50 :fill-pointer 0 :adjustable t
                        :element-type 'character))
        (kind (make-array 50 :fill-pointer 0 :adjustable t
                          :element-type 'character)))
    (loop for c across (concatenate 'string namestring "/")
          with escape = nil and part = id
          do
          (cond
            (escape
             (vector-push-extend c part)
             (setq escape nil))
            (t
             (case c
               ((#\/)
                (push (CosNaming:NameComponent
                       :id (copy-seq id) :kind (copy-seq kind))
                      name)
                (setf (fill-pointer id) 0)
                (setf (fill-pointer kind) 0)
                (setq part id))
               ((#\\) (setq escape t))
               ((#\.) (setq part kind))
               (otherwise
                (vector-push-extend c part))))))
    (nreverse name)))



(defun get-ns ()
  (let ((ns (op:resolve_initial_references (orb_init) "NameService")))
    (typecase ns
      (cosnaming:namingcontext ns)
      (t (object-narrow ns 'cosnaming:namingcontext)))))

(defun resolve (&rest names)
  (op:resolve (get-ns) (ns-name* names)))

(defun rebind (objref &rest names)
  (op:rebind (get-ns) (ns-name* names) objref))
