;;;; Nice interface to NameService etc.

(in-package :clorb)

(defvar *repositories* '())


;;;; Easy DII

;; Interface:
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


(defun object-opdef (object op)
  (let ((effective-id (object-id object)))
    (or (find-opdef *object-interface* op)
        (find-opdef (or (known-interface effective-id)
                        (object-interface object)
                        (loop for rep in *repositories*
                              thereis (op:lookup_id (if (symbolp rep) (symbol-value rep) rep) effective-id))
                        (get-interface effective-id)) 
                    op))))


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
                (push (make-struct "IDL:omg.org/CosNaming/NameComponent:1.0"
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
  (object-narrow
   (op:resolve_initial_references (orb_init) "NameService")
   "IDL:omg.org/CosNaming/NamingContext:1.0"))

(defun resolve (&rest names)
  (corba:funcall "resolve" (get-ns) (ns-name* names)))

(defun rebind (objref &rest names)
  (corba:funcall "rebind" (get-ns) (ns-name* names) objref))
