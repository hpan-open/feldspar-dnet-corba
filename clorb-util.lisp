;;;; Nice interface to NameService etc.

(in-package :clorb)


;;;; Easy DII

;; Interface:
(defun invoke (obj op &rest args)
  (request-invoke
   (object-create-request obj op args)))

(defun corba:funcall (op obj &rest args)
  (apply 'invoke obj op args))


(defun object-interface (obj)
  (handler-case (interface-from-def-cached (invoke obj "_interface"))
    (corba:systemexception (exc)
      (if (member (exception-id exc)
                  '("IDL:omg.org/CORBA/BAD_OPERATION:1.0"
                    "IDL:CORBA/BAD_OPERATION:1.0" ; -dan / for ORBit
                    "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0"
                    "IDL:omg.org/CORBA/INTF_REPOS:1.0")
                  :test #'equal)
          nil
        (error exc)))))


(defun object-opdef (object op &aux effective-id)
  (if (consp op)
      (setq effective-id (first op)
            op (second op))
    (setq effective-id (object-id object)))
  (or (find-opdef *object-interface* op)
      (find-opdef (or (known-interface effective-id)
                      (object-interface object)
                      (get-interface effective-id)) 
                  op)))


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
             :name (param-name param)
             :argument (CORBA:Any :any-typecode (param-typecode param)
                                  :any-value (if (eq (param-mode param) 
                                                     :param_out)
                                                 nil
                                               (pop args)))
             :arg_modes (ecase (param-mode param)
                          (:param_in ARG_IN)
                          (:param_out ARG_OUT)
                          (:param_inout ARG_INOUT)))))
     :opdef opdef)))


;;;; Easy name service access

(defun ns-name (names)
  (loop for ns in names
        append (ns-name1 ns)))

(defun ns-name1 (namestring)
  (let ((name nil)
        (id (make-array 50 :fill-pointer 0 :adjustable t
                        :element-type 'character))
        (kind (make-array 50 :fill-pointer 0 :adjustable t
                          :element-type 'character)))
    (loop for c across (concatenate 'string namestring ";")
          with state = 0 do
          (cond
            ((eql c #\;)
             (push (make-struct "IDL:omg.org/CosNaming/NameComponent:1.0"
                                :id (copy-seq id) :kind (copy-seq kind))
                   name)
             (setf (fill-pointer id) 0
                   (fill-pointer kind) 0)
             (setq state 0))
            (t
             (ecase state
               (0 (cond ((eql c #\&) (setq state 1))
                        (t (vector-push-extend c id))))
               (1 (cond ((eql c #\&) 
                         (vector-push-extend c id)
                         (setq state 0))
                        (t
                         (vector-push-extend c kind)
                         (setq state 2))))
               (2 (cond ((eql c #\&) (setq state 3))
                        (t (vector-push-extend c kind))))
               (3 (cond ((eql c #\&)
                         (vector-push-extend c kind)
                         (setq state 2))
                        (t (error "Name syntax error"))))))))
    (nreverse name)))



(defun get-ns ()
  (object-narrow
   (op::resolve_initial_references (orb_init) "NameService")
   "IDL:omg.org/CosNaming/NamingContext:1.0"))

(defun resolve (&rest names)
  (invoke (get-ns) "resolve" (ns-name names)))

(defun rebind (objref &rest names)
  (invoke (get-ns) "rebind" (ns-name names) objref))
