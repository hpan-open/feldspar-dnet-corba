;;;; NameService

(in-package :clorb)


;;;; NamingContext Servant 

(defconstant +naming-context-id+
  "IDL:omg.org/CosNaming/NamingContext:1.0")
(defconstant +name-component-id+
  "IDL:omg.org/CosNaming/NameComponent:1.0")
(defconstant +binding-id+
  "IDL:omg.org/CosNaming/Binding:1.0")


;;; Define the skeleton classes 

(define-servant naming-context-servant 
    "CosNaming::NamingContext"
  :id +naming-context-id+)

(define-servant binding-iterator-servant 
    "CosNaming::BindingIterator"
  :id "IDL:omg.org/CosNaming/BindingIterator:1.0")


;;;; Naming Context Implementation

(defclass naming-context (naming-context-servant)
  ((bind :initform (make-hash-table :test #'equal)
         :accessor naming-context-bind)))

(defmethod print-object ((self naming-context) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d" (hash-table-count 
                         (slot-value self 'bind)))))

(defmethod describe-object :after ((self naming-context) stream)
  (format stream " Names: ")
  (maphash (lambda (k v)
             (declare (ignore v))
             (format stream #+:ANSI-CL "~A~@_ " #-:ANSI-CL "~S " k))
           (slot-value self 'bind)))


;;;; Method code

(defconstant +missing-node+ 0)
(defconstant +not-context+ 1)
(defconstant +not-object+ 2)

(defun not-found (why rest-of-name)
  (error 'userexception
         :id "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
         :values (list why rest-of-name)))

(defun already-bound ()
  (error 'userexception
         :id "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"))

(define-method new_context ((self naming-context)) 
  (op:_this (make-instance 'naming-context)))

(define-method bind_context ((self naming-context) n nc) 
  (ns-bind self n nc 1))

(define-method bind_new_context ((self naming-context) n)
  (let ((nc (op::new_context self)))
    (ns-bind self n nc 1)
    nc))

(define-method bind ((self naming-context) n obj)
  (ns-bind self n obj))

(define-method rebind ((self naming-context) n obj)
  (ns-bind self n obj 0 t))

(define-method rebind_context ((self naming-context) n nc)
  (ns-bind self n nc 1 t))

(define-method resolve ((self naming-context) n)
  (multiple-value-bind (servant context nn)
      (ns-step self n)
    (if (null servant)
        (invoke context "resolve" nn)        
      (let* ((c (elt nn 0))
             (s (gethash (struct-get c :id) (naming-context-bind servant))) )
        (unless s
          (not-found +missing-node+ nn))
        (second s)))))

(define-method list ((self naming-context) how-many)
  (let ((result '()))
    (maphash
     (lambda (k v)
       (push (make-struct 
              +binding-id+
              :binding_name (vector (make-struct +name-component-id+
                                                 :id k :kind (first v)))
              :binding_type (third v))
             result))
     (naming-context-bind self))
    (if (> (length result) how-many)
        (values (subseq result 0 how-many)
                (make-instance 'binding-iterator 
                  :bindings (subseq result how-many)))
      (values result nil))))


(defun ns-step (self n)
  (do ((i       0     (1+ i))
       (context nil)
       (servant self))
      ((and servant (> i (- (length n) 2)))
       (values servant context (subseq n i)))
    (let ((s 
           (gethash (struct-get (elt n i) :id) 
                    (naming-context-bind servant))))
      (unless s
        (not-found +missing-node+ (subseq n i)))
      (unless (= 1 (third s))
        (not-found +not-context+ (subseq n i)))
      (setq context (second s))
      (setq servant (handler-case
                        (op:reference_to_servant 
                         (poa-current-poa *poa-current*)
                         context)
                      (PortableServer:POA/WrongAdapter () 
                        nil))))))

(defun ns-bind (self n obj &optional (binding-type 0) rebind)
  (multiple-value-bind (servant context nn)
      (ns-step self n)
    (cond 
     (servant 
       (let ((id (struct-get (elt nn 0) :id)))
         (mess 3 "id is ~S" id)
         (when (and (not rebind)
                    (gethash id (naming-context-bind servant)) ) 
           (already-bound))
         (setf (gethash id (naming-context-bind servant))
               (list (struct-get (elt nn 0) :kind) obj binding-type))))
     (t
      (invoke context
              (if rebind
                  (if (= binding-type 0) "rebind" "rebind_context")
                (if (= binding-type 0) "bind" "bind_context"))
              nn
              obj)))))

(define-method unbind ((self naming-context) n)
  (multiple-value-bind (servant context nn)
      (ns-step self n)
    (cond 
     (servant 
       (let ((id (struct-get (elt nn 0) :id)))
         (mess 3 "unbind id is ~S" id)
         (remhash id (naming-context-bind servant))))
     (t
      (invoke context "unbind" nn)))))

;;;; BindingIterator Servant

(defclass binding-iterator (binding-iterator-servant)
  ((bindings :initarg :bindings 
	     :initform nil :accessor bindings)))

(define-method next_one ((bi binding-iterator))
  (if (zerop (length (bindings bi)))
      (values nil (make-struct +binding-id+
                               :binding_name nil
                               :binding_type 0))
    (values t (prog1 (elt (bindings bi) 0)
                (setf (bindings bi)
                  (subseq (bindings bi) 1))))))

(define-method next_n ((bi binding-iterator) n)
  (if (zerop (length (bindings bi)))
      (values nil nil)
    (values t (let* ((bindings (bindings bi))
                     (end (min n (length bindings)))
                     (res (subseq bindings 0 end)))
                (setf (bindings bi) (subseq bindings end))
                res))))

(define-method destroy ((bi binding-iterator))
  (let* ((poa (poa-current-poa *poa-current*))
         (id (op::servant_to_id poa bi)))
    (op::deactivate_object poa id)))


;;;; Service Main Loop

(defvar *ns-servant* nil)

(defun name (&rest strseq)
  (map 'vector (lambda (id)
                 (make-struct +name-component-id+
                              :id id :kind ""))
       strseq))

(defun example1 (ns)
  (loop for (n k b) in `(("hej"  ""   nil)
                         ("."    ""   ,(op:_this ns))
                         ("dev"  c)
                         ("test" c))
      do (ns-bind ns
                  (name n)
                  (if b b (op:new_context ns))
                  (if (eq k 'c) 1 0)
                  t)))

(defun root-ns (&optional recreate)
  (when (or recreate (null *ns-servant*))
    (let ((nc (make-instance 'naming-context)))
      (example1 nc)
      (setf *ns-servant* nc)))
  *ns-servant*)

(defun setup-ns (&optional force)
  (let ((orb (corba:orb_init)))
    (with-open-file (wr *name-service* :direction :output
                        :if-exists :supersede)
                    (princ (orb-object-to-string orb (root-ns force)) wr))))

(defun run-ns ()
  (let ((orb (corba:orb_init)))
    (op:activate (op:the_poamanager (clorb::root-poa)))
    (setup-ns)
    (op:run orb)))



;;;; Persistant contexts

(defclass pns-manager (ServantActivator)
  ((basedir :initform '(:absolute "tmp"))
   (active  :initform nil :accessor pns-active)
   (started :initform (get-universal-time)
            :reader pns-started)
   (seqno   :initform 0  :accessor pns-seqno)))

(defclass pns-context (naming-context) ())

(define-method new_context ((self pns-context)) 
  (let* ((poa (poa-current-poa *poa-current*))
         (manager (op:get_servant_manager poa)))
    (op:create_reference_with_id 
     poa (format nil "~D-~D"
                 (pns-started manager)
                 (incf (pns-seqno manager)))
     +naming-context-id+)))

(defun save-context (orb oid hash basedir)
  (with-open-file (out (make-pathname :directory basedir
                                      :name (oid-to-string oid)
                                      :type "context"
                                      :version :newest)
                   :direction :output
                   :if-exists :supersede)
    (maphash (lambda (k v)
               (print (list
                       k                ; ID
                       (first v)        ; KIND
                       (op:object_to_string orb (second v)) ; Object
                       (third v)        ; Binding type
                       )
                      out))
             hash)))


(defun load-context (orb oid hash basedir)
  (with-open-file (in (make-pathname :directory basedir
                                     :name (oid-to-string oid)
                                     :type "context"
                                     :version :newest)
                   :direction :input
                   :if-does-not-exist :create)
    (loop for line = (read in nil nil)
        while line
        do (setf (gethash (first line) hash)
             (list (second line)
                   (op:string_to_object orb (third line))
                   (fourth line))))))


(define-method incarnate ((manager pns-manager) oid adapter)
  (declare (ignore adapter))
  (let ((orb (orb_init))
        (servant (make-instance 'pns-context)))
    (load-context orb oid (naming-context-bind servant)
                  (slot-value manager 'basedir))
    (push servant (pns-active manager))
    servant))

(defun pns-save (orb poa servant manager &optional oid)
  (unless oid
    (setq oid (op:servant_to_id poa servant)))
  (save-context orb oid (naming-context-bind servant)
                (slot-value manager 'basedir)))

(define-method etherealize ((manager pns-manager)
                            oid adapter servant cleanup-in-progress
                            remaining-activiations)
  (declare (ignore cleanup-in-progress remaining-activiations))
  (let ((orb (orb_init)))
    (pns-save orb adapter servant manager oid)
    (setf (pns-active manager) (delete servant (pns-active manager)))))


(defun setup-pns ()
  (let ((poa
         (op:create_poa (root-POA) "pns" nil 
                        '(:use-servant-manager :persistent :user-id))))
    (op:set_servant_manager poa (make-instance 'pns-manager))
    (op:activate (op:the_poamanager poa))
    (op:create_reference_with_id poa "root" +naming-context-id+)))

(defun save-all (&optional (orb (orb_init)))
  (let* ((poa (op:find_poa (root-POA) "pns"))
         (manager (op:get_servant_manager poa)))
    (dolist (servant (pns-active manager))
      (pns-save orb poa servant manager))))


(defun reset-pns ()
  (let ((poa (op:find_poa (root-POA) "pns")))
    (op:destroy poa t t)
    (setup-pns)))
