;;;; pns-server.lisp -- Persistent NameService v2.1


(in-package "NET.CDDR.CLORB.PERSISTENT-NAMING")



;;;; Generating ID

(defvar *naming-start-time*
  (- (get-universal-time) 3244357381))

(defvar *naming-seqno* 0)

(defun generate-id ()
  (format nil "~A-~A" *naming-start-time* (incf *naming-seqno*)))



;;;; Naming Context Implementation

;; I could use a default servant to handle all...
;; But I'll use a ServantActivator to instantiate a servant for every context

;; A naming context is a directory. All names in the context are translated
;; to file names in the directory. The file contains a type marker plus the
;; the stringified IOR of the object.

;; NameComponents are translated to file names by escaping any but
;; safe characters and concatenating the id and kind by an ampersand.


(defclass NAMING-CONTEXT (cosnaming:namingcontextext-servant)
  ((base :initarg :base :accessor nc-base)
   (local-id :initarg :local-id :accessor local-id)
   (poa :initarg :poa   :accessor poa-of
        :initform (error "Missing :poa argument"))))


(define-method _default_POA ((servant naming-context))
  (poa-of servant))


(defmethod stored-string-id ((obj CORBA:Proxy))
  (op:object_to_string (clorb::the-orb obj) obj))

(defmethod stored-string-id ((obj naming-context))
  (local-id obj))


(defparameter +safe-characters-extra+  "_-,")

(defun encode-name (string)
  (with-output-to-string (out)
    (loop for ch across string
          do (if (or (alphanumericp ch)
                     (find ch +safe-characters-extra+))
                 (princ ch out)
                 (format out "%~2,'0x" (char-code ch))))))

(defun decode-name (string)
  (with-output-to-string (out)
    (loop with state = 0
          for ch across string
          for i from 0
          do (ecase state
               (0 (if (eql ch #\%)
                      (setq state 1)
                      (princ ch out)))
               (1 (setq state 2)
                  (princ (code-char
                          (parse-integer string :start i :end (+ i 2)
                                         :radix 16))
                         out))
               (2 (setq state 0))))))


;;;; Method code

(defun not-found (why rest-of-name)
  (check-type why COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
  ;;(check-type rest-of-name (SEQUENCE COSNAMING:NAMECOMPONENT))
  (error 'COSNAMING:NAMINGCONTEXT/NOTFOUND
         :why why
         :rest_of_name rest-of-name))



(define-method new_context ((self naming-context))
  (let ((new-id (generate-id)))
    (op:create_reference_with_id (op:_default_POA self)
                                 (portableserver:string-to-oid new-id)
                                 (clorb::object-id self))))

(defun pns-path (nc name-component)
  (merge-pathnames
   (make-pathname
    :name (format nil "~A&~A"
                  (encode-name (op:id name-component))
                  (encode-name (op:kind name-component))))
   (nc-base nc)))

(defun pns-bind (nc n obj bind-type &optional rebind)
  (assert (= 1 (length n)))
  (let ((name-component (elt n 0)))
    ;; FIXME: if rebind, should check the type of (possibly) already existing binding
    (let ((path (pns-path nc name-component)))
      (ensure-directories-exist path)
      (handler-case
          (with-open-file (out path :direction :output
                               :if-exists (if rebind
                                              :supersede
                                              :error))
            (print bind-type out)
            (print (if (eql bind-type :ncontext)
                       (stored-string-id (opt-local (op:_poa nc) obj))
                       (stored-string-id obj))
                   out))
       (file-error ()
         (error 'COSNAMING:NAMINGCONTEXT/ALREADYBOUND))))))

(defun resolve1 (self n)
  (let* ((component (elt n 0))
         (path (pns-path self component))
         type ior)
    (unless (probe-file path)
      (not-found :missing_node n))
    (with-open-file (stream path :direction :input)
      (with-standard-io-syntax ()
        (setq type (read stream))
        (setq ior (read stream))))
    (values type
      (if (clorb::string-starts-with ior "IOR:")
          (op:string_to_object (op:_orb self) ior)
          ;; ior is actually only local id, create a reference with that id
          (op:create_reference_with_id (op:_default_POA self)
                                       (portableserver:string-to-oid ior)
                                       (clorb::object-id self))))))



(defun opt-local (poa obj)
  (let (oid)
    (handler-case
        (progn
          (setq oid (op:reference_to_id poa obj))
          (op:id_to_servant poa oid))
     (PortableServer:POA/WrongAdapter ()
       obj)
     (PortableServer:POA/ObjectNotActive ()
       ;; Need to activate it...
       (let ((servant (op:incarnate (op:get_servant_manager poa) oid poa)))
         (op:activate_object_with_id poa oid servant)
         servant)))))


(defun pns-step (self n)
  (assert (> (length n) 1))
  (multiple-value-bind (type obj)
      (resolve1 self n)
    (unless (eq type :ncontext)
      (not-found :not_context n))
    (values
     (opt-local (op:_default_POA self) obj)
     (subseq n 1))))


(defun pns-step-do (self n local remote)
  (if (= 1 (length n))
    (funcall local)
    (multiple-value-bind (next name)
                         (pns-step self n)
      (unless (typep next 'CosNaming:NamingContext)
        (setq next 
              (or (op:narrow 'CosNaming:NamingContextExt next :no-error)
                  (op:narrow 'CosNaming:NamingContext next))))
      (funcall remote next name))))


(define-method bind_context ((self naming-context) n nc)
  (unless nc
    (error 'CORBA:BAD_PARAM))
  (pns-step-do self n
               (lambda () (pns-bind self n nc :ncontext))
               (lambda (next name) (op:bind_context next name nc))))

(define-method bind_new_context ((self naming-context) n)
  (pns-step-do self n
               (lambda ()
                 (let ((nc (op:new_context self)))
                   (op:bind_context self n nc)
                   nc))
               #'op:bind_new_context))

(define-method bind ((self naming-context) n obj)
  (pns-step-do self n
               (lambda () (pns-bind self n obj :nobject))
               (lambda (next name) (op:bind next name obj))))

(define-method rebind ((self naming-context) n obj)
  (clorb::mess 2 "rebind obj=~a" obj)
  (pns-step-do self n
               (lambda () (pns-bind self n obj :nobject t))
               (lambda (next name) (op:rebind next name obj))))

(define-method rebind_context ((self naming-context) n nc)
  (unless nc
    (error 'CORBA:BAD_PARAM))
  (pns-step-do self n
               (lambda () (pns-bind self n nc :ncontext t))
               (lambda (next name) (op:rebind_context next name nc))))

(define-method resolve ((self naming-context) n)
  (pns-step-do self n
               (lambda ()
                 (multiple-value-bind (type obj) (resolve1 self n)
                   (check-type type COSNAMING:BINDINGTYPE)
                   ;; should it check? type = :NOBJECT [NO!]
                   obj))
               #'op:resolve))

(define-method list ((self naming-context) how-many)
  (let* ((bindings
          (loop
           for pn in (directory (make-pathname :name :wild
                                               :defaults (nc-base self)))
           collect
           (let* ((fname (pathname-name pn))
                  (split-pos (position #\& fname))
                  (id (decode-name (subseq fname 0 split-pos)))
                  (kind (decode-name (subseq fname (1+ split-pos))))
                  (type (with-open-file (stream pn :direction :input)
                          (with-standard-io-syntax (read stream)))))
             (Cosnaming:Binding
              :binding_name (list (Cosnaming:Namecomponent :id id :kind kind))
              :binding_type type))))
         (result bindings)
         rest)
    (when (> (length bindings) how-many)
      (setq result (subseq bindings 0 how-many)
            rest   (subseq bindings how-many)))
    (values
     result
     (if rest
         (make-instance 'binding-iterator
           :bindings rest)))))

(define-method unbind ((self naming-context) n)
  (pns-step-do self n
               (lambda () (let ((path (pns-path self (elt n 0))))
                            (delete-file path)))
               #'op:unbind))



;;;;  interface NamingContextExt : NamingContext {
;;    typedef string StringName;
;;    typedef string Address;
;;    typedef string URLString;

;;;    StringName to_string (in Name n)
;;      raises(InvalidName);

(define-method to_string ((self naming-context) n)
  (with-output-to-string (*standard-output*) 
    (let ((first t))
      (map nil (lambda (nc)
                 (unless first (princ "/"))
                 (print-escape-name (op:id nc))
                 (princ ".")
                 (print-escape-name (op:kind nc))
                 (setq first nil))
           n))))

(defun print-escape-name (string)
  (loop for c across string
        do (case c
             ((#\/ #\. #\\)
              (princ #\\)))
        (princ c)))


;;;    Name to_name (in StringName sn)
;;      raises(InvalidName);

(define-method to_name ((self naming-context) sn)
  (clorb::ns-name sn))


;;;    exception InvalidAddress {};

;;;    URLString to_url (in Address addr, in StringName sn)
;;      raises (InvalidAddress, InvalidName);

;FIXME!

;;;    Object resolve_str (in StringName n)
;;      raises (NotFound, CannotProceed, InvalidName, AlreadyBound);

(define-method resolve_str ((self naming-context) n)
  (op:resolve self (clorb::ns-name n)))




;;;; BindingIterator Servant

(defclass BINDING-ITERATOR (cosnaming:bindingiterator-servant)
  ((bindings :initarg :bindings
	     :initform nil :accessor bindings)))

(defvar *null-binding* (cosnaming:binding
                        :binding_name nil
                        :binding_type :nobject))

(define-method next_one ((bi binding-iterator))
  (setf (bindings bi) (coerce (bindings bi) 'list))
  (if (bindings bi)
      (values t (pop (bindings bi)))
    (values nil *null-binding*)))

(define-method next_n ((bi binding-iterator) n)
  (if (zerop (length (bindings bi)))
      (values nil nil)
    (values t (let* ((bindings (bindings bi))
                     (end (min n (length bindings)))
                     (res (subseq bindings 0 end)))
                (setf (bindings bi) (subseq bindings end))
                res))))

;; void destroy () raises (NotEmpty);

(define-method destroy ((bi binding-iterator))
  (let* ((current (op:resolve_initial_references (op:_orb bi) "POACurrent"))
         (poa (op:get_POA current))
         (oid (op:get_object_id current)))
    (op:deactivate_object poa oid)))



;;;; Naming POA


(defclass NAMING-MANAGER (portableserver:servantactivator)
  ((base-path   :initarg :base-path     :accessor base-path-of)))


(define-method incarnate ((m naming-manager) oid adapter)
  (let ((local-id (portableserver:oid-to-string oid)))
    (make-instance 'naming-context 
                   :poa adapter
                   :local-id local-id
                   :base (merge-pathnames
                          (make-pathname :directory `(:relative ,local-id))
                          (base-path-of m)))))


(defun create-naming-poa (orb root-poa base-path poa-name)
  (declare (ignore orb))
  (let ((policies
         (list (op:create_lifespan_policy root-poa :persistent)
               (op:create_servant_retention_policy root-poa :retain)
               (op:create_id_assignment_policy root-poa :user_id)
               (op:create_request_processing_policy root-poa :use_servant_manager))))
    (let ((naming-poa
           (op:create_poa root-poa poa-name (op:the_poamanager root-poa) policies)))
      (setq base-path (make-pathname :type "obj" :defaults base-path))
      (op:set_servant_manager naming-poa
                              (make-instance 'naming-manager
                                             :base-path base-path))
      naming-poa)))



;;;; Create Naming Service Instance


(defun create-root-context (poa)
  (op:create_reference_with_id
   poa
   (portableserver:string-to-oid "root")
   (op:id cosnaming::_tc_namingcontextext)))


(defun create-pns (&key base-path poa-name orb
                   (root-poa (op:resolve_initial_references orb "RootPOA")))
  "Create an instance of the Persistent Naming Service.
  :orb       -- ORB Instance,
  :base-path -- Pathname of directory where names are stored,
  :poa-name  -- Name of POA used,
  :boot-name -- (optional) name for bootstrap reference,
  :root-poa  -- (optional) Parent POA to use [default RootPOA]."
  (setq base-path (pathname base-path))
  (check-type poa-name string)
  (check-type orb CORBA:ORB)
  (check-type root-poa PortableServer:POA)

  (create-root-context
   (create-naming-poa orb root-poa base-path poa-name)))



;;;; Global Instance

;;; Options

(defvar *naming-base-path* (merge-pathnames
                            (make-pathname :type "obj" :directory '(:relative "naming"))
                            clorb::*clorb-pathname-defaults*))

(defvar *poa-name* "Naming")

(defvar *boot-name* "NameService")

(defvar *naming-ior-file*  nil)


;;; Global Instance

(defvar *root-context* nil)


;;; Setup Code

(defun setup-pns (&key (orb (CORBA:ORB_init))
                  export
                  (base-path *naming-base-path*)
                  (poa-name *poa-name*)
                  (boot-name *boot-name*))
  (check-type poa-name string)
  (check-type boot-name (or null string))
  (unless *root-context*
    (setq *root-context*
          (create-pns :orb orb :poa-name poa-name :base-path base-path )))
  (when export
    (clorb:set-boot-object orb boot-name *root-context*))
  (when *naming-ior-file*
      (with-open-file (out *naming-ior-file*
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb *root-context*) out)))

  *root-context*)


;;(defmethod initialize-instance :after ((self naming-context) &key)
;;  (clorb::mess 3 "Naming Context base=~A" (nc-base self)))


(defun pns-initial-ref (orb)
  (when *naming-base-path*
    (net.cddr.clorb::set-initial-reference orb "CLORB-PNS"
                                           (lambda (orb)
                                             (setup-pns :orb orb)))))


(pushnew 'pns-initial-ref net.cddr.clorb::*orb-initializers*)
(when net.cddr.clorb::*the-orb*
  (pns-initial-ref net.cddr.clorb::*the-orb*))
