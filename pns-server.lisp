;;;; NameService v2

(in-package :cl-user)

(eval-when (:compile-toplevel :execute)
  (import '(corba:define-method)))



(defvar *naming-base-path*
    (pathname "/tmp/naming/foo.obj"))

(defvar *naming-start-time*
    (- (get-universal-time) 3191179173))

(defvar *naming-seqno* 0)



;;;; NamingContext Servant

(defparameter +naming-context-id+
  "IDL:omg.org/CosNaming/NamingContext:1.0")
(defparameter +name-component-id+
  "IDL:omg.org/CosNaming/NameComponent:1.0")
(defparameter +binding-id+
  "IDL:omg.org/CosNaming/Binding:1.0")



;;;; Naming Context Implementation

;; I could use a default servant to handle all...
;; But I'll use ... to instantiate a servant for every context

;; A naming context is a directory. All names in the context are translated
;; to file names in the directory. The file contains a type marker plus the
;; the stringified IOR of the object as dotted pair.

;; NameComponents are translated to file names by escaping any but
;; safe characters and concatenating the id and kind by an ampersand.

(defvar *naming-poa* nil)

(defclass naming-context (cosnaming:namingcontext-servant)
  ((base :initarg :base :accessor nc-base)))

(define-method _default_POA ((servant naming-context))
  *naming-poa*)

(defparameter +safe-characters-extra+  "_-,")

(defun encode-name (string)
  (with-output-to-string (out)
    (loop for ch across string
          do (if (or (alphanumericp ch)
                     (find ch +safe-characters-extra+))
                 (princ ch out)
                 (format out "%~02x" (char-code ch))))))

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

(defmethod the-orb ((s clorb::auto-servant))
  (clorb::the-orb (op:_default_POA s)))

(define-method new_context ((self naming-context))
  (let ((new-id (format nil "~A" (get-universal-time))))
    (op:create_reference_with_id (op:_default_POA self)
                                 (portableserver:string-to-oid new-id)
                                 (clorb:servant-interface-id self))))

(defun pns-path (nc name-component)
  (merge-pathnames
   (make-pathname
    :name (format nil "~A&~A"
                  (encode-name (op:id name-component))
                  (encode-name (op:kind name-component))))
   (nc-base nc)))

(defun pns-bind (nc n obj bind-type &optional rebind)
  (assert (= 1 (length n)))
  (let ((orb (the-orb nc))
        (name-component (elt n 0)))
    (let ((path (pns-path nc name-component)))
      (ensure-directories-exist path)
      (handler-case
          (with-open-file (out path :direction :output
                               :if-exists (if rebind
                                              :supersede
                                              :error))
            (print bind-type out)
            (print (op:object_to_string orb obj) out))
       (file-error ()
         (error 'COSNAMING:NAMINGCONTEXT/ALREADYBOUND))))))

(defun resolve1 (self n)
  (let* ((component (elt n 0))
         (path (pns-path self component))
         type ior)
    (unless (probe-file path)
      (not-found :MISSING_NODE n))
    (with-open-file (stream path :direction :input)
      (with-standard-io-syntax ()
        (setq type (read stream))
        (setq ior (read stream))))
    (values type (op:string_to_object (the-orb self) ior))))

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


(define-method bind_context ((self naming-context) n nc)
  (pns-bind self n nc :NCONTEXT))

(define-method bind_new_context ((self naming-context) n)
  (if (= 1 (length n))
      (let ((nc (op:new_context self)))
        (op:bind_context self n nc)
        nc)
      (multiple-value-bind (next name)
          (pns-step self n)
        (op:bind_new_context next name))))

(define-method bind ((self naming-context) n obj)
  (if (= 1 (length n))
      (pns-bind self n obj :NOBJECT)
      (multiple-value-bind (next name)
          (pns-step self n)
        (op:bind next name obj))))


(define-method rebind ((self naming-context) n obj)
  (if (= 1 (length n))
      (pns-bind self n obj :NOBJECT t)
      (multiple-value-bind (next name)
          (pns-step self n)
        (op:rebind next name obj))))

(define-method rebind_context ((self naming-context) n nc)
  (if (= 1 (length n))
      (pns-bind self n nc :NCONTEXT t)
      (multiple-value-bind (next name)
          (pns-step self n)
        (op:rebind_context next name nc))))

(define-method resolve ((self naming-context) n)
  (if (= 1 (length n))
      (multiple-value-bind (type obj)
          (resolve1 self n)
        (check-type type COSNAMING:BINDINGTYPE)
        ;; FIXME: should it check? type = :NOBJECT
        obj)
      (multiple-value-bind (next name)
          (pns-step self n)
        (op:resolve next name))))

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
  (if (= 1 (length n))
      (let ((path (pns-path self (elt n 0))))
        (delete-file path))
      (multiple-value-bind (next name)
          (pns-step self n)
        (op:unbind next name))))


;;;; BindingIterator Servant

(defclass binding-iterator (cosnaming:bindingiterator-servant)
  ((bindings :initarg :bindings
	     :initform nil :accessor bindings)))

(define-method next_one ((bi binding-iterator))
  (if (zerop (length (bindings bi)))
      (values nil (cosnaming:binding
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
  (let* ((current (op:resolve_initial_references (the-orb bi) "POACurrent"))
         (poa (op:get_POA current))
         (oid (op:get_object_id current)))
    (op:deactivate_object poa oid)))


;;;; Setup the naming POA

(defclass naming-manager (PortableServer:ServantActivator)
  ())

(define-method incarnate ((m naming-manager) oid adapter)
  (declare (ignore adapter))
  (make-instance 'naming-context
    :base (merge-pathnames
           (make-pathname
            :directory `(:relative ,(portableserver:oid-to-string oid)))
           *naming-base-path*)))

(defun setup-naming-poa ()
  (let* ((orb (CORBA:ORB_init))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))
    (setq *naming-poa*
          (op:create_POA rootPOA "Naming" (op:the_poamanager rootPOA)
                         '(:retain :persistent
                           :user-id :use-servant-manager)))
    (op:set_servant_manager *naming-poa* (make-instance 'naming-manager))))


#|
(setup-naming-poa)
(setq root
      (op:create_reference_with_id
       *naming-poa*
       (portableserver:string-to-oid "root")
       +naming-context-id+))
|#

(defun setup-pns ()
  (setup-naming-poa)
  (with-open-file (out clorb::*name-service*
                       :direction :output
                       :if-exists :supersede)
    (princ
     (op:object_to_string (CORBA:ORB_init)
                          (op:create_reference_with_id
                           *naming-poa*
                           (portableserver:string-to-oid "root")
                           +naming-context-id+))
     out)))

;;(defmethod initialize-instance :after ((self naming-context) &key)
;;  (clorb::mess 3 "Naming Context base=~A" (nc-base self)))
