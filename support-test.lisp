;;;; support-test.lisp -- support code for testing CLORB

(in-package :clorb)

(defun ensure-typecode (obj type)
  (ensure-typep obj 'CORBA:TypeCode)
  (typecase type
    (symbol
     (ensure (eql (op:kind obj) type)
             "typecode ~A should be of kind ~A" obj type))
    (CORBA:Typecode
     (ensure (op:equal obj type)
             "Typecode ~A equal to ~A" obj type))))


(defmethod match ((pattern corba:typecode) object)
  (boolean-match pattern object (op:equal object pattern)))



;;;; IR Definition Pattern

(defclass DEF-PATTERN (pattern)
  ((kind :initarg :kind :accessor def-kind)))

(defun def-pattern (kind &rest args)
  (make-instance 'def-pattern :kind kind :args args))

(defmethod match ((pattern def-pattern) def)
  (unless def
    (fail-match def "Missing"))
  (unless (eq (def-kind pattern)
              (op:def_kind def))
    (fail-match def "Wrong definition kind"))
  (call-next-method))


;;;; IR Repository Pattern 

(defclass REPOSITORY-PATTERN (pattern)
  ())

(defun repository-pattern (&rest args)
  (make-instance 'repository-pattern :args args))

(defmethod match ((pattern repository-pattern) object)
  (loop for (name pattern) on (pattern-args pattern) by #'cddr
        do (let ((def (op:lookup object name)))
             (cond ((null def) (fail-match object "has no definition named ~S" name))
                   (t
                    (handler-case 
                      (match pattern def)
                      (match-fail (condition)
                                  (fail-match object "~S ~A" name (match-fail-message condition)))))))))



;;;; Struct Pattern

(defclass STRUCT-PATTERN (pattern)
  ())

(defun struct-pattern (&rest args)
  (make-instance 'struct-pattern :args args))

(defmethod match ((pattern struct-pattern) object)
  (unless (typep object 'CORBA:struct)
    (fail-match object "Not a struct"))
  (call-next-method))

(defun struct-class-name (struct)
  (class-name (class-of struct)))


;;;; ISA Pattern

(defclass isa-pattern (pattern)
  ())

(defun isa (type)
  (make-instance 'isa-pattern :args type))

(defmethod print-object ((p isa-pattern) stream)
  (print-unreadable-object (p stream :type t)
    (prin1 (pattern-args p) stream)))

(defmethod match ((pattern isa-pattern) object)
  (boolean-match pattern object (typep object (pattern-args pattern))))


;;;; Additional ensure operators

(defun std-minor (minor)
  (logior omg.org/corba::omgvmcid minor))

(defmacro ensure-repository (&rest args)
  `(ensure-pattern repository (repository-pattern ,@args)))



;;;; Request / Connection testing

;;; Output, sending requests

(defvar *test-sink-stream*)
(defvar *test-response-desc*)
;;(defvar *test-out-desc*)                ; unused?
(defvar *test-out-conn* nil)

;;; Input, receiving requests

(defvar *test-in-conn*)
(defvar *test-response-sink*)
(defvar *test-request-desc*)


;;; Mock classes

(defclass test-connection (connection)
  ((response-func :initarg response-func 
                  :initform nil
                  :accessor response-func)))

(defmethod connection-write-ready :after ((conn test-connection))
  (let ((fun (response-func conn))
        (req (first (connection-client-requests conn))))
    (when (and fun req)
      (funcall fun req))))


;;; Setup code

(defun make-test-connection ()
  (let ((desc (make-io-descriptor))
        (i-stream (make-octet-stream "i-stream"))
        (o-stream (make-octet-stream "o-stream")))
    (let ((other (make-io-descriptor
                  :stream (make-shortcut-stream (make-octet-stream "dummy") i-stream)
                  :status :connected
                  :shortcut-p desc)))
      (setf (io-descriptor-stream desc) (make-shortcut-stream i-stream o-stream))
      (setf (io-descriptor-shortcut-p desc) other)
      (let ((conn (make-instance 'test-connection
                    :orb (CORBA:ORB_init)
                    :io-descriptor desc)))
        (io-descriptor-associate-connection desc conn)
        (values conn other o-stream)))))


(defun setup-test-out ()
  (multiple-value-setq (*test-out-conn* *test-response-desc* *test-sink-stream*)
    (make-test-connection)))


(defun setup-test-in ()
  (multiple-value-setq (*test-in-conn* *test-request-desc* *test-response-sink*)
    (make-test-connection))
  (setup-incoming-connection *test-in-conn*))
        




;;; Testing functions

(defun test-object (orb)
  "Create a proxy object connected to the testing output connection.
Requests sent to this object will end up in *test-sink-stream*."
  (let ((obj (make-instance 'CORBA:Proxy
               :id "IDL:test:1.0"
               :the-orb orb
               :profiles (list (make-iiop-profile
                                :version '(1 . 0)
                                :host "localhost"
                                :port 9999
                                :key #(17))))))
    (setf (selected-profile obj) (first (object-profiles obj)))
    (setf (object-connection obj) *test-out-conn*)
    obj))

(defun test-read-request (&key 
                             (stream *test-sink-stream*)
                             (orb *the-orb*) buffer 
                             request-pattern request-keys
                             args )
  (unless buffer (setq buffer (get-work-buffer orb)))
  (when request-keys
    (when request-pattern (warn "Can't use both request-pattern and request-keys"))
    (setq request-pattern
          (sexp-pattern `(&key &allow-other-keys
                               ,@(loop for (key val) in request-keys
                                       collect `(,key :required ,val))))))
  (let* ((octets (buffer-octets buffer))
         (reader (funcall stream 'reader)))
    (setf (fill-pointer octets) +iiop-header-size+)
    (assert (funcall reader octets 0 +iiop-header-size+))
    (multiple-value-bind (msgtype version)
                         (unmarshal-giop-header buffer)
      (ensure-eql msgtype :request)
      (let ((size (+ (unmarshal-ulong buffer) +iiop-header-size+)))
        (adjust-array octets size)
        (setf (fill-pointer octets) size)
        (assert (funcall reader octets +iiop-header-size+ size)))
      (let ((request
             (list
              :msgtype msgtype
              :version version
              :service-context (unmarshal-service-context buffer) 
              :request-id (unmarshal-ulong buffer)
              :response (unmarshal-octet buffer)
              :object-key (unmarshal-osequence buffer)
              :operation (unmarshal-string buffer)
              :principal (unmarshal-osequence buffer))))
        (when request-pattern
          (ensure-pattern request request-pattern))
        (dolist (a args)
          (ensure-equalp (unmarshal (corba:any-typecode a) buffer)
                         (corba:any-value a)))
        request))))

(defun test-write-response (req results)
  (setup-outgoing-connection *test-out-conn*)
  (let ((buffer (get-work-buffer (the-orb req))))
    (marshal-giop-header :REPLY buffer)
    (marshal-service-context nil buffer) 
    (marshal-ulong (request-id req)  buffer)
    (marshal :no_exception (symbol-typecode 'GIOP:REPLYSTATUSTYPE) buffer)
    (dolist (any results)
      (marshal-any-value any buffer))
    (marshal-giop-set-message-length buffer)
    (let ((octets (buffer-octets buffer)))
      (io-descriptor-set-write *test-response-desc* octets 0 (length octets)))))


(defun test-write-request (&key
                              (desc *test-request-desc*)
                              (orb *the-orb*)
                              buffer message-type message)
  (check-type message-type integer)
  (unless buffer
    (setq buffer (get-work-buffer orb)))
  (marshal-any-value (giop:messageheader_1_0 :magic "GIOP" 
                                             :giop_version (giop:version :major 1 :minor 0)
                                             :byte_order (buffer-byte-order buffer)
                                             :message_type message-type
                                             :message_size 0)
                     buffer)
  (loop for v in message do
        (marshal-any-value v buffer))
  (marshal-giop-set-message-length buffer)
  (io-descriptor-set-write desc (buffer-octets buffer)
                           0 (length (buffer-octets buffer))))
