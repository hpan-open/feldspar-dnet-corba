;;;; clorb-srvreq.lisp
;; $Id: clorb-srvreq.lisp,v 1.9 2002/05/02 20:40:11 lenst Exp $

(in-package :clorb)


(defstruct server-request
  connection
  ;; Actual message
  buffer
  ;; Header info
  msgtype
  iiop-version
  service-context
  req-id
  response
  object-key
  operation
  principal
  ;; After POA demux
  poa oid
  ;; Servant...
  (servant nil)
  (argument :undecoded)
  opdef
  ;; After invokation
  (status 0)
  values
  types
  ;; Support for IDL/Lisp mapping
  result                                ; Any
  (arguments :unset)                    ; NVList
  exception)


;;;; Reading and Decoding

(defun read-request (sreq decode-fun)
  ;; Called when message header has been read
  (let* ((buffer (server-request-buffer sreq))
         (conn (server-request-connection sreq)))
    (multiple-value-bind (msgtype iiop-version) (unmarshal-giop-header buffer)
      (setf (server-request-msgtype sreq) msgtype)
      (setf (server-request-iiop-version sreq) iiop-version))
    (let ((size (+ (unmarshal-ulong buffer) *iiop-header-size*)))
      (mess 1 "Message type ~A size ~A" (server-request-msgtype sreq) size)
      (connection-init-read conn t size decode-fun))))


(defun decode-request (sreq)
  (let ((buffer (server-request-buffer sreq)))
    (with-slots
        (service-context req-id response object-key operation principal)
        sreq
      (ecase (server-request-msgtype sreq)
        (0
         (setf service-context (unmarshal-service-context buffer))
         (setf req-id (unmarshal-ulong buffer))
         (setf response (unmarshal-octet buffer))
         (setf object-key (unmarshal-osequence buffer))
         (setf operation (unmarshal-string buffer))
         (setf principal (unmarshal-osequence buffer))
         (mess 3 "#~D op ~A on '~/clorb:stroid/' from '~/clorb:stroid/'"
               req-id operation object-key principal))
        (3
         (setf req-id (unmarshal-ulong buffer))
         (setf object-key (unmarshal-osequence buffer))
         (setf operation 'locate)
         (setf response 1))))))


;;;; Setting response

(defun set-request-forward (sreq object)
  (with-slots (status values types) sreq
    (setf status 4)
    (setf values (list object))
    (setf types  '(:tk_string))))


(defun set-request-result (sreq results &optional types &key (status 0))
  (when (and types (/= (length types) (length results)))
    (mess 5 "set-request-result: results#~A types#~A"
          (length results) (length types)))
  (setf (server-request-status sreq) status)
  (setf (server-request-values sreq) results)
  (setf (server-request-types sreq)  types))


(defun set-request-exception (sreq condition)
  (when (eq 'locate (server-request-operation sreq))
    (set-request-result sreq nil nil
                        :status
                        (if (typep condition 'CORBA:OBJECT_NOT_EXIST)
                            0
                          1))
    (return-from set-request-exception))
  (when (typep condition 'userexception)
    ;; FIXME: Check that the exeception is defined for the op
    (set-request-result
     sreq
     (cons (exception-id condition)
           (userexception-values condition))
     (cons ':tk_string
           (if (and (slot-exists-p condition 'types)
                    (slot-boundp condition 'types))
               (slot-value condition 'types)
             (handler-case
                 (map 'list #'second
                      (third (typecode-params
                              (get-typecode (exception-id condition)))))
               (error (exc)
                 (mess 8 "~A: ~A" condition exc)
                 (setf condition
                   (make-condition 'corba:unknown :minor 1008))))))
     :status 1))
  (when (not (typep condition 'exception))
    (mess 8 "Unknown exception: ~A" condition)
    (setf condition (make-condition 'corba:unknown :minor 1009)))
  (when (typep condition 'corba:systemexception)
    (set-request-result sreq
                        (list (exception-id condition)
                              (system-exception-minor condition)
                              (system-exception-completed condition))
                        (list :tk_string
                              :tk_ulong
                              OMG.ORG/CORBA::TC_completion_status)
                        :status 2)))

;;;; Sending response

(defun send-response (sreq)
  (unless (or (not (zerop (server-request-status sreq)))
              (eq (server-request-arguments sreq) :unset))
    (decode-arguments-for-output sreq))
  (with-slots (response req-id status) sreq
    (when (and (logbitp 0 response)
               (not (eq (server-request-values sreq) 'defer)))
      (let ((buffer (get-work-buffer))
            (msg-type nil))
        (cond ((eq (server-request-operation sreq) 'locate)
               (setq msg-type 4)
               (marshal-giop-header msg-type buffer)
               (marshal-ulong req-id buffer)
               (marshal-ulong status buffer))
              (t
               (setq msg-type 1)
               (marshal-giop-header msg-type buffer)
               (marshal-ulong 0 buffer) ; service-context
               (marshal-ulong req-id buffer)
               (marshal-ulong status buffer)
               (marshal-multiple (server-request-values sreq)
                                 (server-request-types sreq)
                                 buffer)))
        (marshal-giop-set-message-length buffer)
        (connection-send-buffer (server-request-connection sreq)
                                buffer)
        (mess 3 "#~D reply type ~A status ~A (len ~D)"
              req-id msg-type status (length (buffer-octets buffer)))))))


(defun decode-arguments-for-output (sreq)
  (with-slots (arguments result values types) sreq
    (loop for arg in arguments
        unless (zerop (logand ARG_OUT (op:arg_modes arg)))
        collect (any-value (op:argument arg)) into a-values
        collect (any-typecode (op:argument arg)) into a-types
        finally (setf types a-types
                      values a-values))
    (when result
      (let ((res-type (any-typecode result))
            (res-value (any-value result)))
        (unless (eq (typecode-kind res-type) :tk_void)
          (push res-type types)
          (push res-value values))))))



;;;; The standard interface

;;  interface ServerRequest {			         // PIDL
;;    readonly attribute Identifier operation;
;;    void arguments(inout NVList nv);
;;    Context ctx();
;;    void set_result(in Any val);
;;    void set_exception(in Any val);
;;  };

(define-method operation ((sreq server-request))
  (server-request-operation sreq))

(define-method arguments ((sreq server-request) nvlist)
  ;; Presumably input is the types and output the values
  ;; The types are encoded in CORBA:Any embeded in NamedValue
  ;; Should only be called once
  (unless (eq (server-request-arguments sreq) :unset)
    (error 'BAD_INV_ORDER))
  (setf (server-request-arguments sreq) nvlist)
  (doseq (nv nvlist)
    (cond
     ((/= 0 (logand ARG_IN (op:arg_modes nv)))
      (setf (any-value (op:argument nv))
        (unmarshal (any-typecode (op:argument nv))
                   (server-request-buffer sreq))))
     ((null (op:argument nv))
      (setf (op:argument nv) (CORBA:Any)))))
  nvlist)

(define-method ctx ((sreq server-request))
  (error "NIY"))

(define-method set_result ((sreq server-request) val)
  (setf (server-request-result sreq) val))

(define-method set_exception ((sreq server-request) val)
  (setf (server-request-exception sreq) val)
  (set-request-exception sreq val))



;;; clorb-srvreq.lisp ends here
