;;;; clorb-srvreq.lisp
;; $Id: clorb-srvreq.lisp,v 1.4 2001/06/03 23:59:17 lenst Exp $

(in-package :clorb)


(defstruct server-request
  stream
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

(defun read-request (sreq stream)
  (let* ((buffer (server-request-buffer sreq))
         (octets (buffer-octets buffer)))
    (setf (fill-pointer octets) 12)
    (setf (buffer-position buffer) 0)
    (let ((read-length
           (read-sequence octets stream)))
      (when (< read-length (length octets))
        (break "not read enough")
        (close stream)
        (mess 2 "Closing stream ~A" stream)
        (return-from read-request nil)))
    (setf (server-request-msgtype sreq)
      (unmarshal-giop-header buffer))
    (let ((size (+ (unmarshal-ulong buffer) 12)))
      (mess 1 "Message type ~A size ~A" (server-request-msgtype sreq) size)
      ;; FIXME: check against array-dimension-limit
      (if (< (array-total-size octets) size)
          (adjust-array octets size))
      (setf (fill-pointer octets) size)
      (read-sequence octets stream :start 12)
      (mess 2 "Receive (~D)" size)
      t)))


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
  (with-slots (buffer stream response req-id status values types) sreq
    (when (and (logbitp 0 response)
               (not (eq values 'defer)))
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
               (marshal-multiple values types buffer)))
        (marshal-giop-set-message-length buffer)
        (write-sequence (buffer-octets buffer) stream)
        (mess 3 "#~D reply type ~A status ~A (len ~D)" 
              req-id msg-type status (length (buffer-octets buffer)))
        (force-output stream)))))

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

(define-method op::ctx ((sreq server-request))
  (error "NIY"))

(define-method op::set_result ((sreq server-request) val)
  (setf (server-request-result sreq) val))

(define-method op::set_exception ((sreq server-request) val)
  (setf (server-request-exception sreq) val)
  (set-request-exception sreq val))



;;; clorb-srvreq.lisp ends here
