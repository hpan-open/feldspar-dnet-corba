;;;; Auto-Dynamic servant (CLORB specific)

;;; This will use the interface repository or a local interface-def
;;; object to marshal requests parameters and invoke operations.

(defclass auto-servant (servant)
  ())

(defvar *current-server-request* nil
  "While operations are invoked on a servant, this variable will
be bound to the server-request.")

(defmethod servant-interface-id ((servant auto-servant))
  "Override to specifify interface by ID"
  nil)

(defmethod servant-interface ((servant auto-servant))
  "Override this if there is a ready interface-def object."
  (get-interface (servant-interface-id servant)))

(defmethod primary-interface ((servant servant) oid poa)
  (declare (ignore oid poa))
  (or (servant-interface-id servant)
      (op:id (servant-interface servant))))

;;; ----------------------------------------------------------------------

(defun operation-symbol (string)
  (intern (string-upcase string) :op))

(defun servant-opinfo (servant operation)
  (let ((opdef       
         (find-opdef *object-interface* operation)))
    (if opdef 
        (values 
         (if (equal operation "_interface")
             'op:_get_interface 
           (operation-symbol operation))
         (opdef-inparam-typecodes opdef)
         (opdef-outparam-typecodes opdef))
      (find-opinfo (servant-interface servant) operation))))

(defmethod find-opinfo ((interface interface) operation)
  (let ((opdef (find-opdef interface operation)))
    (when opdef
      (multiple-value-bind (name type)
          (analyze-operation-name operation)
        (values (case type
                  (:setter 
                   (fdefinition (list 'setf (operation-symbol name))))
                  (:getter 
                   (operation-symbol name))
                  (otherwise
                   (operation-symbol operation)))
                (opdef-inparam-typecodes opdef)
                (opdef-outparam-typecodes opdef))))))

(defmethod find-opinfo ((interface interface-def) operation)
  (multiple-value-bind (name type)
      (analyze-operation-name operation)
    (let ((def (op:lookup interface name))
          (sym (intern (string-upcase name) :op)))
      (case type
        (:setter
         (assert (eq (op:def_kind def) :dk_Attribute))
         (values (fdefinition (list 'setf sym))
                 (list (op:type def))
                 nil))
        (:getter
         (assert (eq (op:def_kind def) :dk_Attribute))
         (values sym
                 nil
                 (list (op:type def))))
        (otherwise
         (assert (eq (op:def_kind def) :dk_Operation))
         (values sym
                 (opdef-inparam-typecodes def)
                 (opdef-outparam-typecodes def)))))))

(defmethod servant-invoke ((servant auto-servant) sreq)
  (let* ((operation (server-request-operation sreq)))
    (mess 3 "servant-invoke ~S ~S" servant operation)
    (mess 1 "sreq: service-context=~S" (server-request-service-context sreq))
    (multiple-value-bind (method inparams outparams)
        (servant-opinfo servant operation)
      (let ((args (unmarshal-multiple inparams (server-request-buffer sreq))))
        (unless (or (functionp method)
                    (fboundp method))
          (error 'CORBA:NO_IMPLEMENT))
        (let* ((*current-server-request* sreq)
               (res (multiple-value-list
                     (apply method servant args))))
          (cond ((equal res '(defer))
                 (set-request-result sreq 'defer))
                (t
                 (set-request-result sreq res outparams))))))))


(define-method _get_interface ((servant auto-servant))
  (let ((i (servant-interface servant)))
    ;; As long as we still have the IIR with its own interface struct
    (if (or (typep i 'CORBA:Object))
        i
      (call-next-method))))

(define-method _is_a ((servant auto-servant) interface-id)
  (or (equal interface-id (interface-id *object-interface*))
      (op:is_a (servant-interface servant) interface-id)))



(defmethod servant-interface :around ((servant auto-servant))
  (let ((id (servant-interface-id servant)))
    (or (and id (op:lookup_id *idef-repository* id))
        (call-next-method))))

