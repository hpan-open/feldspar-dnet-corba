(in-package :clorb)


;;;; Value


(define-typecode value-typecode
  :kind :tk_value
  :cdr-syntax (complex :tk_string :tk_string :tk_short :tk_typecode 
                       (sequence (:tk_string :tk_typecode :tk_short)))
  :params (id name type_modifier concrete_base_type :members)
  :member-params (member_name member_type member_visibility)
  :constant (corba::TC_ValueBase "IDL:omg.org/CORBA/ValueBase:1.0" "ValueBase"
                                 CORBA::VM_NONE nil ())
  :share named-typecode :shared-params 2
  :extra-slots (truncatable-interfaces 
                member-types all-member-types 
                feature-symbols all-feature-symbols ))


(defmacro define-value (symbol &key id name base_value 
                               is_abstract is_custom is_truncatable
                               supported_interfaces abstract_base_values members)
  (let ((value-bases (append (if base_value (list base_value))
                             abstract_base_values)))
  `(progn
     (defclass ,symbol 
       (,@(or value-bases (list 'CORBA::ValueBase))
        ,@supported_interfaces)
       (,@(loop for (name) in members
                collect `(,(feature name) :initarg ,(key name)))))
     (defmethod shared-initialize ((value ,symbol) slot-names &key factory &allow-other-keys)
       (declare (ignore slot-names))
       (if factory (error 'CORBA:BAD_PARAM))
       (call-next-method))
     (defmethod object-id ((value ,symbol))
       ,id)
     ,@(loop for (name nil nil) in members nconc 
             (list `(define-method ,(feature name) ((value ,symbol))
                      (slot-value value ',(feature name)))
                   `(define-method (setf ,(feature name)) (new (value ,symbol))
                      (setf (slot-value value ',(feature name)) new))))
     (set-symbol-id/typecode 
      ',symbol ,id                      
      (create-value-tc ,id ,name 
                       ,(cond (is_abstract corba:vm_abstract)
                              (is_truncatable corba:vm_truncatable)
                              (is_custom corba:vm_custom)
                              (t corba:vm_none ))
                       ,(if base_value `(symbol-typecode ',base_value))
                       (list ,@(loop for (name tc access) in members
                                     collect `(list ,name ,tc ,access))))))))




(defun truncatable-interfaces (tc)
  (with-cache-slot (tc truncatable-interfaces)
    (if (eql (op:type_modifier tc) corba:vm_truncatable)
      (cons (op:id tc)
            (truncatable-interfaces (op:concrete_base_type tc)))
      (list (op:id tc)))))



;;;; Encoding Support

(defconstant min-value-tag #x7fffff00)
(defconstant max-value-tag #x7fffffff)
(defconstant value-flag-url      #x01)
(defconstant value-flag-repoid   #x02)
(defconstant value-flag-repoids  #x06)
(defconstant value-flag-chunking #x08)



;;;; Indirection Support


(defun marshal-record (obj func buffer)
  (cond ((null obj) (marshal-long 0 buffer))
        (t
         (with-out-buffer (buffer)
           (align 4)
           (let ((old (gethash obj (buffer-record buffer))))
             (cond (old
                    (marshal-long -1 buffer)
                    (marshal-long (- old pos) buffer))
                   (t
                    (setf (gethash obj (buffer-record buffer)) pos)
                    (funcall func obj buffer))))))))


(defun marshal-string-record (string buffer)
  (marshal-record string #'marshal-string buffer))


(defun marshal-string-sequence (strings buffer)
  (marshal-ulong (length strings) buffer)
  (map nil #'marshal-string-record strings (repeated buffer)))


(defvar *unmarshal-record-register*)


(defun unmarshal-record-register (obj)
  (funcall *unmarshal-record-register* obj))


(defun unmarshal-chunk-tag (buffer)
  (when chunking-p
    (unless *chunk-end*
      (let ((tag (without-chunking (buffer) (unmarshal-long buffer))))
        (cond ((< 0 tag min-value-tag)
               (setf *chunk-end* (+ tag (buffer-in-pos buffer)))
               nil)
              (t
               tag))))))

(defconstant signed-indirection-tag -1)
(defconstant unsigned-indirection-tag #xFFFFFFFF)

(defun unmarshal-record (func buffer &optional tag-type &rest args)
  ;; Unmarshal with support for indirection and NULL values
  (let ((tag (ecase tag-type
               (:unsigned (unmarshal-ulong buffer))
               (:chunk-tag (unmarshal-chunk-tag buffer))
               ((:signed nil) (unmarshal-long buffer)))))
    (when (eql tag-type :chunk-tag)
      (cond ((null tag)
             (setq tag (unmarshal-long buffer)))
            ((< tag min-value-tag)
             (mess 5 "Non value-tag")
             (error 'CORBA:MARSHAL))))
    (cond ((zerop tag) nil)
          ((if (eql tag-type :unsigned)
             (= tag unsigned-indirection-tag) 
             (= tag signed-indirection-tag))
           (let ((pos (+ (buffer-in-pos buffer)
                         (unmarshal-long buffer))))
             (or (gethash pos (buffer-record buffer))
                 (error "Illegal indirection"))))
          (t
           (let ((start-pos (- (buffer-in-pos buffer) 4))
                 (registered nil))
             (unless tag-type
               (setf (buffer-in-pos buffer) start-pos))
             (flet ((register (obj)
                      (unless registered
                        (setf registered t)
                        (when obj
                          (setf (gethash start-pos (buffer-record buffer)) obj)))
                      obj))
               (let ((*unmarshal-record-register* #'register))
                 (register (if tag-type 
                             (apply func tag args)
                             (funcall func buffer))))))))))


(defun unmarshal-string-record (buffer)
  (unmarshal-record  #'unmarshal-string buffer))

(defun unmarshal-list (func buffer)
  (loop repeat (unmarshal-long buffer) collect (funcall func buffer)))

(defun unmarshal-string-record-list (buffer)
  (unmarshal-list #'unmarshal-string-record buffer))

(defun unmarshal-string-record-list-record (buffer)
  (unmarshal-record #'unmarshal-string-record-list buffer))



;;;; Marshall Value


(defvar *chunk-tail* nil)

(defvar *chunk-start* nil)

(defun start-chunk (buffer)
  (with-out-buffer (buffer)
    (align 4)
    (setq *chunk-start* pos)
    (incf pos 4)))

(defun end-chunk (buffer)
  (when *chunk-start*
    (with-out-buffer (buffer)
      (if (= *chunk-start* (- pos 4))
        (incf pos -4)                   ; empty chunk, remove
        (let ((old-pos pos))
          (setf pos *chunk-start*)
          (marshal-long (- old-pos pos 4) buffer)
          (setf pos old-pos))))
    (setf *chunk-start* nil)))



(defun marshal-value-header (repoid chunking buffer)
  ;; Write the value header with chunking flag if indicated and
  ;; with repoid, can be NIL - no id, a string, or a list of strings.
  (end-chunk buffer)
  (let ((tag min-value-tag))
    (if chunking (incf tag #x08))
    (when repoid (incf tag (if (consp repoid) #x06 #x02)))
    (marshal-long tag buffer))
  (cond ((consp repoid)
         (marshal-record repoid #'marshal-string-sequence buffer))
        (repoid (marshal-string-record repoid buffer))))


(defun all-member-types (tc)
  (and tc
       (with-cache-slot (tc all-member-types)
         (append (all-member-types (op:concrete_base_type tc))
                 (tc-member-types tc)))))

(defun all-feature-symbols (tc)
  (and tc
       (with-cache-slot (tc all-feature-symbols)
         (append (all-feature-symbols (op:concrete_base_type tc))
                 (tc-feature-symbols tc)))))

(defun all-feature-values (tc value)
  (loop for feature in (all-feature-symbols tc) 
        collect (slot-value value feature)))


(defun marshal-value-state (chunking values types buffer)
  (cond (chunking
         (let ((*chunking-level* (1+ *chunking-level*))
               (*chunk-start* nil))
           (let ((*chunk-tail* nil))
             (loop for (val . more) on values and tc in types
                   do (setq *chunk-tail* (not more))
                   (unless *chunk-start* (start-chunk buffer))
                   (marshal val tc buffer)))
           (end-chunk buffer)
           (unless *chunk-tail*       ; end-tag, unless in tail of enclosing value
             (marshal-long (- *chunking-level*) buffer))))
        (t
         (marshal-multiple values types buffer))))


(defun marshal-value (value buffer &optional expected-id)
  (flet ((marshal-value-1 (value buffer)
           (let ((id (object-id value)))
             (let ((exact-type (equal id expected-id))
                   (value-class (ifr-id-symbol id)))
               (assert value-class)
               (let ((tc (symbol-typecode value-class)))
                 (let ((truncatable (and (not exact-type)
                                         (eql corba:vm_truncatable (op:type_modifier tc)))))
                   (let ((chunking (or truncatable (> *chunking-level* 0)))
                         (repoid (cond (truncatable (truncatable-interfaces tc))
                                       ((not exact-type) id))))
                     (marshal-value-header repoid chunking buffer)
                     (marshal-value-state chunking 
                                          (all-feature-values tc value)
                                          (all-member-types tc)
                                          buffer))))))))
    (marshal-record value #'marshal-value-1 buffer)))


(defmethod marshal (value (tc-formal value-typecode) buffer)
  (marshal-value value buffer (op:id tc-formal)))



;;;; Value factory registry

(defvar *value-factory-registry* (make-hash-table :test #'equal))

(defun lookup-value-factory (id)
  (gethash id *value-factory-registry*))



;;;; Unmarshal Value


(defun unmarshal-value-header (valuetag buffer)
  (cond ((< valuetag min-value-tag)
         (error 'corba:no_implement))
        (t
         (let ((url-flag (logand valuetag #x01))
               (repoid-flags (logand valuetag #x06))
               (chunked-flag (logand valuetag #x08)))
           (assert (zerop (- (logandc2 valuetag min-value-tag)
                             url-flag repoid-flags chunked-flag)))
           (let ((url (unless (zerop url-flag)
                        (unmarshal-string-record buffer)))
                 (repoid
                  (case repoid-flags
                    (0 nil)
                    (2 (unmarshal-string-record buffer))
                    (6 (unmarshal-string-record-list-record buffer))
                    (otherwise (error 'corba:no_implement)))))
             (values (not (zerop chunked-flag)) repoid url))))))
         

(defun unmarshal-value-state (chunked truncate keys types buffer)
  (labels 
    ((read-tag () (unmarshal-long buffer))
     (rewind-tag () (incf (buffer-in-pos buffer) -4))
     (truncating ()
       (unless truncate
         (mess 5 "Truncating a non truncatable value")
         (error 'CORBA:MARSHAL)))
     (truncate-chunk ()
       (when *chunk-end*
         (truncating)
         (setf (buffer-in-pos buffer) *chunk-end*)
         (setf *chunk-end* nil)
         t))
     (find-end-of-value ()
       (loop 
         (or (truncate-chunk)
             (let ((tag (without-chunking (buffer) (read-tag))))
               (cond ((eql tag (- *chunking-level*))
                      (return))
                     ((< (- *chunking-level*) tag 0)
                      ;; end of enclosing value also
                      (rewind-tag)
                      (return))
                     ((< tag (- *chunking-level*))
                      ;; end of sub value presumably
                      (truncating))
                     ((< tag min-value-tag)
                      ;; skip a chunk
                      (truncating)
                      (incf (buffer-in-pos buffer) tag))
                     (t ; value tag
                      ;; need to skip this value
                      (let ((chunked (without-chunking (buffer)
                                       (unmarshal-value-header tag buffer))))
                        (assert chunked)))))))))
    ;; ------------------------------------------------
    (with-in-chunking (chunked)
      (prog1
        (loop for key in keys for tc in types
              collect key collect (unmarshal tc buffer)) 
        (if chunking-p (find-end-of-value))))))



(defun unmarshal-value (buffer &optional expected-id)
  (unmarshal-record #'unmarshal-value-1 buffer :chunk-tag
                    buffer expected-id))


(defun unmarshal-value-1 (valuetag buffer expected-id)
  (assert (or (not chunking-p) (not *chunk-end*)))
  (multiple-value-bind (chunked repoid)
                       (without-chunking (buffer)
                         (unmarshal-value-header valuetag buffer))
    (let (tc symbol truncate)
      (unless repoid
        (setq repoid expected-id))
      ;; check type is known
      (dolist (id (mklist repoid))
        (when (setq symbol (ifr-id-symbol id)) (return))
        (setq truncate t))
      (or symbol (error 'CORBA:NO_IMPLEMENT))
      (if (and truncate (not chunked))
        (error 'CORBA:MARSHAL))
      (or tc (setq tc (symbol-typecode symbol)))
      (let ((value (make-instance (or (lookup-value-factory (op:id tc)) symbol) 
                     :create-for-unmarshal t)))
        (unmarshal-record-register value)
        (let ((keys nil) (types nil))
          (let ((tc-stack nil))
            (do ((base-tc tc (op:concrete_base_type base-tc)))
                ((null base-tc))
              (push base-tc tc-stack))
            (dolist (tc tc-stack)
              (dotimes (i (op:member_count tc))
                (push (key (op:member_name tc i)) keys)
                (push (op:member_type tc i) types))))
          (apply #'reinitialize-instance value
                 :create-for-unmarshal t 
                 (unmarshal-value-state chunked truncate
                                        (nreverse keys)
                                        (nreverse types)
                                        buffer)))
        value))))

(defmethod unmarshal ((tc-formal value-typecode) buffer)
  (unmarshal-value buffer (op:id tc-formal)))



;;;; ValueBox


(define-typecode value_box-typecode
  :kind :tk_value_box
  :cdr-syntax (complex :tk_string :tk_string :tk_typecode)
  :params (id name content_type)
  :share alias-typecode                 ; ? realy share non abstract class ?
  :shared-params 3)


(defclass value-box ()
  ((op::data :initarg :data
             :accessor box-data)))
(define-method data ((box value-box))
  (box-data box))
(define-method (setf data) (new (box value-box))
  (setf (box-data box) new))
(defmethod print-object ((box value-box) stream)
  (print-unreadable-object (box stream :type t :identity t)
    (when (slot-boundp box 'op::data)
      (prin1 (box-data box) stream))))

(defmethod box-data ((box t))
  box)

(defmacro define-value-box (symbol &key id name version original_type type)
  (declare (ignore version))
  `(progn
     (set-symbol-id/typecode
      ',symbol ,id (create-value-box-tc ,id ,name ,original_type))
     ,@(if type
         `((deftype ,symbol ()
             ',type))
         `((defclass ,symbol (value-box) ())
           (defun ,symbol (value) (make-instance ',symbol :data value))))))


(defmethod marshal (box (tc value_box-typecode) buffer)
  (marshal-record box
                  (lambda (box buffer)
                    (let ((chunking (not (zerop *chunking-level*))))
                      (end-chunk buffer)
                      (marshal-value-header (op:id tc) chunking buffer)
                      (marshal-value-state chunking
                                           (list (box-data box))
                                           (list (op:content_type tc))
                                           buffer)))
                  buffer))

(defmethod unmarshal ((tc value_box-typecode) buffer)
  (unmarshal-record
   (lambda (tag)
     (multiple-value-bind (chunked repoid)
                          (without-chunking (buffer)
                            (unmarshal-value-header tag buffer))
       (assert (or (null repoid)
                   (equal (car (mklist repoid)) (op:id tc))))
       (let ((initargs (unmarshal-value-state
                        chunked nil '(:data) (list (op:content_type tc)) buffer)))
         (typecase (second initargs)
           ((or number character)
            (apply #'make-instance (or (ifr-id-symbol (op:id tc)) 'value-box)
                   initargs))
           (t (second initargs))))))
   buffer :chunk-tag))



;;;; AbstractInterface


(define-typecode abstract_interface-typecode
  :kind :tk_abstract_interface
  :cdr-syntax (complex :tk_string :tk_string)
  :params (id name)
  :share named-typecode :shared-params 2)


(defmacro define-abstract-interface (symbol super &key (id "") proxy (name "") mixin)
  `(progn
     (set-symbol-id/typecode ',symbol ,id (create-abstract-interface-tc ,id ,name))
     (setf (get ',symbol 'ifr-bases) ',super)
     (defclass ,mixin () ())
     (defclass ,symbol (,mixin ,@super) ())
     ,@(if proxy
         `((defclass ,(car proxy) (,mixin ,@(cdr proxy)) ())
           (register-proxy-class ,id ',(car proxy))))
     (defmethod object-id ((obj ,mixin))
       ,id)
     (defmethod object-is-a or ((obj ,mixin) interface-id)
       (string= interface-id ,id))))
  

(defmethod marshal (obj (tc abstract_interface-typecode) buffer)
  (assert (or (null obj) (op:_is_a obj (op:id tc))))
  (etypecase obj
    (CORBA:Proxy
     (marshal-bool t buffer)
     (marshal-object obj buffer))
    ((or NULL CORBA:ValueBase)
     (marshal-bool nil buffer)
     (marshal-value obj buffer))
    ;; this is doubtful as the spec says requires that
    ;; "the object is already registered with the ORB/OA"
    (CORBA:Object
     (marshal-bool t buffer)
     (marshal-object obj buffer))))


(defmethod unmarshal ((tc abstract_interface-typecode) buffer)
  (let ((case-label (unmarshal-bool buffer)))
    (if case-label
      (unmarshal-object buffer (op:id tc))
      (unmarshal-value buffer))))
