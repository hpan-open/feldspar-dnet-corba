;;;; The ORB Interface

(in-package :clorb)

(defvar *the-orb* nil)

(defvar *orb-initializers* nil)



;;;  interface ORB {				// PIDL

(define-corba-class ORB ()
  :slots 
  ((adaptor :initform nil :accessor adaptor)
   (active  :initarg :active  :accessor orb-active)
   (host    :initarg :host    :accessor orb-host)
   (port    :initarg :port    :accessor orb-port)
   (initial-references :initform '()
                       :accessor orb-initial-references)))

;; initial ref:
;;  ( name  .  ( string . object  ))

(defun set-initial-reference (orb name string &optional object)
  (let ((old (assoc name (orb-initial-references orb)
                    :test #'string=)))
    (cond (old
           (setf (car (cdr old)) string
                 (cdr (cdr old)) object))
          (t
           (push (cons name (cons string object))
                 (orb-initial-references orb))))))

(defun refresh-initial-references (orb)
  (dolist (ref (orb-initial-references orb))
    (when (cadr ref) (setf (cddr ref) nil))))


;;;    exception InvalidName {}; 
(define-user-exception CORBA:ORB/InvalidName
    :id "IDL:omg.org/CORBA/ORB/InvalidName:1.0")


(defun ORB_init (&optional args (orbid ""))
  (declare (ignore args orbid))
  (unless *the-orb*
    (setq *the-orb* (make-instance 'CORBA:ORB
                      :active t
                      :host *host*
                      :port *port*))
    (when *name-service*
      (set-initial-reference *the-orb* "NameService" *name-service*))
    (when *interface-repository*
      (set-initial-reference *the-orb* "InterfaceRepository"
                             *interface-repository*))
    (dolist (fn *orb-initializers*)
      (funcall fn *the-orb*)))
  (setf (orb-active *the-orb*) t)
  *the-orb*)

;;;    void shutdown( in boolean wait_for_completion );
(define-method shutdown ((orb orb) wait_for_completion)
  (setf (orb-active orb) nil))

(defvar *running-orb* t
  "Will be set to true in the process that is running the ORB server part.
If this is true, orb-wait will check server streams also.
Can be set to true globally for singel-process / development.")



;;;    Object string_to_object (in string str);
(define-method string_to_object ((orb orb) str) 
  (string-to-object orb str))

;;;    string object_to_string (in Object obj);
(define-method object_to_string ((orb orb) obj)
  (declare (ignore orb))
  (object-to-string obj))

(defun object-to-string (objref)
  (format nil
	  "IOR:~{~2,'0X~}"
	  (map 'list #'identity (marshal-make-encapsulation
				 (lambda (buffer) 
                                   (marshal-ior objref buffer))))))


;;;    ObjectIdList list_initial_services (); 
(define-method list_initial_references ((orb orb))
  (mapcar #'car (orb-initial-references orb)))

;;;    Object resolve_initial_references (in ObjectId identifier)
;;;      raises (InvalidName); 
(define-method resolve_initial_references ((orb orb) name)
  (let ((ref-entry
         (assoc name (orb-initial-references orb)
                :test #'string=)))
    (unless ref-entry
      (error 'ORB/InvalidName))
    (let ((obj (cddr ref-entry)))
      (unless obj
        (let ((designator (cadr ref-entry)))
          (cond ((stringp designator)
                 (setf obj (op:string_to_object orb designator)))
                (t
                 (setf obj (funcall designator orb)))))
        (setf (cddr ref-entry) obj))
      obj)))

;;;    boolean work_pending(  );
(define-method work_pending ((orb orb))
  ;; FIXME
  t)

;;;    void perform_work( );
(define-method perform_work ((orb orb))
  (let ((*running-orb* t))
    (orb-wait)))

;;;    void run();
(define-method run ((orb orb))
  (let ((*running-orb* t))
    (loop while (orb-active orb)
        do (orb-wait))))



;;;    Status create_list ( in long    count,	 
;;;                         out NVList new_list );
;;;    Status create_operation_list ( in OperationDef oper, 
;;;                                   out NVList      new_list );
;;;
;;;    Status get_default_context (out Context ctx);
;;;    boolean get_service_information (in ServiceType         service_type,
;;;                                     out ServiceInformation service_information );
;;;    // get_current deprecated operation - should not be used by new code
;;;    // new code should use resolve_initial_reference operation instead
;;;    Current get_current();
;;;    // deprecate get_current in the next major printing of CORBA
;;;
;;;
;;;    typedef string ObjectId;
;;;    typedef sequence <ObjectId> ObjectIdList; 
;;;    



;;;; Parsing Stringified Object Refrences

(defun string-to-object (orb str)
  (multiple-value-bind (method rest)
      (split-url str)
    (cond
      ((null method) (error "Illegal object reference: ~A" str))
      ((string-equal method "corbaloc")
       (multiple-value-bind (addrs key)
           (parse-corbaloc rest)
         (cond
           ((eq (car addrs) :rir)
            (op:resolve_initial_references orb (decode-objkey-string key)))
           (t
            (let ((key (decode-objkey-vector key))
                  (proxy (make-instance 'CORBA:Proxy
                           :id ""
                           :profiles '())))
              (dolist (addr addrs)
                (assert (eq :iiop (car addr)))
                (let ((version (second addr))
                      (host (third addr))
                      (port (fourth addr)))
                  (let ((profile
                         (make-iiop-profile
                          :version version
                          :host host
                          :port port
                          :key key)))
                    (push profile (object-profiles proxy)))))
              proxy)))))
      ;; IOR:xx urls
      ((string-equal method "IOR")
       (unmarshal-encapsulation (decode-hex-string rest) #'unmarshal-ior))
      ;; file:///foo/bar urls
      ((string-equal method "file")
       (let ((filename rest))
         (when (string-starts-with rest "//")
           ;; has host part, ignore
           (setq filename (subseq rest (position #\/ rest :start 2))))
         (let ((ior-string
                (with-open-file (fs filename :direction :input)
                  (read-line fs))))
           (string-to-object orb ior-string))))
      (t
       (error "Unrecognized URL method: ~A" method)))))


(defun split-url (str)
  (let ((method-end (position #\: str)))
    (if method-end
      (values (subseq str 0 method-end)
              (subseq str (+ method-end 1)))
      nil)))

(defun string-starts-with (string prefix)
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))


(defun parse-corbaloc (str)
  ;; str = <obj_addr_list>["/"<key_string>]
  ;; <obj_addr_list> = [<obj_addr> ","]* <obj_addr>
  ;; - URL escaped.
  (let ((key-pos (position #\/ str))
        (key-string ""))
    (when key-pos
      (setq key-string (subseq str (+ key-pos 1)))
      (setq str (subseq str 0 key-pos)))
    (let ((addr-list '())
          (last-pos 0))
      (loop for comma-pos = (position #\, str :start last-pos)
            while comma-pos do 
            (push (subseq str last-pos comma-pos) addr-list)
            (setq last-pos (+ comma-pos 1)))
      (push (subseq str last-pos) addr-list)
      (values 
       (mapcar #'parse-obj-addr addr-list)
       key-string))))


(defun parse-obj-addr (str)
  (multiple-value-bind (prot-token rest)
      (split-url str)
    (cond 
      ((null prot-token) (error "malformed obj-addr"))
      ((string-equal prot-token "rir")
       :rir)
      ((or (string-equal prot-token "iiop")
           (string-equal prot-token ""))
       ;; <iiop_addr> = [<version>] <host> [":" <port>]
       ;; <host> = DNS-style Host Name  or ip_address
       ;; <version> = <major> "." <minor> "@" 
       (let ((version "1.0")
             (port "2809"))
         (let ((pos (position #\@ rest)))
           (when pos
             (setq version (subseq rest 0 pos))
             (setq rest    (subseq rest (+ pos 1)))))
         (let ((pos (position #\: rest)))
           (when pos
             (setq port (subseq rest (+ pos 1)))
             (setq rest (subseq rest 0 pos))))
         (list :iiop (parse-iiop-version version)
               rest (parse-integer port)))))))


(defun decode-hex-string (string)
  (assert (evenp (length string)))
  (let ((ints 
         (loop for i from 0 below (length string) by 2
               collect (parse-integer string :start i :end (+ i 2)
                                      :radix 16))))
    (make-array (length ints)
                :initial-contents ints
                :element-type '(unsigned-byte 8))))

(defun decode-objkey-string (string)
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

(defun decode-objkey-vector (string)
  (let ((out (make-array 50 :adjustable t :fill-pointer 0
                         :element-type 'CORBA:Octet)))
    (loop with state = 0
          for ch across string
          for i from 0
          do (ecase state
               (0 (if (eql ch #\%)
                      (setq state 1)
                      (vector-push-extend (char-code ch) out)))
               (1 (setq state 2)
                  (vector-push-extend 
                   (parse-integer string :start i :end (+ i 2)
                                  :radix 16)
                   out))
               (2 (setq state 0))))
    out))

(defun parse-iiop-version (str)
  (multiple-value-bind (major pos)
      (parse-integer str :junk-allowed t)
    (assert (eq (char str pos) #\.))
    (let ((minor (parse-integer str :start (+ pos 1))))
      (cons major minor))))
