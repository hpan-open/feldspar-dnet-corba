;;;; The ORB Interface

(in-package :clorb)

(defvar *the-orb* nil)

(defvar *default-initial-references*
    `(("NameService" 
       . ,(lambda (orb) (object-from-var orb *name-service*)))
      ("InterfaceRepository" 
       . ,(lambda (orb) (object-from-var orb *interface-repository*)))))


;;;  interface ORB {				// PIDL

(define-corba-class ORB ()
  :slots 
  ((adaptor :initform nil :accessor adaptor)
   (active  :initarg :active  :accessor orb-active)
   (initial-references :initarg :initial-references
                       :accessor orb-initial-references)))

;;;    exception InvalidName {}; 
(define-user-exception CORBA:ORB/InvalidName
    :id "IDL:omg.org/CORBA/ORB/InvalidName:1.0")

;;; (Object) Adaptor interface
(defgeneric listner-sockets (adaptor))
(defgeneric handle-socket (adaptor socket &optional blocking))
(defgeneric client-streams (adaptor))
(defgeneric handle-stream (adaptor stream))
(defgeneric listner-host (adaptor))
(defgeneric listner-port (adaptor))


(defun ORB_init (&optional args (orbid ""))
  (declare (ignore args orbid))
  (unless *the-orb*
    (setq *the-orb* (make-instance 'CORBA:ORB
                     :active t
                     :initial-references *default-initial-references*)))
  (setf (orb-active *the-orb*) t)
  *the-orb*)

;;;    void shutdown( in boolean wait_for_completion );
(define-method shutdown ((orb orb) wait_for_completion)
  (setf (orb-active orb) nil))

(defvar *running-orb* nil
  "Will be set to true in the process that is running the ORB server part.
If this is true, orb-wait will check server streams also.
Can be set to true globally for singel-process / development.")


(defun orb-wait (orb &optional streams)
  "Wait till input available on any of the STREAMS.
STREAMS can be a list of streams or a single stream. The functions will 
also check if anything is to be done on server side.
Returns 
 nil, if nothing available (yet)
 a stream, if input available from that stream (member of STREAMS)
 :cant, if implementation can't determine if input is available.
"
  (let ((client-streams '())
        (source-streams '())
        (server-sockets '())
        (adaptor (adaptor orb)))
    (when (and *running-orb* adaptor)
      (setq source-streams (setq client-streams (client-streams adaptor)))
      (setq server-sockets (listner-sockets adaptor)))
    (if (listp streams)
        (setq source-streams (append streams source-streams))
        (push streams source-streams))
    (multiple-value-bind (type stream)
        (wait-for-input-on-streams server-sockets source-streams)
      (ecase type
        (:stream
         (if (member stream client-streams)
             (progn
               (handle-stream adaptor stream)
               nil)
             stream))
        (:server
         (handle-socket adaptor stream)
         nil)
        (:cant 
         (when (and *running-orb* adaptor (null streams))
           ;; Should do server things, but don't know if there is
           ;; anything to do : (
           (let ((anything nil))
             (dolist (stream client-streams)
               (mess 1 "Poll ~S" stream)
               (when (handle-stream adaptor stream)
                 (setq anything t)))
             (dolist (socket server-sockets)
               (handle-socket adaptor socket (not anything)))))
         :cant)
        ((nil) nil)))))

;;;    Object string_to_object (in string str);
(define-method string_to_object ((orb orb) str) 
  (declare (ignore orb))
  (string-to-object str))

;;;    string object_to_string (in Object obj);
(define-method object_to_string ((orb orb) obj)
  (declare (ignore orb))
  (object-to-string obj))

(defun string-to-object (str)
  (unless (string-equal "IOR:" str :end2 4)
    ;;(wildcard:match "IOR:*" str)
    (error "Illegal object reference: ~A" str))
  (let ((ints
	 (loop for i from 4 below (length str) by 2
	       collect (parse-integer str :start i :end (+ i 2) :radix 16))))
    (unmarshal-encapsulation
     (make-array (length ints)
                 :initial-contents ints
                 :element-type '(unsigned-byte 8))
     #'unmarshal-ior)))

(defun object-to-string (objref)
  (format nil
	  "IOR:~{~2,'0X~}"
	  (map 'list #'identity (marshal-make-encapsulation
				 (lambda (buffer) 
                                   (marshal-ior objref buffer))))))

(defun file-to-object (orb file)
  (op:string_to_object orb
                        (with-open-file (fs file :direction :input)
                          (read-line fs))))

(defun object-from-var (orb refstring)
  (cond
   ((and (stringp refstring)
         (> (length refstring) 4)
         (string-equal "IOR:" refstring :end2 4))
    (op:string_to_object orb refstring))
   (t
    (file-to-object orb refstring))))


;;;    ObjectIdList list_initial_services (); 
(define-method list_initial_references ((orb orb))
  (mapcar #'car (orb-initial-references orb)))

;;;    Object resolve_initial_references (in ObjectId identifier)
;;;      raises (InvalidName); 
(define-method resolve_initial_references ((orb orb) name)
  (let ((ref-entry
         (assoc name (orb-initial-references orb)
         :test #'equal)))
    (unless ref-entry
      (error 'ORB/InvalidName))
    (funcall (cdr ref-entry) orb)))

;;;    boolean work_pending(  );
(define-method work_pending ((orb orb))
  ;; FIXME
  t)

;;;    void perform_work( );
(define-method perform_work ((orb orb))
  (let ((*running-orb* t))
    (orb-wait orb nil)))

;;;    void run();
(define-method run ((orb orb))
  (let ((*running-orb* t))
    (loop while (orb-active orb)
        do (orb-wait orb nil))))

(defun orb-host (orb)
  (listner-host (adaptor orb)))

(defun orb-port (orb)
  (listner-port (adaptor orb)))

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




