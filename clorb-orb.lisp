;;;; The ORB Interface

(in-package :clorb)


(defvar *the-orb* nil)

(defvar *default-initial-references*
    `(("NameService" 
       . ,(lambda (orb) (object-from-var orb *name-service*)))
      ("InterfaceRepository" 
       . ,(lambda (orb) (object-from-var orb *interface-repository*)))))

(define-corba-class ORB ()
  :slots 
  ((adaptor :initform nil :accessor adaptor)
   (active  :initarg :active  :accessor orb-active)
   (initial-references :initarg :initial-references
                       :accessor orb-initial-references)))

;;; (Object) Adaptor interface
(defgeneric listner-sockets (adaptor))
(defgeneric handle-socket (adaptor socket))
(defgeneric client-streams (adaptor))
(defgeneric handle-stream (adaptor stream))
(defgeneric server-fallback (adaptor))


(defun ORB_init (&optional args (orbid ""))
  (declare (ignore args orbid))
  (unless *the-orb*
    (setq *the-orb* (make-instance 'CORBA:ORB
                     :active t
                     :initial-references *default-initial-references*)))
  (setf (orb-active *the-orb*) t)
  *the-orb*)

(define-method op::shutdown ((orb orb))
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
         (when (and *running-orb* adaptor)
           ;; Should do server things, but don't know if there is
           ;; anything to do : (
           (unless streams
             (server-fallback adaptor)))
         :cant)
        ((nil) nil)))))



(define-method op::string_to_object ((orb orb) str) 
  (orb-string-to-object orb str))

(define-method op::object_to_string ((orb orb) obj)
  (orb-object-to-string orb obj))

(defun orb-string-to-object (orb str)
  (declare (ignore orb))
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

(defun orb-object-to-string (orb objref)
  (declare (ignore orb))
  (format nil
	  "IOR:~{~2,'0X~}"
	  (map 'list #'identity (marshal-make-encapsulation
				 (lambda (buffer) 
                                   (marshal-ior objref buffer))))))

(defun file-to-object (orb file)
  (op::string_to_object orb
                        (with-open-file (fs file :direction :input)
                          (read-line fs))))

(defun object-from-var (orb refstring)
  (cond
   ((and (stringp refstring)
         (> (length refstring) 4)
         (string-equal "IOR:" refstring :end2 4))
    (op::string_to_object orb refstring))
   (t
    (file-to-object orb refstring))))


(define-method op::list_initial_references ((orb orb))
  (mapcar #'car (orb-initial-references orb)))

(define-method op::resolve_initial_references ((orb orb) name)
  (let ((ref-entry
         (assoc name (orb-initial-references orb)
         :test #'equal)))
    (unless ref-entry
      (error 'object-not-exist))
    (funcall (cdr ref-entry) orb)))

(define-method op::work_pending ((orb orb))
  ;; FIXME
  t)

(define-method op::perform_work ((orb orb))
  (let ((*running-orb* t))
    (orb-wait orb nil)))

(define-method op::run ((orb orb))
  (let ((*running-orb* t))
    (loop while (orb-active orb)
        do (orb-wait orb nil))))

