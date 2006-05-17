;;; clorb-conn.lisp --- Connection Layer

(in-package :clorb)

(defvar *request-id-seq* 0)

;;(defvar *default-trace-connection* nil)



;;;; Connection Class


(defclass Connection ()
  ((the-orb         :initarg :orb                          :accessor the-orb)
   (read-buffer     :initarg :read-buffer                  :accessor connection-read-buffer)
   (read-callback   :initarg :read-callback                :accessor connection-read-callback)
   (write-buffer    :initarg :write-buffer   :initform nil :accessor connection-write-buffer)
   (io-descriptor   :initarg :io-descriptor  :initform nil :accessor connection-io-descriptor)
   (client-requests                          :initform nil :accessor connection-client-requests)
   (server-requests                          :initform nil :accessor connection-server-requests)
   (server-p        :initarg :server-p       :initform nil :accessor server-p)
   ;; Defrag support
   (assembled-handler  :initform nil  :accessor assembled-handler)
   (fragment-buffer    :initform nil  :accessor fragment-buffer)))


(defmethod print-object ((conn connection) stream)
  (print-unreadable-object (conn stream :identity t :type t)
    (let ((desc (connection-io-descriptor conn)))
      (when desc (write-string (io-describe-descriptor desc) stream)))))



;;;; Connection Methods

(defmethod next-request-id ((conn Connection))
  (incf *request-id-seq*))


;;(defvar *desc-conn* (make-hash-table))

(defun make-associated-connection (orb desc)
  (let* ((conn (make-instance 'Connection :orb orb :io-descriptor desc)))
    (io-descriptor-associate-connection desc conn)
    conn))

(defun connection-destroy (conn)
  (let ((desc (connection-io-descriptor conn)))
    (io-descriptor-destroy desc)))


(defun create-connection (orb host port)
  (let ((desc (io-create-descriptor)))
    (handler-case
      (progn (io-descriptor-connect desc host port)
             (make-associated-connection orb desc))
      (error (err)
             (mess 4 "(connect ~S ~S): ~A" host port err)
             (setf (io-descriptor-error desc) err)
             (setf (io-descriptor-status desc) :broken)
             (io-descriptor-destroy desc)
             nil))))


(defun connection-working-p (conn)
  (io-descriptor-working-p (connection-io-descriptor conn)))


(defun connection-init-read (conn continue-p n callback)
  (setf (connection-read-callback conn) callback)
  (let ((desc (connection-io-descriptor conn)))
    (let* ((buffer (if continue-p
                     (connection-read-buffer conn)
                     (get-work-buffer (the-orb conn))))
           (octets (buffer-octets buffer))
           (start (fill-pointer octets)))
      (unless continue-p
        (setf (connection-read-buffer conn) buffer))
      (when (< (array-total-size octets) n)
        (adjust-array octets n))
      (setf (fill-pointer octets) n)
      (io-descriptor-set-read desc octets start n))))


(defun connection-send-buffer (conn buffer)
  (when (connection-write-buffer conn)
    (orb-wait (lambda (conn) (not (connection-write-buffer conn))) conn))
  (setf (connection-write-buffer conn) buffer)
  (let ((desc (connection-io-descriptor conn))
        (octets (buffer-octets buffer)))
    (io-descriptor-set-write desc octets 0 (length octets))))


(defun connection-add-client-request (conn request)
  (push request (connection-client-requests conn)))

(defun connection-remove-client-request (conn request)
  (setf (connection-client-requests conn)
        (delete request (connection-client-requests conn))))

(defun find-waiting-client-request (conn request-id)
  (let ((req-list (connection-client-requests conn)))
    (let ((req (find request-id req-list :key #'request-id)))
      (if req
        (setf (connection-client-requests conn) (delete req req-list))
        (mess 4 "Unexpected response with request id ~d" request-id))
      req)))


(defun connection-add-server-request (conn request)
  (push request (connection-server-requests conn)))

(defun connection-remove-server-request (conn request)
  (setf (connection-server-requests conn)
        (delete request (connection-server-requests conn))))


(defun connection-add-fragment (conn buffer header-size)
  (with-accessors ((fragment-buffer fragment-buffer)) conn
    (when fragment-buffer
      (setf (buffer-octets buffer)
            (concatenate 'octets
                         (buffer-octets fragment-buffer)
                         (subseq (buffer-octets buffer) header-size))))
    (setf fragment-buffer buffer)))



;;;; Connection events


(defun connection-error (conn)
  ;; Called when there is IO error
  (let ((requests (connection-client-requests conn)))
    (dolist (req requests)
      (setf (request-exception req) 
            (system-exception 'CORBA:COMM_FAILURE))
      (setf (request-status req) :error )))
  (setf (connection-client-requests conn) nil))


(defun connection-close (conn)
  ;; Called on recipt of a connection close message
  (connection-destroy conn)
  ;; The server should not have started on any of the outstanding requests.
  (dolist (req (connection-client-requests conn))
    (assert (null (request-status req))) ; is there a race condition?
    (setf (request-exception req) (system-exception 'CORBA:TRANSIENT 3 :completed_no))
    (setf (request-status req) :error))
  (setf (connection-client-requests conn) nil))


(defun connection-read-ready (conn)
  (funcall (connection-read-callback conn) conn))


(defmethod connection-write-ready ((conn connection))
  (setf (connection-write-buffer conn) nil))

(defun connection-init-defragmentation (conn handler)
  (cond ((assembled-handler conn)
         (mess 5 "Fragment overrun")
         (connection-error conn)
         nil)
        (t
         (setf (assembled-handler conn) handler)
         (setf (fragment-buffer conn) nil)
         t)))

;;;; Event loop (orb-wait)

(defvar *new-connection-callback*
  (lambda (desc)
    (io-descriptor-destroy desc)))


(defvar *running-orb* t
  "Will be set to true in the process that is running the ORB server part.
If this is true, orb-wait will check server streams also.
Can be set to true globally for singel-process / development.")


(defun orb-wait (wait-func &rest wait-args)
  (if *running-orb*
    (loop until (apply wait-func wait-args) do (orb-work *the-orb* t nil))
    (apply #'process-wait "orb-wait" wait-func wait-args)))


(defun orb-run-queue (orb)
  (loop while (work-queue orb)
     do (funcall (pop (work-queue orb)))))

(defun orb-work (orb run-queue poll)
  (when run-queue
    (orb-run-queue orb))
  (let ((event-processed nil))
    (loop for event = (io-get-event)
          while event
          do (setq event-processed t)
          (let* ((desc (second event))
                 (conn (io-descriptor-connection desc)))
            (mess 1 "io-event: ~S ~A ~A" (car event) (io-descriptor-stream desc) conn)
            (case (car event)
              (:read-ready
               (when conn (connection-read-ready conn)))
              (:write-ready
               (io-descriptor-set-write desc nil 0 0)
               (when conn (connection-write-ready conn)))
              (:new
               (funcall *new-connection-callback* desc))
              (:connected
               ;; Not implemented yet..; for outgoing connections setup
               nil)
              (:error
               (mess 4 "Error: ~A" (io-descriptor-error desc))
               (io-descriptor-destroy desc)
               (when conn (connection-error conn))))))
    (unless event-processed
      (io-driver poll))))
  

