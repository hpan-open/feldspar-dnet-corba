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
   (write-callback  :initarg :write-callback               :accessor connection-write-callback)
   (error-callback  :initarg :error-callback               :accessor connection-error-callback)
   (io-descriptor   :initarg :io-descriptor  :initform nil :accessor connection-io-descriptor)
   (client-requests                          :initform nil :accessor connection-client-requests)))



;;;; Connection Methods

(defmethod next-request-id ((conn Connection))
  (incf *request-id-seq*))


(defvar *desc-conn* (make-hash-table))

(defun make-associated-connection (orb desc)
  (let* ((conn (make-instance 'Connection :orb orb :io-descriptor desc)))
    (setf (gethash desc *desc-conn*) conn)
    conn))

(defun connection-destroy (conn)
  (let ((desc (connection-io-descriptor conn)))
    (io-descriptor-destroy desc)
    (remhash desc *desc-conn*)))


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
                     (get-work-buffer)))
           (octets (buffer-octets buffer))
           (start (fill-pointer octets)))
      (unless continue-p
        (setf (connection-read-buffer conn) buffer))
      (when (< (array-total-size octets) n)
        (adjust-array octets n))
      (setf (fill-pointer octets) n)
      (io-descriptor-set-read desc octets start n))))


(defun write-done (conn)
  (setf (connection-write-buffer conn) nil))

(defun connection-send-buffer (conn buffer)
  (when (connection-write-buffer conn)
    (orb-wait (lambda (conn) (not (connection-write-buffer conn))) conn))
  (setf (connection-write-buffer conn) buffer)
  (setf (connection-write-callback conn) #'write-done)
  (let ((desc (connection-io-descriptor conn))
        (octets (buffer-octets buffer)))

    (io-descriptor-set-write desc octets 0 (length octets))))



;;;; Connection events


(defun connection-error (conn)
  ;; Called when there is IO error
  (funcall (connection-error-callback conn) conn))


(defun connection-read-ready (conn)
  (funcall (connection-read-callback conn) conn))


(defun connection-write-ready (conn)
  (funcall (connection-write-callback conn) conn))




;;;; Event loop (orb-wait)

(defvar *new-connection-callback*
  (lambda (desc)
    (io-descriptor-destroy desc)))


(defvar *running-orb* t
  "Will be set to true in the process that is running the ORB server part.
If this is true, orb-wait will check server streams also.
Can be set to true globally for singel-process / development.")


(defun orb-wait (&optional wait-func &rest wait-args)
  (if *running-orb*
    (if wait-func
      (loop until (apply wait-func wait-args) do (orb-work))
      (orb-work))
    (if wait-func
      (apply #'process-wait "orb-wait" wait-func wait-args)
      (sleep 0.1))))


(defun orb-work ()
  (declare (optimize (debug 3)))
  (multiple-value-bind (event desc) (io-driver)
    (when event
      (mess 1 "io-event: ~S ~A" event (io-descriptor-stream desc)))
    (let ((conn (gethash desc *desc-conn*)))
      (case event
        (:read-ready
         ;;(io-descriptor-set-read desc nil 0 0)
         (when conn
           (connection-read-ready conn)))
        (:write-ready
         (io-descriptor-set-write desc nil 0 0)
         (connection-write-ready conn))
        (:new
         (funcall *new-connection-callback* desc))
        (:connected
         ;; Not implemented yet..; for outgoing connections setup
         nil)
        (:error
         (mess 4 "Error: ~A" (io-descriptor-error desc))
         (io-descriptor-destroy desc)
         (when conn (connection-error conn)))
        ((nil)
         (mess 1 "time out"))))))

