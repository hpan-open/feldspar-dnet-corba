;;;; clorb-io.lisp  --  a reactive IO layer for CLORB
;; $Id: clorb-io.lisp,v 1.27 2004/12/28 00:02:07 lenst Exp $


;; io-reset ()
;; io-create-listener (&optional port)
;; io-create-descriptor () => desc
;; io-descriptor-destroy (desc)
;; io-descriptor-connect (desc host port)
;; io-descriptor-working-p (desc) => boolean
;; io-descriptor-set-read (desc buf start end)
;; io-descriptor-set-write (desc buf start end)
;; io-descriptor-status (desc) => status
;; io-descriptor-error (desc) => condition or nil
;; io-descriptor-read-buffer (desc) => buffer
;; io-driver () => event, desc
;; io-describe-descriptor (desc)

;; io-descriptor-shortcut-connect (desc)
;; io-fix-broken ()
;; io-reset (&key listener)
;; io-system-switch (new-class)


(in-package :clorb)

(defparameter *io-background-write-treshold* 200)

(defvar *io-event-queue* nil)

(defvar *io-socket* nil
  "listener socket (or similar). Producer of connections.")
(defvar *io-listener-process* nil)

(defvar *io-descriptions* nil
  "List of all io-descriptors that we will do I/O on.")

(defvar *io-work-pending* nil)


;;;; Event Queue

(defun io-queue-event (type desc)
  (push (list type desc) *io-event-queue*))

(defun io-work-pending-p ()
  (or *io-work-pending* *io-event-queue*))


;;;; IO-DESCRIPTOR

(defstruct io-descriptor
  status
  error
  connection
  ;; -- internal --
  stream
  (read-buffer  nil :type (or null octets))
  (read-pos       0 :type buffer-index)
  (read-limit     0 :type buffer-index)
  read-process
  (write-buffer nil :type (or null octets))
  (write-pos      0 :type buffer-index)
  (write-limit    0 :type buffer-index)
  write-process
  shortcut-p )

;; FIXME: define and document status codes
;; - define what error contain (e.g. a condition)
;; ~ status member nil :connected :broken



;;;; Shortcut stream


(defun make-octet-stream (&optional (label (gensym)))
  (let ((buffer nil))
    (labels 
      ((reader (buf start end)
         (let ((len (- end start)))
           (when (<= len (length buffer))
             (mess 1 "Reading ~A (~D ~D)" label start end)
             (replace buf buffer 
                      :start1 start :end1 end
                      :start2 0 :end2 len)
             (setf buffer (subseq buffer len))
             len)))
       (writer (buf start end)
         (mess 1 "Writing ~A (~D ~D)" label start end)
         (let ((len (- end start)))
           (setf buffer (concatenate 'vector buffer (subseq buf start end)))
           len)))
      (lambda (op)
        (ecase op
          ((reader) #'reader)
          ((writer) #'writer)
          ((buffer) buffer))))))


(defun make-shortcut-stream (input output)
  (let ((reader (funcall input 'reader))
        (writer (funcall output 'writer)))
    (lambda (op buf start end)
      (ecase op
        ((read) (funcall reader buf start end))
        ((write) (funcall writer buf start end))))))

(defun io-shortcut-write (shortcut-stream seq start end)
  (funcall shortcut-stream 'write seq start end))

(defun io-shortcut-read (shortcut-stream seq start end)
  (funcall shortcut-stream 'read seq start end))


(defun io-descriptor-shortcut-connect (desc)
  (let ((i-stream (make-octet-stream))
        (o-stream (make-octet-stream)))
    (let ((other (make-io-descriptor
                  :stream (make-shortcut-stream o-stream i-stream)
                  :status :connected
                  :shortcut-p desc)))
      (push other *io-descriptions*)
      (io-queue-event :new other)
      (setf (io-descriptor-stream desc) (make-shortcut-stream i-stream o-stream))
      (setf (io-descriptor-shortcut-p desc) other))))



;;;; Recovery

(defun io-fix-broken ()
  (setq *io-descriptions*
        (loop for desc in *io-descriptions*
              if (eq :broken (io-descriptor-status desc))
              do (unless (io-descriptor-shortcut-p desc)
                   (close (io-descriptor-stream desc)))
              else collect desc)))

(defun io-reset (&key (listener nil))
  (when (and listener *io-socket*)
    (ignore-errors
     (socket-close *io-socket*))
    (setq *io-socket* nil))
  (setq *io-event-queue* nil)
  (end-process (shiftf *io-listener-process* nil))
  (dolist (desc *io-descriptions*)
    (end-process (shiftf (io-descriptor-read-process desc) nil))
    (end-process (shiftf (io-descriptor-write-process desc) nil))
    (ignore-errors
     (close (io-descriptor-stream desc)))
    (setf (io-descriptor-status desc) :broken))
  (setq *io-descriptions* nil))



;;;; IO-System

(defclass io-system ()
  ())

(defclass io-system-select (io-system)
  ())

(defclass io-system-multiprocess (io-system)
  ((read-ready :initform nil  :accessor io-read-ready)))


(defvar *io-system*
  (make-instance 'io-system-select))

(defun io-system-switch (new-class)
  (io-reset)
  (setq *io-system* (make-instance new-class)))


(defgeneric io-ready-for-read (system desc)
  (:method ((system io-system) desc)
           (declare (ignore desc))))

(defgeneric io-ready-for-write (system desc)
  (:method ((system io-system) desc)
           (declare (ignore desc))))




;;;; Creating io-descriptors

(defun io-create-listener (&optional port)
  (setq *io-socket* (open-passive-socket port))
  (setq port (or port (passive-socket-port *io-socket*)))
  (mess 3 "listener created on ~A" port)
  (values port (passive-socket-host *io-socket*)))


(defun io-create-descriptor (&optional conn)
  (let ((desc (make-io-descriptor :connection conn)))
    (push desc *io-descriptions*)
    desc))

(defun io-descriptor-associate-connection (desc conn)
  (setf (io-descriptor-connection desc) conn))

(defun io-descriptor-destroy (desc)
  (setf (io-descriptor-status desc) :broken)
  (ignore-errors
    (close (io-descriptor-stream desc)))
  (setf (io-descriptor-connection desc) nil)
  (setq *io-descriptions* (delete desc *io-descriptions*)))


(defun io-describe-descriptor (desc)
  (let ((stream (io-descriptor-stream desc)))
    (typecase stream
      (function "loopback")
      (t (multiple-value-bind (host port)
                              (socket-peer stream)
           (format nil "~A:~A" host port))))))




;;;; Making connections


(defvar *host-translations* '())

(defvar *io-loopback-p* nil)


(defun io-descriptor-connect (desc host port)
  (setq host
        (or (cdr (assoc host *host-translations* :test #'string=))
            host))
  (cond ((and *io-loopback-p*
              (funcall *io-loopback-p* host port))
         (io-descriptor-shortcut-connect desc)
         (mess 3 "made shortcut connection"))
        (t
         (setf (io-descriptor-stream desc) (open-active-socket host port))
         (mess 3 "connect to ~A:~A = ~A" host port (io-descriptor-stream desc))))
  (setf (io-descriptor-status desc) :connected))


(defun io-descriptor-working-p (desc)
  (and (not (eql (io-descriptor-status desc) :broken))
       (or (io-descriptor-shortcut-p desc)
           (not (socket-stream-closed-p (io-descriptor-stream desc))))))




;;;; Reading 


(defun io-descriptor-read-ready (desc)
  (and (io-descriptor-read-buffer desc)
       (< (io-descriptor-read-pos desc)
          (io-descriptor-read-limit desc))))


(defun io-descriptor-set-read (desc buf start end)
  (declare (optimize (speed 2) (safety 3) (debug 1)))
  (setf (io-descriptor-read-buffer desc) buf
        (io-descriptor-read-pos desc) start
        (io-descriptor-read-limit desc) end)
  (when (and buf (> end start))
    (io-ready-for-read *io-system* desc))
  nil)



;;;; Writing 


(defun io-descriptor-write-ready (desc)
  (and (io-descriptor-write-buffer desc)
       (< (io-descriptor-write-pos desc)
          (io-descriptor-write-limit desc))))


(defun io-descriptor-set-write (desc buf start end)
  (setf (io-descriptor-write-buffer desc) buf
        (io-descriptor-write-pos desc) start
        (io-descriptor-write-limit desc) end)
  (when (and buf (> end start))
    (io-ready-for-write *io-system* desc)))



;;;; Listening


(defun io-listen (blocking)
  (let ((new (accept-connection-on-socket *io-socket* blocking)))
    (when new
      (let ((desc (io-create-descriptor)))
        (setf (io-descriptor-stream desc) new)
        (setf (io-descriptor-status desc) :connected)
        (io-queue-event :new desc)))))


(defun io-bg-listen ()
  (loop (io-listen t)))


(defun io-start-bg-listen ()
  (unless *io-listener-process*
    (setf *io-listener-process*
          (start-process "CORBA listen" #'io-bg-listen))))



;;;; Shortcut driver


(defun io-poll-shortcut (desc &optional io)
  (let ((result nil))
    (flet ((out (from)
             (when (io-descriptor-write-ready from)
               (io-shortcut-write (io-descriptor-stream from)
                                  (io-descriptor-write-buffer from)
                                  (io-descriptor-write-pos from)
                                  (io-descriptor-write-limit from))
               (setf (io-descriptor-write-pos from) (io-descriptor-write-limit from))
               (io-queue-event :write-ready from)
               (setq result t)))
           (in (to)
             (when (and (io-descriptor-read-ready to)
                        (io-shortcut-read (io-descriptor-stream to)
                                          (io-descriptor-read-buffer to)
                                          (io-descriptor-read-pos to)
                                          (io-descriptor-read-limit to)))
               (setf (io-descriptor-read-pos to) (io-descriptor-read-limit to))
               (io-queue-event :read-ready to)
               (setq result t))))
      (case io
        (:read (in desc))
        (:write (out desc) 
                (in (io-descriptor-shortcut-p desc)))
        (t (out desc) (in desc)))
      result)))


(defun io-driver-shortcut ()
  (let ((result nil))
    (dolist (desc *io-descriptions*)
      (when (io-descriptor-shortcut-p desc)
        (when (io-poll-shortcut desc)
          (setq result t))))
    result))




;;;; Select Driver 

(defun io-poll-desc (desc status)
  (with-slots (stream read-buffer read-limit read-pos
                      write-buffer write-pos write-limit) desc
    (declare (type (or null octets)
                   read-buffer write-buffer)
             (type buffer-index read-limit read-pos
                   write-limit write-pos))

    (cond ((eq status :error)
           (io-queue-event :error desc))
          (status
           (handler-case
               (progn 
                 (when (and (not (eql status :output))
                            (io-descriptor-read-ready desc))
                   (let ((n (read-octets-no-hang read-buffer read-pos
                                                   read-limit stream)))
                       (incf read-pos n)
                       (if (>= read-pos read-limit)
                           (io-queue-event :read-ready desc)))) 
                 (when (and (member status '(:output :io :append))
                            (io-descriptor-write-ready desc))
                   (let ((n (write-octets-no-hang write-buffer write-pos
                                                  write-limit stream)))
                     (incf write-pos n)
                     (if (>= write-pos write-limit)
                         (io-queue-event :write-ready desc)))))
             (stream-error (e)
               (setf (io-descriptor-status desc) :broken)
               (setf (io-descriptor-error desc)  e)
               (io-queue-event :error desc)))))))


(defun io-poll-select (poll)
  (let ((select (make-select)))
    (dolist (desc *io-descriptions*)
      (unless (io-descriptor-shortcut-p desc)
        (let ((stream (io-descriptor-stream desc)))
          (when stream
            (let ((input (io-descriptor-read-ready desc))
                  (output (io-descriptor-write-ready desc)))
              (when (or input output)
                (select-add-stream select stream input output desc) ))))))
    (when *io-socket*
      (select-add-listener select *io-socket*))
    (setq select (select-wait select poll))
    (select-do-result select #'io-poll-desc)
    (when *io-socket*
      (io-listen nil))))


(defmethod io-system-driver ((system io-system-select) poll)
  (setq *io-work-pending* nil)
  (io-poll-select poll))


(defmethod io-ready-for-write ((system io-system-select) desc)
  (setq *io-work-pending* t)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :write)))


(defmethod io-ready-for-read ((system io-system-select) desc)
  (setq *io-work-pending* t)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :read)))




;;;; Multiprocess System 


#+digitool
(defun io-desc-read (desc)
  (handler-case
    (loop
      (mess 1 "io-desc-read loop")
      (let ((stream (io-descriptor-stream desc)))
        (unless (socket-stream-listen stream)
          (process-wait-with-timeout "wait for input" nil
                                     #'socket-stream-listen stream))
        (read-octets (io-descriptor-read-buffer desc)
                     (io-descriptor-read-pos desc) (io-descriptor-read-limit desc)
                     stream)
        (setf (io-descriptor-read-pos desc) (io-descriptor-read-limit desc))
        (mess 1 "Enqueue event read-ready")
        (io-queue-event :read-ready desc))
      (mess 1 "Disable process ~A" (current-process))
      (ccl:process-disable (current-process))
      (ccl:process-allow-schedule))
    (stream-error
     (e)
     (setf (io-descriptor-status desc) :broken)
     (setf (io-descriptor-error desc)  e)
     (io-queue-event :error desc))))


(defun io-desc-write (desc)
  (handler-case
    (progn
      (write-octets (io-descriptor-write-buffer desc)
                    (io-descriptor-write-pos desc) (io-descriptor-write-limit desc)
                    (io-descriptor-stream desc))
      (setf (io-descriptor-write-pos desc) (io-descriptor-write-limit desc))
      (io-queue-event :write-ready desc))
    (stream-error
     (e)
     (setf (io-descriptor-status desc) :broken)
     (setf (io-descriptor-error desc)  e)
     (io-queue-event :error desc) ))
  (setf (io-descriptor-write-process desc) nil))


#+digitool
(defmethod io-ready-for-read ((system io-system-multiprocess) desc)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :read)
    (let ((old-process (io-descriptor-read-process desc)))
      (cond (old-process
             (mess 1 "Old process= ~A" old-process)
             ;;(ccl:process-reset-and-enable old-process)
             (ccl:process-enable old-process)
             (mess 1 "reset: ~A" old-process))
            (t
             (let ((new (start-process "CORBA read" #'io-desc-read desc)))
               (mess 1 "New process= ~A" new)
               (setf (io-descriptor-read-process desc) new)))))))


(defmethod io-ready-for-write ((system io-system-multiprocess) desc)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :write)
    (if (> (- (io-descriptor-write-limit desc) (io-descriptor-write-pos desc)) 
           *io-background-write-treshold*)
      (setf (io-descriptor-write-process desc)
            (start-process "CORBA write" #'io-desc-write desc))
      (io-desc-write desc))))


(defmethod io-system-driver ((system io-system-multiprocess) poll)
  (declare (ignore poll))
  (io-start-bg-listen)
  (process-wait-with-timeout "waiting for event" 3600
                             (lambda () *io-event-queue*)))




;;;; Driver


(defun io-event-waiting-p ()
  *io-event-queue*)

(defun io-get-event ()
  (pop *io-event-queue*))

(defun io-driver (poll)
  (if poll
    (io-system-driver *io-system* t)
    (loop repeat 10 until *io-event-queue* do (io-system-driver *io-system* nil))))

