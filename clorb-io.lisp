;;;; clorb-io.lisp  --  a reactive IO layer for CLORB
;;
;; io-reset ()
;; io-create-listener (&optional port)
;; io-create-descriptor ()
;; io-descriptor-destroy (desc)
;; io-descriptor-connect (desc host port)
;; io-descriptor-set-read (desc buf start end)
;; io-descriptor-set-write (desc buf start end)
;; io-descriptor-status
;; io-descriptor-error
;; io-descriptor-read-buffer
;; io-driver ()
;;

(in-package :clorb)

(defparameter *io-background-write-treshold* 200)

(defvar *io-event-queue* nil)

(defvar *io-socket* nil
  "listener socket (or similar). Producer of connections.")
(defvar *io-listener-process* nil)

(defvar *io-descriptions* nil
  "List of all io-descriptors that we will do I/O on.")



;;;; IO-DESCRIPTOR

(defstruct io-descriptor
  status
  error
  ;; -- internal --
  stream
  (read-buffer  nil :type (or null octets))
  (read-pos       0 :type buffer-index)
  (read-limit     0 :type buffer-index)
  read-process
  (write-buffer nil :type (or null octets))
  (write-pos      0 :type buffer-index)
  (write-limit    0 :type buffer-index)
  write-process)

;; FIXME: define and document status codes
;; - define what error contain (e.g. a condition)
;; ~ status member nil :connected :broken

(defun io-fix-broken ()
  (setq *io-descriptions*
        (loop for desc in *io-descriptions*
              if (eq :broken (io-descriptor-status desc))
              do (close (io-descriptor-stream desc))
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
  #+digitool (make-instance 'io-system-multiprocess)
  #-digitool (make-instance 'io-system-select))

(defun io-system-switch (new-class)
  (io-reset)
  (setq *io-system* (make-instance new-class)))


(defgeneric io-ready-for-read (system desc)
  (:method ((system io-system) desc)
           (declare (ignore desc))))

(defgeneric io-ready-for-write (system desc)
  (:method ((system io-system) desc)
           (declare (ignore desc))))



;;;; Event Queue

(defun io-queue-event (type desc)
  (push (list type desc) *io-event-queue*))


;;;; Creating io-descriptors

(defun io-create-listener (&optional port)
  (setq *io-socket* (open-passive-socket port))
  (setq port (or port (passive-socket-port *io-socket*)))
  (mess 3 "listener created on ~A" port)
  (values port (passive-socket-host *io-socket*)))


(defun io-create-descriptor ()
  (let ((desc (make-io-descriptor)))
    (push desc *io-descriptions*)
    desc))

(defun io-descriptor-destroy (desc)
  (ignore-errors
    (close (io-descriptor-stream desc)))
  (setq *io-descriptions*
        (delete desc *io-descriptions*)))



;;;; Making connections

(defvar *host-translations* '())

(defun io-descriptor-connect (desc host port)
  (setq host
        (or (cdr (assoc host *host-translations* :test #'string=))
            host))
  (setf (io-descriptor-stream desc)
        (open-active-socket host port))
  (mess 3 "connect to ~A:~A = ~A" host port (io-descriptor-stream desc))
  (setf (io-descriptor-status desc) :connected))

(defun io-descriptor-working-p (desc)
  (and (not (eql (io-descriptor-status desc) :broken))
       (not (socket-stream-closed-p (io-descriptor-stream desc)))))



;;;; Reading 


(defun io-descriptor-read-ready (desc)
  (and (io-descriptor-read-buffer desc)
       (< (io-descriptor-read-pos desc)
          (io-descriptor-read-limit desc))))


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

#+digitool
(defmethod io-ready-for-read ((system io-system-multiprocess) desc)
  (let ((old-process (io-descriptor-read-process desc)))
    (cond (old-process
           (mess 1 "Old process= ~A" old-process)
           ;;(ccl:process-reset-and-enable old-process)
           (ccl:process-enable old-process)
           (mess 1 "reset: ~A" old-process))
          (t
           (let ((new (start-process "CORBA read" #'io-desc-read desc)))
             (mess 1 "New process= ~A" new)
             (setf (io-descriptor-read-process desc) new))))))


(defun io-descriptor-set-read (desc buf start end)
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


(defmethod io-ready-for-write ((system io-system-multiprocess) desc)
  (if (> (- (io-descriptor-write-limit desc) (io-descriptor-write-pos desc)) 
         *io-background-write-treshold*)
    (setf (io-descriptor-write-process desc)
          (start-process "CORBA write" #'io-desc-write desc))
    (io-desc-write desc)))


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
        desc))))

(defun io-bg-listen ()
  (loop
    (let ((new (io-listen t)))
      (when new
        (io-queue-event :new new)))))

(defun io-start-bg-listen ()
  (unless *io-listener-process*
    (setf *io-listener-process*
          (start-process "CORBA listen" #'io-bg-listen))))



;;;; Select Driver 

(defun io-poll-desc (desc status)
  (with-slots (stream read-buffer read-limit read-pos
                      write-buffer write-pos write-limit) desc
    (declare (type (or null octets)
                   read-buffer write-buffer)
             (type buffer-index read-limit read-pos
                   write-limit write-pos))
    (or
     (when (and read-buffer (< read-pos read-limit)
                status (not (eql status :output)))
       (handler-case
         (when (socket-stream-listen stream)
           (let ((n (read-octets-no-hang read-buffer read-pos
                                         read-limit stream)))
             (incf read-pos n)
             (if (>= read-pos read-limit)
               :read-ready)))
         (stream-error (e)
          (setf (io-descriptor-status desc) :broken)
          (setf (io-descriptor-error desc)  e)
          :error)))
     (when (and write-buffer (< write-pos write-limit)
                (member status '(:output :io :append)))
       (handler-case
         (let ((n (write-octets-no-hang write-buffer write-pos
                                        write-limit stream)))
           (incf write-pos n)
           (if (>= write-pos write-limit)
             :write-ready))
         (stream-error (e)
                (setf (io-descriptor-status desc) :broken)
                (setf (io-descriptor-error desc)  e)
                :error)))
     (if (eq status :error)
       :error))))


(defmethod io-system-driver ((system io-system-select))
  (declare (optimize (speed 2)))
  (loop
    named io-driver-select
    for inactivity from 0 to 500
    for select = (make-select)
    for cookies = '()
    do
    (dolist (desc *io-descriptions* nil)
      (let ((stream (io-descriptor-stream desc)))
        (when stream
          (let ((input (io-descriptor-read-ready desc))
                (output (io-descriptor-write-ready desc)))
            ;; SBCL has always an input buffer, empty that before doing select
            #+(or sbcl cmu18)
            (when (and input (socket-stream-listen stream))
              (let ((event (io-poll-desc desc :input)))
                (if event
                  (return-from io-driver-select (values event desc)))))
            (when (or input output)
              (push (cons (select-add-stream select stream input output)
                          desc)
                    cookies))))))
    (when *io-socket*
      (select-add-listener select *io-socket*))
    (setq select (select-wait select))
    (loop for (cookie . desc) in cookies
          for status = (select-stream-status select cookie)
          when status
          do (let ((event (io-poll-desc desc status)))
               (if event
                 (return-from io-driver-select (values event desc)))))
    (when *io-socket*
      (let ((new (io-listen nil)))
        (when new
          (return-from io-driver-select (values :new new)))))))



;;;; Multiprocess driver 

(defmethod io-system-driver ((system io-system-multiprocess))
  (io-start-bg-listen)
  (unless *io-event-queue*
    (process-wait-with-timeout "waiting for event" 3600
                               (lambda () *io-event-queue*)))
  (let ((event (pop *io-event-queue*)))
    (values-list (or event '(nil)))))



;;;; Driver

(defun io-driver ()
  (io-system-driver *io-system*))
