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


;;;; IO Layer

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

(defparameter *io-multi-process* (or #+mcl t ))
(defparameter *io-background-write-treshold* 200)

(defvar *io-event-queue* nil)
(defvar *io-ready-for-read* nil)

(defvar *io-socket* nil
  "listener socket (or similar). Producer of connections.")
(defvar *io-listener-process* nil)

(defvar *io-descriptions* nil
  "List of all io-descriptors that we will do I/O on.")


(defun io-fix-broken ()
  (setq *io-descriptions*
        (loop for desc in *io-descriptions*
              if (eq :broken (io-descriptor-status desc))
              do (close (io-descriptor-stream desc))
              else collect desc))
  (setq *io-ready-for-read*
        (loop for desc in *io-ready-for-read*
              unless (eq :broken (io-descriptor-status desc))
              collect desc)))

(defun io-reset (&key (listener nil))
  (when (and listener *io-socket*)
    (ignore-errors
     (socket-close *io-socket*))
    (setq *io-socket* nil))
  (setq *io-event-queue* nil)
  (setq *io-ready-for-read* nil)
  (end-process (shiftf *io-listener-process* nil))
  (dolist (desc *io-descriptions*)
    (end-process (shiftf (io-descriptor-read-process desc) nil))
    (end-process (shiftf (io-descriptor-write-process desc) nil))
    (ignore-errors
     (close (io-descriptor-stream desc)))
    (setf (io-descriptor-status desc) :broken))
  (setq *io-descriptions* nil))


(defun io-queue-event (type desc)
  (push (list type desc) *io-event-queue*))

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

(defvar *host-translations* '())

(defun io-descriptor-connect (desc host port)
  (setq host
        (or (cdr (assoc host *host-translations* :test #'string=))
            host))                 
  (setf (io-descriptor-stream desc)
        (open-active-socket host port))
  (mess 3 "connect to ~A:~A = ~A" host port (io-descriptor-stream desc))
  (setf (io-descriptor-status desc) :connected))            


(defun io-desc-read (desc)
  (handler-case
    (let ((stream (io-descriptor-stream desc)))
      (read-octets (io-descriptor-read-buffer desc)
                   (io-descriptor-read-pos desc) (io-descriptor-read-limit desc)
                   stream)
      (setf (io-descriptor-read-pos desc) (io-descriptor-read-limit desc))
      (io-queue-event :read-ready desc))
    (stream-error
     (e)
     (setf (io-descriptor-status desc) :broken)
     (setf (io-descriptor-error desc)  e)
     (io-queue-event :error desc) ))
  (setf (io-descriptor-read-process desc) nil))
 
(defun io-descriptor-set-read (desc buf start end)
  (setf (io-descriptor-read-buffer desc) buf
        (io-descriptor-read-pos desc) start
        (io-descriptor-read-limit desc) end)
  (when (and *io-multi-process* buf (> end start))
    (push desc *io-ready-for-read*))
  nil)


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

(defun io-descriptor-set-write (desc buf start end)
  (setf (io-descriptor-write-buffer desc) buf
        (io-descriptor-write-pos desc) start
        (io-descriptor-write-limit desc) end)
  (when (and *io-multi-process* buf (> end start))
    (if (> (- end start) *io-background-write-treshold*)
      (setf (io-descriptor-write-process desc) (start-process "write" #'io-desc-write desc))
      (io-desc-write desc))))


(defun io-bg-listen ()
  (loop
    (let ((new (accept-connection-on-socket *io-socket* t)))
      (when new
        (let ((desc (io-create-descriptor)))
          (setf (io-descriptor-stream desc) new)
          (setf (io-descriptor-status desc) :connected)
          (io-queue-event :new desc))))))

(defun io-start-bg-listen ()
  (unless *io-listener-process*
    (setf *io-listener-process*
          (start-process "listen" #'io-bg-listen))))


(defun io-poll-desc (desc status)
  (with-slots (stream read-buffer read-limit read-pos
                      write-buffer write-pos write-limit) desc
    (declare (type (or null octets)
                   read-buffer write-buffer)
             (type buffer-index read-limit read-pos
                   write-limit write-pos))
    (or
     (when (and read-buffer (< read-pos read-limit)
                (member status '(:input :io)))
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
                (member status '(:output :io)))
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

(defun io-multi-process-poll ()
  (unless *io-event-queue*
    (let ((ready-streams
           (loop for desc in *io-ready-for-read*
                 collect (io-descriptor-stream desc))))
      (process-wait-with-timeout "waiting for event" 120
                                 (lambda (streams) 
                                   (or (not (null *io-event-queue*))
                                       (some #'listen streams)))
                                 ready-streams))
    (setq *io-ready-for-read*
         (delete-if (lambda (desc)
                      (or (eq :broken (io-descriptor-status desc))                       
                          (if (listen (io-descriptor-stream desc))
                            (setf (io-descriptor-read-process desc)
                                  (start-process "read" #'io-desc-read desc)))))
                    *io-ready-for-read*)) )
  (let ((event (pop *io-event-queue*)))
    (values-list (or event '(nil)))))


(defun io-driver ()
  (declare (optimize (speed 2)))

  (when *io-multi-process*
    (io-start-bg-listen)
    (return-from io-driver
      (io-multi-process-poll)))

  (loop
   for inactivity from 0 to 500
   for select = (make-select)
   for cookies = '()
   do
   (dolist (desc *io-descriptions* nil)
     (with-slots (stream read-buffer read-limit read-pos
                         write-buffer write-pos write-limit) desc
       (declare (type buffer-index read-limit read-pos
                      write-limit write-pos))
       (when stream
         (let ((input (and read-buffer (< read-pos read-limit)))
               (output (and write-buffer (< write-pos write-limit))))
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
                  (return-from io-driver (values event desc)))))
   (when *io-socket*
     (let ((new (accept-connection-on-socket *io-socket* nil)))
       (when new
         (let ((desc (io-create-descriptor)))
           (setf (io-descriptor-stream desc) new)
           (setf (io-descriptor-status desc) :connected)
           (return-from io-driver (values :new desc))))))))
