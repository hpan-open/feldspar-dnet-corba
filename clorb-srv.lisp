;;;; CORBA server module

(in-package :clorb)

;; Status values
(put :normal  'reply-status 0)
(put :forward 'reply-staus 4)
(put :forward 'locate-status 2)
(put :exception 'reply-status 2)


;;;; GIOP / IIOP stuff

(defun decode-userexception (exc)
  (values
   (cons ':tk_string
         (if (slot-boundp exc 'types)
             (slot-value exc 'types)
             (handler-case
                 (map 'list #'second
                      (third (typecode-params 
                              (get-typecode (exception-id exc)))))
              (error () (error 'unknown)))))
   (cons (exception-id exc)
         (userexception-values exc))))


;;;; Server proper

(defvar *server-socket* nil
  "The `master' socket that we accept new conections on")

(defclass clorb-adaptor ()
  ((orb :initarg :orb  :accessor adaptor-orb)
   (listner-socket 
    :initarg :listner
    :initform nil :accessor listner-socket)
   (client-streams :initform nil :accessor client-streams-raw)))

(defun setup-server (&optional (orb (ORB_init)))
  (setq *server-socket* (open-passive-socket *port*))
  (let ((adaptor (make-instance 'clorb-adaptor 
                   :orb orb
                   :listner *server-socket*)))
    (setf (adaptor orb) adaptor)))

(defmethod listner-sockets ((adaptor clorb-adaptor))
  (if (listner-socket adaptor)
      (list (listner-socket adaptor))))

(defun srv-handle-socket (adaptor socket &optional allow-blocking)
  (let ((conn (accept-connection-on-socket socket allow-blocking)))
    (when conn
      (push conn (client-streams-raw adaptor)))))

(defmethod handle-socket ((adaptor clorb-adaptor) socket)
  (srv-handle-socket adaptor socket))



(defmethod client-streams ((adaptor clorb-adaptor))
  (dolist (stream (client-streams-raw adaptor))
    (unless (socket-stream-functional-p stream)
      (mess 1 "Removing closed stream ~S" stream)
      (setf (client-streams-raw adaptor)
        (delete stream (client-streams-raw adaptor)))))
  (client-streams-raw adaptor))


(defmethod handle-stream ((adaptor clorb-adaptor) stream)
  (when (and (typep stream 'stream)
             (open-stream-p stream))
    (when (socket-stream-listen stream)
      (mess 1 "Getting message from ~S" stream)
      (get-message stream))
    t))

(defmethod server-fallback ((adaptor clorb-adaptor))
  ;; remove dead streams. if no streams left, accept new else get input
  ;; from first available stream
  (let ((anything nil))
    (dolist (stream (client-streams adaptor))
      (mess 1 "Poll ~S" stream)
      (when (handle-stream adaptor stream)
        (setq anything t)))
    (when (listner-socket adaptor)
      (srv-handle-socket adaptor (listner-socket adaptor) (not anything)))))


(defun get-message (stream)
  (let ((sreq (make-server-request 
               :stream stream
               :buffer (get-work-buffer))))
    (when (read-request sreq stream)
      (decode-request sreq)
      (poa-demux sreq))))


(defun poa-demux (sreq)
  (with-slots (object-key operation) sreq
    (multiple-value-bind (reftype poaid oid) 
        (decode-object-key-poa object-key)
      (setf (server-request-oid sreq) oid)
      (let ((poa (gethash poaid *poa-map*)))
        (cond
         ((and reftype poa)
          (setf (server-request-poa sreq) poa)
          (mess 3 "Using POA ~A oid '~/clorb:stroid/'" (op:the_name poa) oid)
          ;; Check if POA is active, holding, discarding...
          (poa-invoke poa sreq))
         (t
          (set-request-exception 
           sreq (make-condition 'CORBA:OBJECT_NOT_EXIST))
          (send-response sreq)))))))

(defun root-POA (&optional (orb (orb_init)))
  (unless (adaptor orb)
    (setup-server orb))
  (unless *root-POA*
    (setq *root-poa* (create-POA nil "root" nil nil)))
  *root-POA*)

(eval-when (:load-toplevel :execute)
  (pushnew (cons "RootPOA" #'root-POA)
           *default-initial-references*
           :key #'car :test #'equal ))

(defun main-loop (&optional (orb (ORB_init)))
  (root-POA)
  (op:run orb))

(defun recover (&optional (orb (ORB_init)))
  (let ((adaptor (adaptor orb)))
    (mapc #'socket-close (client-streams-raw adaptor))
    (setf (client-streams-raw adaptor) nil))
  (main-loop orb))
