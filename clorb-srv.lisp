;;;; clorb-srv.lisp --- CORBA server module

(in-package :clorb)

;; Status values
(put :normal  'reply-status 0)
(put :forward 'reply-staus 4)
(put :forward 'locate-status 2)
(put :exception 'reply-status 2)


;;;; GIOP / IIOP stuff

'(defun decode-userexception (exc)
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

(defun setup-server (&optional (orb (ORB_init)))
  (multiple-value-bind (port host)
      (io-create-listener (orb-port orb))
    (setf (adaptor orb) t)
    (setf (orb-port orb) port)
    (unless (orb-host orb)
      (setf (orb-host orb) host))
    (setup-shortcut)))

(defun setup-incoming-connection (conn)
  (connection-init-read conn nil *iiop-header-size* #'poa-request-handler))

(defun setup-shortcut-in (&optional (conn-in *shortcut-in*))
  (setup-incoming-connection conn-in))

(defun setup-shortcut ()
  (let ((orb (CORBA:ORB_init)))
    (setup-shortcut-out (orb-host orb) (orb-port orb))
    (setup-shortcut-in)))

(defun shortcut-off ()
  (let* ((orb (CORBA:ORB_init))
         (holder (get-connection-holder (orb-host orb) (orb-port orb))))
    (setf (cdr holder) nil)))

(defun poa-connection-handler (desc)
  (let ((conn (make-associated-connection desc)))
    (setup-incoming-connection conn)))

(defun poa-request-handler (conn)
  (let ((sreq (make-server-request
               :connection conn
               :buffer (connection-read-buffer conn))))
    (read-request sreq
                  (lambda (conn)
                    (setup-incoming-connection conn)
                    (decode-request sreq)
                    (poa-demux sreq)))))

(defun poa-demux (sreq)
  (with-slots (object-key operation) sreq
    (multiple-value-bind (reftype poa oid)
        (decode-object-key-poa object-key)
      (setf (server-request-oid sreq) oid)
      (cond
        ((and reftype poa)
         (setf (server-request-poa sreq) poa)
         (mess 3 "Using POA ~A oid '~/clorb:stroid/'" (op:the_name poa) oid)
         ;; Check if POA is active, holding, discarding...
         (poa-invoke poa sreq))
        (t
         (set-request-exception
          sreq (make-condition 'CORBA:OBJECT_NOT_EXIST))
         (send-response sreq))))))

(defun root-POA (&optional (orb (orb_init)))
  (unless (adaptor orb)
    (setup-server orb))
  (unless *root-POA*
    (setq *root-poa* (create-POA nil "root" nil nil))
    (setf (the-orb *root-poa*) orb))
  *root-POA*)

(defun initialize-poa (orb)
  (set-initial-reference orb "RootPOA" #'root-POA)
  (set-initial-reference orb "POACurrent" nil
                         (make-instance 'PortableServer::Current)))


(eval-when (:load-toplevel :execute)
  (setq *new-connection-callback* 'poa-connection-handler)
  (pushnew 'initialize-poa *orb-initializers* ))

(defun main-loop (&optional (orb (ORB_init)))
  (root-POA)
  (op:run orb))

(defun recover (&optional (orb (ORB_init)))
  (io-reset :listener nil)
  (main-loop orb))


;;; clorb-srv.lisp ends here
