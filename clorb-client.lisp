;;;; Connection handling

(in-package :clorb)


(defvar *iiop-connections* nil
  "All active client sockets.
Organized as two levels of a-lists:
  ( (host . ((port . socket) ...)) ...)
Where host is a string and port an integer.")

(defun get-connection (host port)
  (let* ((hp				; A host-ports pair
	  (assoc host *iiop-connections* :test #'equal))
	 (pp				; A port-socket pair
	  (assoc port (cdr hp))))
    (unless (and pp (open-stream-p (cdr pp)))
      (unless hp
	(push (setq hp (cons host nil)) *iiop-connections*))
      (let ((sock (open-active-socket host port)))
	(if pp
	    (setf (cdr pp) sock)
	  (push (setq pp (cons port sock)) (cdr hp)))))
    (cdr pp)))

(defun get-clients ()
  (loop for hp in *iiop-connections*
	nconc (loop for pp in (cdr hp) collect (cdr pp))))

