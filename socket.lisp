(in-package :clorb)

;;; TCP/IP sockets implementation glue

;;; Frob with the *features* to make this all a bit easier.

(eval-when (:compile-toplevel :execute)
  #+clisp
  (if (find-package "EXT")
      (pushnew :clisp-ext *features*)))


;;; The :sockets (db-sockets) library can be used in CMUCL or SBCL:
;;; it's optional (though desirable) in the former, which otherwise
;;; uses its usual socket support (cmucl-sockets).  In SBCL we have
;;; no other option

#+(or cmu sbcl)
(eval-when (:compile-toplevel :load-toplevel)
  (if (find-package "SOCKETS")
      (pushnew :db-sockets *features*)
    #+cmu (pushnew :cmucl-sockets *features*)
    #+sbcl (error "We need the SOCKETS library; SBCL doesn't have its own")))

#+mcl 
(require "OPENTRANSPORT")


(defmacro %SYSDEP (desc &rest forms)
  (when (null forms)
      (error "No system dependent code to ~A" desc))
  (car forms))


#+mcl
(defclass mcl-listner-socket ()
  ((port :initarg :port :accessor mcl-listner-port)
   (stream :initform nil :accessor listner-stream)))


;; the unconnected socket is returned by OPEN-PASSIVE-SOCKET and used
;; in ACCEPT-CONNECTION-ON-PORT and COERCE-TO-WAITABLE-THING

;;; In Acl this is some type internal to the socket packet, I rather not
;; put in a dependency on that.                    [lenst/2000-05-18 21:07:21]
;(deftype unconnected-socket ()
;  #+allegro 'integer                    ;this is a complete guess
;  #+clisp 'socket-server                ;I checked this one
;  #+db-sockets 'sockets:inet-socket
;  #+cmucl-sockets 'integer
;  )

;; connected sockets must be streams

(defun open-passive-socket (port)
  "Returns an UNCONNECTED-SOCKET for a TCP socket listening on PORT.  Set SO_REUSEADDDR if possible"
  (%SYSDEP
   "open listner socket"
   #+clisp 
   (if port (socket-server port) (socket-server))
   #+cmucl-sockets 
   (ext:create-inet-listener port)
   #+db-sockets
   (let ((s (sockets:make-inet-socket :stream :tcp)))
     (setf (sockets:sockopt-reuse-address s) t)
     (sockets:socket-bind s #(0 0 0 0) (or port 0))
     (sockets:socket-listen s 5)
     s)
   #+allegro
   (socket:make-socket :connect :passive :local-port port
                       :format :binary
                       :reuse-address t)
   #+mcl
   (let ((listner (make-instance 'mcl-listner-socket :port port)))
     (accept-connection-on-socket listner)
     listner)))

(defun passive-socket-host (socket)
  (%SYSDEP
   "Get the hostname/IP of socket"
   #+clisp
   (socket-server-host socket)
   #+mcl
   (ccl::inet-host-name (ccl::stream-local-host (listner-stream listner-socket)))
   ;;#+Allegro (socket:ipaddr-to-hostname (socket:local-host socket))
   ;; Default
   (or *host* "localhost")))


(defun passive-socket-port (socket)
  (declare (ignorable socket))
  (%SYSDEP
   "Get the port of socket"
   #+clisp
   (socket-server-port socket)
   #+Allegro
   (socket:local-port socket)
   #+db-sockets
   (multiple-value-bind (adr port)
       (sockets:socket-name socket)
     (declare (ignore adr))
     port)
   ;; Default
   *port* ))


(defun open-active-socket (host port)
  "Open a TCP connection to HOST:PORT, and return the stream asociated with it"
  (%SYSDEP
   "open socket to host/port"
   #+clisp 
   (socket-connect port host :element-type '(unsigned-byte 8))
   #+cmucl-sockets
   (system:make-fd-stream (ext:connect-to-inet-socket host port)
                          :input t :output t :element-type
                          '(unsigned-byte 8))
   #+db-sockets
   (let ((s (sockets:make-inet-socket :stream :tcp))
         (num (car (sockets:host-ent-addresses
                    (sockets:get-host-by-name host)))))
     (sockets:socket-connect s num port)
     (sockets:socket-make-stream s :element-type '(unsigned-byte 8)
                                 :input t :output t :buffering :none))
   #+allegro
   (socket:make-socket 
    :remote-host host :remote-port port
    :format :binary)

   #+mcl
   (ccl::open-tcp-stream host port :element-type '(unsigned-byte 8))))


(defun accept-connection-on-socket (socket &optional (blocking nil))
  "Accept a connection on SOCKET and return the stream associated
with the new connection.  Do not block unless BLOCKING is non-NIL"
  ;; XXX this probably races on clisp - the client can go away between
  ;; select and accept.  Dunno what allegro does
  (declare (ignorable blocking))
  (%SYSDEP
   "accept a connection"
   #+cmucl-sockets
   (when blocking
     (let ((new (ext:accept-tcp-connection socket)))
       (mess 3 "Acception tcp connection: ~S" new)
       (setq new (system:make-fd-stream new
                                        :input t :output t :element-type
                                        '(unsigned-byte 8)))
       (mess 2 " - to stream: ~S" new)
       new))
   ;;  (error "non-blocking accept() not yet implemented for cmucl sockets")
   #+db-sockets
   (let ((before (sockets:non-blocking-mode socket)))
     (unwind-protect
          (handler-case
              (progn
                (setf (sockets:non-blocking-mode socket) (not blocking))
                (let ((k (sockets:socket-accept socket)))
                  (setf (sockets:non-blocking-mode k) nil)
                  (sockets:socket-make-stream k :element-type '(unsigned-byte 8)
                                              :input t :output t )))
            (sockets::interrupted-error nil))
       (setf (sockets:non-blocking-mode socket) before)))
   #+clisp 
   (when (socket-wait socket 0 10)
     (let* ((conn (socket-accept socket 
                                 :element-type '(unsigned-byte 8))))
       (mess 4 "Accepting connection from ~A"
             (socket-stream-peer conn))
       conn))
   #+allegro 
   (let ((conn (socket:accept-connection socket :wait blocking)))
     (mess 4 "Accepting connection from ~A:~D"
           (socket:ipaddr-to-hostname (socket:remote-host conn))
           (socket:remote-port conn) )
     conn)
   #+mcl
   (let* ((s (listner-stream socket))
          (state (and s (ccl::opentransport-stream-connection-state s)))
          (new nil))
     (when (member state '(:incon :dataxfer))
       (mess 3 "Accepting connection ~S" s)
       (setq new s
             state nil))
     (when (member state '(nil :uninit :closed))
       (mess 3 "New listner replacing ~S" s)
       (or (mcl-listner-port socket)
           (error "MCL OpenTransport needs explicit port number for listner socket"))
       (setf (listner-stream socket)
             (ccl::open-tcp-stream nil (mcl-listner-port socket) 
                                   :element-type '(unsigned-byte 8)
                                   :reuse-local-port-p t)))
     new)))



;;; coerce an unconnected-socket to something intelligible to
;;; wait-for-input-on-streams
(defun coerce-to-waitable-thing (i)
  #+allegro (if (streamp i) i (socket:socket-os-fd i))
  #-allegro i
  ;; The following is not needed right now    [lenst/2000-05-18 21:08:38]
  #|(etypecase i (stream i) (unconnected-socket))|#)


;; Check if input is directly available on (socket) stream
;; if this returns false positives and wait-for-input-on-streams
;; returns :cant the server functionallity is severly reduced :()
(defun socket-stream-listen (stream)
  (declare (ignorable stream))
  (%SYSDEP 
   "check if input is available on socket stream"
   #+clisp-ext
   (not (ext:read-byte-will-hang-p stream))
   #+clisp t                            ;Ouch!
   ;; Default
   (listen stream)))


(defun socket-stream-functional-p (stream)
  "True if stream is a functional connection.
False if, e.g., the peer has closed."
  (%SYSDEP
   "check if stream is functional"
   #+mcl
   (and stream
        (eq (ccl::opentransport-stream-connection-state stream)
            :dataxfer))
   ;; Default
   (and stream (open-stream-p stream))))


(defun socket-close (socket)
  (%SYSDEP
   "close a listener socket"
   #+clisp 
   (socket-server-close socket)
   ;; default
   (close socket)))



(defun wait-for-input-on-streams (server-sockets streams)
  "SERVER-SOCKETS is a list of UNCONNECTED-SOCKETs.  
STREAMS is a list whose elements are STREAMs. Wait for a while,
returning as soon as one of them might be ready for input. 
Return 
  NIL - if no input available
  :SERVER SOCKET  - if unconnected-socket SOCKET can accept conn.
  :STREAM STREAM  - if input available from STREAM
  :CANT           - if the implementation doesn't support waiting
"
  (declare (ignorable server-sockets streams))
  (%SYSDEP 
   "wait for for input on streams and sockets"

   #+(and sbcl unix) 
   (let ((all-waitable (mapcar #'sb-sys:fd-stream-fd streams)))
     (dolist (socket server-sockets)
       (push (sockets:socket-file-descriptor socket) all-waitable))
     (let ((rset 0) (maxn 0))
       (dolist (n all-waitable)
         (incf rset (ash 1 n))
         (setq maxn (max n maxn)))
       (mess 2 "Selecting set ~A" rset)
       (multiple-value-bind (result readable wset xset)
           (sb-unix:unix-select (1+ maxn) rset 0 0 10)
         (mess 2 "Select return ~A ~A" result readable)
         (block findit
           (dolist (socket server-sockets)
             (when (logbitp (sockets:socket-file-descriptor socket)
                            readable)
               (return-from findit (values :server socket))))
           (dolist  (stream streams)
             (when (logbitp (sb-sys:fd-stream-fd stream)
                            readable)
               (return-from findit (values :stream stream))))
           nil))))
   
   #+allegro 
   (let ((all-waitable streams))
     (dolist (socket server-sockets)
       (push (socket:socket-os-fd socket) all-waitable))
     (let ((ready 
            (handler-case
                (mp:wait-for-input-available 
                 all-waitable
                 :timeout 20
                 :whostate "wating for CORBA input")
              (type-error (err)
                ;; Seems like this can happen if the other end is
                ;; closing the socket. The caller should remove these
                ;; streams before calling again!
                (mess 3 "Error while waiting for input: ~A" err)
                nil))))
       (if (not ready)
           nil
           (if (member (car ready) streams)
               (values :stream (car ready))
               (values :server
                       (loop for s in server-sockets
                             when (eql (car ready) (socket:socket-os-fd s)) 
                             return s))))))
   
   ;; Default
   (progn (sleep 0.01)
          :cant)))
