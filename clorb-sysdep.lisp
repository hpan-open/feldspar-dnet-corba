;;;; clorb-sysdep.lisp
;; most system dependent code is collected here
;; - TCP/IP sockets implementation glue
;; - interface to select (or poll) functionality
;; - read/write of octet sequences, blocking and non-blocking
;; - multi processing
;; - external programs

(in-package :clorb)


;;; Frob with the *features* to make this all a bit easier.

(eval-when (:compile-toplevel :execute)
  #+clisp
  (if (find-package "EXT")
      (pushnew :clisp-ext *features*))
  #+clisp
  (pushnew :clisp-new-socket-status *features*)
  #+openmcl
  (pushnew :use-acl-socket *features*)
  #+(and MCL (not openmcl))
  (when (find-package "BSD")
    (pushnew :clorb-mcl-bsd *features*)))
  


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


(defmacro %SYSDEP (desc &rest forms)
  (when (null forms)
      (error "No system dependent code to ~A" desc))
  (car forms))


;;;; TCP/IP sockets implementation glue

#+(and mcl (not openmcl))
(require "OPENTRANSPORT")

#+(and mcl (not openmcl))
(defclass mcl-listner-socket ()
  ((port :initarg :port :accessor mcl-listener-port)
   (stream :initform nil :accessor listener-stream)))


;; the unconnected socket is returned by OPEN-PASSIVE-SOCKET and used
;; in ACCEPT-CONNECTION-ON-PORT and COERCE-TO-WAITABLE-THING

(defun open-passive-socket (port)
  "Returns an UNCONNECTED-SOCKET for a TCP socket listening on PORT.  Set SO_REUSEADDDR if possible"
  (%SYSDEP
   "open listener socket"
   #+dummy-tcp
   (list 'dummy-listner (or port 115577))
   #+(or allegro use-acl-socket)
   (socket:make-socket :connect :passive :local-port port
                       :format :binary
                       :reuse-address t)
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
   #+mcl
   (let ((listener (make-instance 'mcl-listener-socket :port port)))
     (accept-connection-on-socket listener)
     listener)))

(defun passive-socket-host (socket)
  (declare (ignorable socket))
  (%SYSDEP
   "Get the hostname/IP of socket"
   #+dummy-tcp
   "localhost"
   #+clisp
   (socket-server-host socket)
   #+(and mcl (not use-acl-socket))
   (let ((host (ccl::stream-local-host (listener-stream socket))))
     (handler-case
       (ccl::inet-host-name host)
       (ccl::tcp-unknown-domain-name 
        nil
        (ccl::tcp-addr-to-str host))))
      
   ;;#+Allegro (socket:ipaddr-to-hostname (socket:local-host socket))
   ;; Default
   "localhost"))


(defun passive-socket-port (socket)
  (declare (ignorable socket))
  (%SYSDEP
   "Get the port of socket"
   #+dummy-tcp
   (second socket)
   #+clisp
   (if socket (socket-server-port socket) *port*)
   #+(or Allegro use-acl-socket)
   (socket:local-port socket)
   #+db-sockets
   (multiple-value-bind (adr port)
       (sockets:socket-name socket)
     (declare (ignore adr))
     port)
   #+mcl
   (mcl-listener-port socket)))




(defun open-active-socket (host port &optional (binary t))
  "Open a TCP connection to HOST:PORT, and return the stream asociated with it"
  (let ((type (if binary '(unsigned-byte 8) 'base-char)))
    (declare (ignorable type))
    (%SYSDEP
     "open socket to host/port"
     #+dummy-tcp
     (error "Dummy TCP can connect")
     #+clisp 
     (socket-connect port host :element-type type) 
     #+cmucl-sockets
     (system:make-fd-stream (ext:connect-to-inet-socket host port)
                            :input t :output t :element-type type)
     #+db-sockets
     (let ((s (sockets:make-inet-socket :stream :tcp))
           (num (car (sockets:host-ent-addresses
                      (sockets:get-host-by-name host)))))
       (sockets:socket-connect s num port)
       (sockets:socket-make-stream s :element-type type
                                   :input t :output t :buffering :none))
     #+(or allegro use-acl-socket)
     (socket:make-socket 
      :remote-host host :remote-port port
      :format (if binary :binary :text))
     
     #+mcl
     (ccl::open-tcp-stream host port :element-type type))))


(defun accept-connection-on-socket (socket &optional (blocking nil))
  "Accept a connection on SOCKET and return the stream associated
with the new connection.  Do not block unless BLOCKING is non-NIL"
  (declare (ignorable blocking))
  (%SYSDEP
   "accept a connection"
   #+dummy-tcp
   nil
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
                 (sockets:socket-make-stream k
                                             :element-type '(unsigned-byte 8)
                                             :input t :output t )))
           (sockets::interrupted-error nil))
       (setf (sockets:non-blocking-mode socket) before)))
   #+clisp 
   (when (socket-wait socket (if blocking 10 0))
     (let* ((conn (socket-accept socket 
                                 :element-type '(unsigned-byte 8))))
       (mess 4 "Accepting connection from ~A"
             (socket-stream-peer conn))
       conn))
   #+(or allegro use-acl-socket)
   (let ((conn (socket:accept-connection socket :wait blocking)))
     (when conn
       (mess 4 "Accepting connection from ~A:~D"
             (or (ignore-errors
                  (socket:ipaddr-to-hostname (socket:remote-host conn)))
                 (acl-socket:ipaddr-to-dotted (socket:remote-host conn)))
             (socket:remote-port conn)))
     conn)
   #+mcl
   (let* ((s (listener-stream socket))
          (state (and s (ccl::opentransport-stream-connection-state s)))
          (new nil))
     (when (member state '(:incon :dataxfer))
       (mess 3 "Accepting connection ~S" s)
       (setq new s
             state nil))
     (when (member state '(nil :uninit :closed))
       (mess 3 "New listener replacing ~S" s)
       (or (mcl-listener-port socket)
           (error "MCL OpenTransport needs explicit port number for listener socket"))
       (setf (listener-stream socket)
         (ccl::open-tcp-stream nil (mcl-listener-port socket) 
                               :element-type '(unsigned-byte 8)
                               :reuse-local-port-p t)))
     new)))


;; Check if input is directly available on (socket) stream
;; if this returns false positives and wait-for-input-on-streams
;; returns :cant the server functionallity is severly reduced :()
(defun socket-stream-listen (stream)
  (declare (ignorable stream))
  (%SYSDEP 
   "check if input is available on socket stream"

   #+clisp-ext
   (eq t (ext:read-byte-lookahead stream))
   ;;(not (ext:read-byte-will-hang-p stream))
   #+clisp t                            ;Ouch!

   ;; Default
   (listen stream))) 

(defun socket-close (socket)
  (%SYSDEP
   "close a listener socket"
   #+clisp 
   (socket-server-close socket)
   ;; default
   (close socket)))


;;;; HTTP GET

(defun http-get-ior (host port path)
  (%SYSDEP
   "get IOR from http server"
   (with-open-stream (s (open-active-socket host port nil))
     (let ((crlf (coerce (vector #\Return #\Linefeed) 'string)))
       (format s "GET /~A~A" path crlf)
       (let ((ior (read-line s)))
         (cond 
          ((and (stringp ior)
                (> (length ior) 4)
                (string= "IOR:" ior :end2 4))
           (setq ior (string-right-trim crlf ior)))
          (t
           (mess 4 "Non IOR from http://~A:~A~A~% >> ~S" 
                 host port path ior)
           nil)))))))


;;;; Select interface
;;(make-select) => y
;;(select-add-listener y s) => cookie?
;;(select-add-stream y s input output)   => cookie
;;(select-wait y) => y'
;;(select-stream-status y cookie) => :input/:output/:io/:error

#+clisp
(eval-when (:compile-toplevel)
  (pushnew :clisp-new-socket-status *features*))


(%SYSDEP
 "make select obj"

 #+sbcl
 (defstruct select
   (rset 0)
   (wset 0)
   (maxn 0 :type fixnum))

 (defstruct (select (:constructor make-select))
   value
   (writepending nil))
 )

#+sbcl
(defmacro %add-fd (select-obj fd-number set)
  (let ((sobj '#:sobj)
        (fd   '#:fd))
    `(let ((,sobj ,select-obj)
           (,fd   ,fd-number))
      (declare (fixnum ,fd))
      (setf (select-maxn ,sobj)
       (max (select-maxn ,sobj) ,fd))
      (setf (,set ,sobj) (logior (,set ,sobj) (ash 1 ,fd))))))


(defmacro select-add-listener (select-obj socket)
  (declare (ignorable select-obj socket))
  (%SYSDEP
   "add listener to select"

   #+clisp-new-socket-status
   `(length (push ,socket (select-value ,select-obj)))

   #+sbcl
   `(%add-fd ,select-obj
     (sockets:socket-file-descriptor ,socket)
     select-rset)

   #+allegro
   `(push (socket:socket-os-fd ,socket) (select-value ,select-obj))

   ;; Default
   nil))


(defun select-add-stream (select stream input output)
  "Add STREAM to SELECT for INPUT and/or OUTPUT.
Returns cookie that should be used after select-wait to get
status for stream."
  (declare (ignorable select stream input output)
           (optimize (speed 2)))
  (%SYSDEP
   "add stream to select"

   #+clisp-new-socket-status
   (length (push
             (cons stream
              (cond ((not input)  :output)
                    ((not output) :input)
                    (t            :io)))
             (select-value select)))

   #+clisp
   (length (push stream (select-value select)))

   #+sbcl
   (let ((fd (sb-sys:fd-stream-fd stream)))
     (declare (fixnum fd))
     (when input
       (%add-fd select fd select-rset))
     (when output
       (%add-fd select fd select-wset))
     fd)

   #+allegro
   (progn
      (when output
        (setf (select-writepending select) t))
      (when input
        (car (push stream (select-value select)))))

   ;; Default
   0))


(defun select-wait (select)
  "Wait for selected streams.
Returns select result to be used in getting status for streams."
  (%SYSDEP
   "wait on selected streams"

   #+clisp
   (let (result)
     ;;(sleep 2)
     (mess 1 "Selecting ~A" select)
     ;; FIXME: can use larger timeout when socket-status is fixed
     ;; to work on server sockets.
     (let ((select-list (select-value select)))
       (when select-list
         (setq result (socket:socket-status (nreverse select-list) 10))))
     (mess 1 "Select result ~A" result)
     result)

   #+allegro
   (mp:wait-for-input-available
    (select-value select)
    :timeout (if (select-writepending select) 0 20)
    :whostate "wating for CORBA input")

   #+sbcl
   (multiple-value-bind (result rset wset xset)
       (sb-unix:unix-select (1+ (select-maxn select))
                            (select-rset select)
                            (select-wset select)
                            0 20)
     (declare (ignorable xset))
     ;;FIXME: should perhaps use xset
     (mess 2 "Select return ~A ~A ~A ~A" result rset wset xset)
     (setf (select-rset select) rset)
     (setf (select-wset select) wset)
     select)

   ;; Default
   select))


(defmacro select-stream-status (select-res cookie)
  `(%SYSDEP
    "get stream status"

    #+clisp
    (elt ,select-res (1- ,cookie))

    #+sbcl
    (if (logbitp ,cookie (select-rset ,select-res))
        (if (logbitp ,cookie (select-wset ,select-res))
            :io
            :input)
        (if (logbitp ,cookie (select-wset ,select-res))
            :output
            nil))
    #+allegro
    (if (member ,cookie ,select-res)
        :io :output)

    ;; Default
    :io))



;;;; Read / Write 

(deftype octets () `(vector (unsigned-byte 8)))
(deftype index () '(integer 0 #.array-dimension-limit))


(defun read-octets (seq start end stream)
  ;; read octets into seq from start to end
  (declare (optimize speed)
           (type octets seq)
           (type index start end))
  #+(and mcl (not xxuse-acl-socket))
  (let ((read-pos start))
    (declare (type index read-pos))
    (loop while (< read-pos end)
          do (setf (aref seq read-pos) (read-byte stream t))
          (incf read-pos)))
  #-(and mcl (not xxuse-acl-socket))
  (let ((n (read-sequence seq stream :start start :end end)))
    (when (< n (- end start))
      (error 'end-of-file :stream stream))))


(defun read-octets-no-hang (seq start end stream)
  ;; read octets into seq from start to end, may stop reading before end if would hang
  ;; return number of octets read
  (declare (optimize speed)
           (type octets seq)
           (type index start end))
  (let ((read-pos start))
    (declare (type index read-pos))
    (%SYSDEP
     "read many octets without hanging"
     #+clisp
     (loop for byte = (ext:read-byte-no-hang stream)
           while byte do
           (setf (aref seq read-pos) byte)
           (incf read-pos)
           until (= read-pos end))
     ;; Default
     (loop while (listen stream)
           do (setf (aref seq read-pos) (read-byte stream))
           (incf read-pos)
           until (= read-pos end)))
    ;; Common end:
    (- read-pos start)))


(defun write-octets (seq start end stream)
  (loop for i from start below end
        do (write-byte (aref seq i) stream))
  (force-output stream))

(defun write-octets-no-hang (seq start end stream)
  "returns number of octets written"
  (%SYSDEP
   "write octets without hanging"
   #+clisp
   (let ((count 0))
     (case (ext:socket-status stream 0)
       ((:output :io)
        (loop repeat 100
              while (< start end)
              do (write-byte (aref seq start) stream)
              (incf start) (incf count)))
       (:error (error "Stream status error")))
     (force-output stream)
     count)

   #+(and mcl (not openmcl))
   (progn
     (loop for i from start below end
         do (write-byte (aref seq i) stream))
     (force-output stream)
     (- end start))

   ;; Default is possibly blocking
   (progn
     (write-sequence seq stream :start start :end end)
     (force-output stream)
     (- end start))))



;;;; Multi process support

(defun start-process (options proc &rest args)
  (declare (ignorable options))
  (%SYSDEP
   "start a process"
   #+mcl (apply #'ccl:process-run-function options proc args)

   ;; Default: just do it
   (progn (apply proc args)
          nil)))

(defun end-process (process)
  (and process
       (%SYSDEP "end process"
                #+mcl (ccl:process-reset process nil :kill)
                nil)))

(defun process-wait-with-timeout (whostate timeout wait-func &rest args)
  (declare (ignorable whostate timeout wait-func args))
  (%SYSDEP
   "suspend process until function returns true"
   #+mcl
   (apply #'ccl:process-wait-with-timeout whostate timeout wait-func args)
   ;; Default: ignore
   nil ))


;;;; Running external programs

(defun shell-to-string-or-stream (command)
  (%SYSDEP
   "Run a command in shell with output to string"

   #+openmcl
   (with-output-to-string (out)
     (ccl:run-program "/bin/bash" (list "-c" command)
                      :output out))

   #+clorb-mcl-bsd
   (bsd:system-command command)

   #+(and mcl (not openmcl))
   (let ((string
          (ccl:send-eval (format nil "#!/bin/sh~A~A" #\Newline command)
                         "JGTaskEvaluator")))
     (substitute #\Newline #\LineFeed string))

   #+clisp 
   (make-pipe-input-stream command)
   
   #+excl
   (with-output-to-string (out)
     (multiple-value-bind (shell-stream error-stream process)
         (excl:run-shell-command command :wait nil :output :stream )
       (loop for c = (read-char shell-stream nil nil)
           while c do (princ c out))
       (when process
         (loop (when (sys:reap-os-subprocess :pid process :wait nil)
                 (return))))
       (close shell-stream)
       (when error-stream (close error-stream))))
   
   ;; No default
   ;; should perhaps signal error (runtime) instead of compile time error
   ))


(defun external-namestring (pathname)
  (%SYSDEP "convert pathname to a namestring suitable for external programs"
           #+(and MCL (not openmcl))
           (if (ccl::using-posix-paths-p)
             (ccl::posix-namestring pathname)
             (namestring pathname))
           ;; Default
           (namestring pathname)))
