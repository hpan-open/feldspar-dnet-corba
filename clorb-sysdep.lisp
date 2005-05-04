;;;; clorb-sysdep.lisp
;; most system dependent code is collected here
;; - TCP/IP sockets implementation glue
;; - interface to select (or poll) functionality
;; - read/write of octet sequences, blocking and non-blocking
;; - multi processing
;; - external programs

(in-package :clorb)


;;; Frob with the *features* to make this all a bit easier.

#+digitool
(eval-when (:compile-toplevel :execute :load-toplevel)
  #-clorb-mcl-task-evaluator
  (progn 
    (pushnew 'clorb::mcl-bsd *features*)
    (cond ((find-package "BSD"))
          ((find-package "NET.CDDR.PACKER")
           (funcall (intern "REQUIRE-PACKAGE" "NET.CDDR.PACKER") "BSD"))
          (t
           (require "BSD")))))
  


;;; The :sockets (db-sockets) library can be used in CMUCL or SBCL:
;;; it's optional (though desirable) in the former, which otherwise
;;; uses its usual socket support (cmucl-sockets).  In SBCL we have
;;; no other option

;;; Added support for SB-BSD-SOCKETS in SBCL. It seems to be a
;;; standard contrib package.

#+(or cmu sbcl)
(eval-when (:compile-toplevel :load-toplevel)
  (cond ((find-package "SOCKETS")
         (pushnew 'clorb::db-sockets *features*))
        ((find-package "SB-BSD-SOCKETS")
         (pushnew 'clorb::sb-bsd-sockets *features*))
        (t
         #+cmu (pushnew 'clorb::cmucl-sockets *features*)
         #+sbcl
         (progn
           (require :sb-bsd-sockets)
           (pushnew 'clorb::sb-bsd-sockets *features*)           
           (unless (find-package "SB-BSD-SOCKETS")
             (error "We need the SOCKETS library; SBCL doesn't have its own")           )))))


(defmacro %sysdep (desc &rest forms)
  (when (null forms)
      (error "No system dependent code to ~A" desc))
  (car forms))


;;;; TCP/IP sockets implementation glue

#+digitool
(require "OPENTRANSPORT")

#+digitool
(defclass MCL-LISTENER-SOCKET ()
  ((port :initarg :port :accessor mcl-listener-port)
   (stream :initform nil :accessor listener-stream)))



;; the unconnected socket is returned by OPEN-PASSIVE-SOCKET and used
;; in ACCEPT-CONNECTION-ON-PORT and COERCE-TO-WAITABLE-THING

(defun open-passive-socket (port)
  "Returns an UNCONNECTED-SOCKET for a TCP socket listening on PORT.  Set SO_REUSEADDDR if possible"
  (%SYSDEP
   "open listener socket"

   #+dummy-tcp
   (list 'dummy-listener (or port 115577))

   #+(or allegro use-acl-socket)
   (socket:make-socket :connect :passive :local-port port
                       :format :binary :reuse-address t)

   #+openmcl
   (openmcl-socket:make-socket :connect :passive :local-port port
                               :format :binary :reuse-address t)

   #+clisp 
   (if port (ext:socket-server port) (ext:socket-server))

   #+clorb::cmucl-sockets 
   (ext:create-inet-listener (or port 0))

   #+clorb::db-sockets
   (let ((s (sockets:make-inet-socket :stream :tcp)))
     (setf (sockets:sockopt-reuse-address s) t)
     (sockets:socket-bind s #(0 0 0 0) (or port 0))
     (sockets:socket-listen s 5)
     s)

   #+clorb::sb-bsd-sockets
   (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                           :type :stream :protocol :tcp)))
     (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
     (sb-bsd-sockets:socket-bind s #(0 0 0 0) (or port 0))
     (sb-bsd-sockets:socket-listen s 5)
     s)

   #+digitool
   (let ((listener (make-instance 'mcl-listener-socket :port port)))
     (accept-connection-on-socket listener)
     listener)))


;; name of local host from passive socket, this is the local host, and
;; will be used to default the hostname used in object references

(defun passive-socket-host (socket)
  (declare (ignorable socket))
  (%SYSDEP
   "Get the hostname/IP of socket"

   #+dummy-tcp
   "localhost"

   #+clisp
   (ext:socket-server-host socket)

   #+(or Allegro use-acl-socket)
   (socket:ipaddr-to-hostname (socket:local-host socket))

   #+digitool
   (let ((host (ccl::stream-local-host (listener-stream socket))))
     (handler-case
       (if (zerop host)
         "localhost"
         (ccl::inet-host-name host))
       (ccl::tcp-unknown-domain-name 
        nil
        (ccl::tcp-addr-to-str host))))

   #+openmcl
   (or (ignore-errors (openmcl-socket:ipaddr-to-hostname
                       (openmcl-socket:local-host socket)))
       "localhost")

   ;; Default
   "localhost"))



;; the port of a passive socket, will be used in object references

(defun passive-socket-port (socket)
  (declare (ignorable socket))
  (%SYSDEP
   "Get the port of socket"

   #+dummy-tcp
   (second socket)

   #+clisp
   (ext:socket-server-port socket)

   #+(or Allegro use-acl-socket)
   (socket:local-port socket)

   #+openmcl
   (openmcl-socket:local-port socket)

   #+clorb::db-sockets
   (multiple-value-bind (adr port)
       (sockets:socket-name socket)
     (declare (ignore adr))
     port)

   #+clorb::sb-bsd-sockets
   (multiple-value-bind (adr port)
       (sb-bsd-sockets:socket-name socket)
     (declare (ignore adr))
     port)

   #+cmu
   (nth-value 1 (ext:get-socket-host-and-port socket))

   #+digitool
   (mcl-listener-port socket)))




(defparameter *connect-timeout* 120)

(defun open-active-socket (host port &optional (binary t))
  "Open a TCP connection to HOST:PORT, and return the stream asociated with it"
  (let ((type (if binary '(unsigned-byte 8) 'base-char)))
    (declare (ignorable type))
    (%SYSDEP
     "open socket to host/port"

     #+dummy-tcp
     (error "Dummy TCP cannot connect")

     #+clisp 
     (ext:socket-connect port host :element-type type) 

     #+clorb::cmucl-sockets
     (system:make-fd-stream (ext:connect-to-inet-socket host port)
                            :input t :output t :element-type type)

     #+clorb::db-sockets
     (let ((s (sockets:make-inet-socket :stream :tcp))
           (num (car (sockets:host-ent-addresses
                      (sockets:get-host-by-name host)))))
       (sockets:socket-connect s num port)
       (sockets:socket-make-stream s :element-type type
                                   :input t :output t :buffering :none))
     #+clorb::sb-bsd-sockets
     (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                             :type :stream :protocol :tcp))
           (num (sb-bsd-sockets:host-ent-address
                 (sb-bsd-sockets:get-host-by-name host))))
       (sb-bsd-sockets:socket-connect s num port)
       (sb-bsd-sockets:socket-make-stream s :element-type type
                                   :input t :output t :buffering :none))

     #+(or allegro use-acl-socket)
     (socket:make-socket 
      :remote-host host :remote-port port
      :format (if binary :binary :text))

     #+openmcl
     (openmcl-socket:make-socket 
      :remote-host host :remote-port port
      :format (if binary :binary :text))

     #+digitool
     (ccl::open-tcp-stream host port :element-type type 
                           :connect-timeout *connect-timeout*))))


(defun socket-peer (conn)
  "Returns: host port"
  (declare (ignorable conn))
  (%SYSDEP
   "Get host port for peer"
    #+(or allegro use-acl-socket)
    (values
     (or (ignore-errors (acl-socket:ipaddr-to-hostname (acl-socket:remote-host conn)))
         (acl-socket:ipaddr-to-dotted (acl-socket:remote-host conn)))
     (acl-socket:remote-port conn))
    #+openmcl
    (values (let ((host (openmcl-socket:remote-host conn)))
              (and host
                   (or (ignore-errors
                         (openmcl-socket:ipaddr-to-hostname host))
                       (openmcl-socket:ipaddr-to-dotted host))))
            (openmcl-socket:remote-port conn))
    #+Digitool
    (values (let ((host (ccl::stream-remote-host conn)))
              (ccl::tcp-addr-to-str host)
              #+(or)
              (ccl::inet-host-name host))
            (ccl::stream-remote-port conn))
   
   ;; Default
   nil ))

(defun accept-connection-on-socket (socket &optional (blocking nil))
  "Accept a connection on SOCKET and return the stream associated
with the new connection.  Do not block unless BLOCKING is non-NIL"
  (declare (ignorable blocking))
  (%SYSDEP
   "accept a connection"

   #+dummy-tcp
   nil

   #+clorb::cmucl-sockets
   (when blocking
     (let ((new (ext:accept-tcp-connection socket)))
       (mess 3 "Accepting tcp connection: ~S" new)
       (setq new (system:make-fd-stream new
                                        :input t :output t :element-type
                                        '(unsigned-byte 8)))
       (mess 2 " - to stream: ~S" new)
       new))
   ;;  (error "non-blocking accept() not yet implemented for cmucl sockets")

   #+clorb::db-sockets
   (let ((before (sockets:non-blocking-mode socket)))
     (unwind-protect
          (handler-case
              (progn
                (setf (sockets:non-blocking-mode socket) (not blocking))
                (let ((k (sockets:socket-accept socket)))
                  (when k
                    (setf (sockets:non-blocking-mode k) nil)
                    (sockets:socket-make-stream k
                                                :element-type '(unsigned-byte 8)
                                                :input t :output t ))))
            (sockets::interrupted-error nil))
       (setf (sockets:non-blocking-mode socket) before)))

   #+clorb::sb-bsd-sockets
   (let ((before (sb-bsd-sockets:non-blocking-mode socket)))
     (unwind-protect
         (handler-case
             (progn
               (setf (sb-bsd-sockets:non-blocking-mode socket) (not blocking))
               (let ((k (sb-bsd-sockets:socket-accept socket)))
                 (when k
                   (setf (sb-bsd-sockets:non-blocking-mode k) nil)
                   (sb-bsd-sockets:socket-make-stream k
                                                      :element-type '(unsigned-byte 8)
                                                      :input t :output t ))))
           (sb-bsd-sockets:interrupted-error nil))
       (setf (sb-bsd-sockets:non-blocking-mode socket) before)))

   #+clisp 
   (when (ext:socket-wait socket (if blocking 10 0))
     (let* ((conn (ext:socket-accept socket 
                                 :element-type '(unsigned-byte 8))))
       (mess 4 "Accepting connection from ~A"
             (ext:socket-stream-peer conn))
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

   #+openmcl
   (let ((conn (openmcl-socket:accept-connection socket :wait blocking)))
     (when conn
       (mess 4 "Accepting connection ~S" conn))
     conn)

   #+digitool
   (let* ((s (listener-stream socket)))
     (when (and s blocking)
       (flet ((ready-p (s)
                (not (eql (ccl::opentransport-stream-connection-state s) :unbnd))))
         (unless (ready-p s)
           (ccl:process-wait "waiting" #'ready-p s))))

     (let ((state (and s (ccl::opentransport-stream-connection-state s)))
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
       new))))


(defun socket-stream-closed-p (stream)
  (%SYSDEP
   "check if the stream has been closed (by other side presumably)"

   #+digitool
   (let ((state (ccl::opentransport-stream-connection-state stream)))
     (not (member state '(:outcon :dataxfer))))

   ;; Default, assume it is not closed
   (null stream)))


;; Check if input is directly available on (socket) stream
;; or eof, or other error.
(defun socket-stream-listen (stream)
  (declare (ignorable stream))
  (%SYSDEP 
   "check if input is available on socket stream"

   #+clisp
   (ext:read-byte-lookahead stream)

   #+CCL
   (or (listen stream)
       (ccl:stream-eofp stream)
       (socket-stream-closed-p stream))

   ;; Default
   (listen stream))) 

(defun socket-close (socket)
  (%SYSDEP
   "close a listener socket"

   #+clisp 
   (ext:socket-server-close socket)

   ;; default
   (close socket)))


;;;; HTTP GET

(defun http-get-ior (host port path)
  (%SYSDEP
   "get IOR from http server"
   (with-open-stream (s (open-active-socket host port nil))
     (let ((crlf (coerce (vector #\Return #\Linefeed) 'string)))
       (format s "GET ~A~A" path crlf)
       (force-output s)
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
;;(select-reset y)
;;(select-add-listener y s)
;;(select-add-stream y s input output cookie)
;;(select-wait y) => y'
;;(select-do-result y' func)
;; where: (func cookie status)


(%SYSDEP
 "make select obj"

 #+(or sbcl cmu18 cmu19)
 (defstruct SELECT
   (fds  nil)
   (cookies nil)
   (direct-input nil)
   (rset 0)
   (wset 0)
   (maxn 0 :type fixnum))

 (defstruct (SELECT (:constructor make-select))
   (value   nil)
   (cookies nil)
   (streams nil)
   (writepending nil))
 )

#+(or sbcl cmu18 cmu19)
(defmacro %add-fd (select-obj fd-number set)
  (let ((sobj '#:sobj)
        (fd   '#:fd))
    `(let ((,sobj ,select-obj)
           (,fd   ,fd-number))
      (declare (fixnum ,fd))
      (setf (select-maxn ,sobj)
       (max (select-maxn ,sobj) ,fd))
      (setf (,set ,sobj) (logior (,set ,sobj) (ash 1 ,fd))))))

#+(or sbcl cmu18 cmu19)
(defmacro %socket-file-descriptor (socket)
  (%SYSDEP
   "file descriptor for listener socket"
   #+clorb::db-sockets `(sockets:socket-file-descriptor ,socket)
   #+clorb::sb-bsd-sockets `(sb-bsd-sockets:socket-file-descriptor ,socket)
   #+(or cmu18 cmu19) socket))

#+(or sbcl cmu18 cmu19)
(defmacro %stream-fd (stream)
  (%SYSDEP
   "file descriptor for stream"
   #+(or cmu18 cmu19) `(system:fd-stream-fd ,stream)
   #+sbcl  `(sb-sys:fd-stream-fd ,stream)))

#+(or sbcl cmu18 cmu19)
(defmacro %unix-select (maxn rset wset xset timeout)
  `(#+sbcl sb-unix:unix-select #+(or cmu18 cmu19) unix:unix-select
           ,maxn ,rset ,wset ,xset ,timeout))


(defun select-add-listener (select socket)
  (declare (ignorable select socket))
  (%SYSDEP
   "add listener to select"

   #+clisp
   (progn
     (push socket (select-value select))
     (push nil (select-cookies select)))

   #+(or sbcl cmu18 cmu19)
   (%add-fd select (%socket-file-descriptor socket) select-rset)

   #+allegro
   (push (socket:socket-os-fd socket) (select-value select))

   ;; Default
   nil))


(defun select-listener (select socket)
  (declare (ignorable select socket)) 
  (%SYSDEP
   "listener ready in select result"

   #+(or sbcl cmu18 cmu19)
   (logbitp (%socket-file-descriptor socket)
            (select-rset select))

   ;; Default
   nil))


(defun select-add-stream (select stream input output cookie)
  "Add STREAM to SELECT for INPUT and/or OUTPUT.
Returns cookie that should be used after select-wait to get
status for stream."
  (declare (ignorable select stream input output)
           (optimize (speed 2)))
  (%SYSDEP
   "add stream to select"

   #+clisp
   (progn
     (push
      (cons stream
            (cond ((not input)  :output)
                  ((not output) :input)
                  (t            :io)))
      (select-value select))
     (push cookie (select-cookies select)))

   #+(or cmu18 cmu19 sbcl)
   (let ((fd (%stream-fd stream)))
     (declare (fixnum fd))
     (when input
       ;; there is buffering in the stream, need to empty the buffer
       ;; before doing a real select
       (when (socket-stream-listen stream)
         (push cookie (select-direct-input select)))
       (%add-fd select fd select-rset))
     (when output
       (%add-fd select fd select-wset))
     (push fd (select-fds select))
     (push cookie (select-cookies select)))

   #+(or allegro)
   (progn
      (when output
        (setf (select-writepending select) t))
      (when input
        (push stream (select-value select)))
      (push stream (select-streams select))
      (push cookie (select-cookies select)))

   ;; Default
   (progn
     (let ((read-ready
            (if input (socket-stream-listen stream))))
       (when (or output read-ready)
         (setf (select-writepending select) t))
       (push cookie (select-cookies select))
       (push (if read-ready
                 (if output :io :input)
                 (if output :output nil))
             (select-value select)))) ))


(defun select-wait (select poll)
  "Wait for selected streams.
Returns select result to be used in getting status for streams."
  (%SYSDEP
   "wait on selected streams"

   #+clisp
   (progn
     (mess 1 "Selecting ~A" select)
     (let ((select-list (select-value select)))
       (when select-list
         (setf (select-value select) (ext:socket-status select-list (if poll 0 60)))))
     (mess 1 "Select result ~A" (select-value select))
     select)

   #+allegro
   (progn
     (setf (select-value select)
           (mp:wait-for-input-available
            (select-value select)
            :timeout (if (or poll (select-writepending select)) 0 20)
            :whostate "wating for CORBA input"))
     select)

   #+(or cmu18 cmu19 sbcl)
   (progn
     (unless (select-direct-input select)
       (mess 2 "Enter select ~A ~A"
             (select-rset select) (select-wset select))
       (multiple-value-bind (result rset wset xset)
           (%unix-select (1+ (select-maxn select))
                         (select-rset select)
                         (select-wset select)
                         0 (if poll 0 200))
         (declare (ignorable xset))
         ;;FIXME: should perhaps use xset
         (mess 2 "Select return ~A ~A ~A ~A" result rset wset xset)
         (setf (select-rset select) rset)
         (setf (select-wset select) wset)))
     select)


   ;; Default
   (progn
     (unless (or poll (select-writepending select))
       (sleep 0.02))
     select)))


(defun select-do-result (select func)
  (%SYSDEP
   "loop thru result of a select"

   #+clisp
   (loop for cookie in (select-cookies select)
        for status in (select-value select)
        when (and cookie status)
        do (funcall func cookie status))

   #+allegro
   (loop for cookie in (select-cookies select)
        for stream in (select-streams select)
        do (funcall func cookie
                    (if (member stream (select-value select))
                        :io :output)))

   #+(or sbcl cmu18 cmu19)
   (if (select-direct-input select)
       (loop for cookie in (select-direct-input select)
            do (funcall func cookie :input))
       (loop
          with rset = (select-rset select)
          with wset = (select-wset select)
          for cookie in (select-cookies select)
          for fd in (select-fds select)
          do (let ((status (if (logbitp fd rset)
                               (if (logbitp fd wset) :io :input)
                               (if (logbitp fd wset) :output nil))))
               (when status (funcall func cookie status)))))

   ;; Default
   (loop for cookie in (select-cookies select)
        for status in (select-value select)
        do (funcall func cookie status))))

;;;; Read / Write 

(deftype octets () `(vector (unsigned-byte 8)))
(deftype index () '(integer 0 #.array-dimension-limit))


(defun read-octets (seq start end stream)
  ;; read octets into seq from start to end
  (declare (optimize speed)
           (type octets seq)
           (type index start end))
  (let ((n (read-sequence seq stream :start start :end end)))
    (when (< n (- end start))
      (error 'end-of-file :stream stream))))


(defun read-octets-no-hang (seq start end stream)
  ;; read octets into seq from start to end, may stop reading before end
  ;; if would hang return number of octets read
  (declare (optimize speed)
           (type octets seq)
           (type index start end))
  (let ((read-pos start))
    (declare (type index read-pos))
    (%SYSDEP
     "read many octets without hanging"

     #+clisp
     (progn
       (setq read-pos (ext:read-byte-sequence seq stream
                                              :start start :end end :no-hang t))
       (if (and (= read-pos start)
                (eql :eof (ext:read-byte-lookahead stream )))
           (error 'end-of-file :stream stream)))

     ;; Default
     (loop while (socket-stream-listen stream)
           do (setf (aref seq read-pos) (read-byte stream))
           (incf read-pos)
           until (= read-pos end)))

    ;; Common end:
    (- read-pos start)))


(defun write-octets (seq start end stream)
  (declare (type octets seq)
           (type index start end))
  (write-sequence seq stream :start start :end end)
  (force-output stream))

(defun write-octets-no-hang (seq start end stream)
  "returns number of octets written"
  (%SYSDEP
   "write octets without hanging"

   ;; Default is possibly blocking
   (progn
     (write-sequence seq stream :start start :end end)
     (force-output stream)
     (- end start))))



;;;; Multi process support

(defun current-process ()
  (%SYSDEP
   "return the current process"
   #+ccl ccl:*current-process*
   :current-process))

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

(defun process-wait (whostate wait-func &rest args)
  (declare (ignorable whostate))
  (%SYSDEP
   "wait for condition to come true"
   #+CCL 
   (apply #'ccl:process-wait whostate wait-func args)

   ;; Default
   (loop until (apply wait-func args)
         do (sleep 0.1))))



;;;; Running external programs

(defun shell-to-stream (command stream)
  (%SYSDEP
   "execute shell command with output to stream"
   #+openmcl
   (ccl:run-program "/bin/bash" (list "-c" command) :output stream)
   #+cmu
   (ext:run-program "/bin/sh" (list "-c" command) :output stream)
   #+sbcl
   (sb-ext:run-program "/bin/sh" (list "-c" command) :output stream)
   ;; Default
   (error "No implementation for SHELL-TO-STREAM: ~S ~A" command stream)))


(defun shell-to-string (command)
  (%SYSDEP
   "Run a command in shell with output to string"

   #+(or cmu openmcl sbcl)
   (with-output-to-string (out)
     (shell-to-stream command out))

   #+clorb::mcl-bsd
   (bsd:system-command command)

   #+clorb-mcl-task-evaluator
   (let ((string
          (ccl:send-eval (format nil "#!/bin/sh~A~A" #\Newline command)
                         "JGTaskEvaluator")))
     (substitute #\Newline #\LineFeed string))

   #+clisp 
   (with-output-to-string (out)
     (let ((shell-stream (ext:make-pipe-input-stream command)))
       (loop for c = (read-char shell-stream nil nil)
           while c do (princ c out))
       (close shell-stream)))

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
   
   (error "No implementation for SHELL-TO-STRING: ~S" command)))


(defun external-namestring (pathname)
  (setq pathname (truename pathname))
  (%SYSDEP "convert pathname to a namestring suitable for external programs"
           #+digitool
           (if (ccl::using-posix-paths-p)
             (ccl::posix-namestring pathname)
             (namestring pathname))
           ;; Default
           (namestring pathname)))


(defun cpp-command-string (file include-directories &optional defines)
  ;; FIXME: add support for defines
  (format nil
          #-MCL "cpp~{ -I'~A'~}~{ -D'~A'~} '~A'"
          ;; apples /usr/bin/cpp is buggy
          #+MCL "cpp~{ -I\"'~A'\"~}~{ -D'~A'~} \"'~A'\""
          (mapcar #'external-namestring include-directories)
          defines
          (external-namestring file)))


(defun home-volume-prefix ()
  "The directory prefix for the home volume.
Usually :ABSOLUTE but for MCL there is also a volume name. 
This is needed when translating from file:-URLs."
  (%SYSDEP "home-volume-prefix"
           #+Digitool 
           (subseq (pathname-directory (truename "home:")) 0 2)
           ;; default
           '(:ABSOLUTE)))
