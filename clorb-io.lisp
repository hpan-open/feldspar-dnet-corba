;;
;; io-reset ()
;; io-create-listner (&optional port)
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

;;;; New select interface
;;(make-select) => y
;;(%add-listner y s) => cookie?
;;(select-add-stream y s input output)   => cookie
;;(select-wait y) => y'
;;(%select-stream-status y cookie) => :input/:output/:io/:error

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


(defmacro %value (x) `(select-value ,x))

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


(defmacro %add-listner (select-obj socket)
  (declare (ignorable select-obj socket))
  (%SYSDEP
   "add listner to select"

   #+clisp-new-socket-status
   `(length (push ,socket (%value ,select-obj)))

   #+sbcl
   `(%add-fd ,select-obj
     (sockets:socket-file-descriptor ,socket)
     select-rset)

   #+allegro
   `(push (socket:socket-os-fd ,socket) (%value ,select-obj))

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
             (%value select)))

   #+clisp
   (length (push stream (%value select)))

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
        (car (push stream (%value select)))))

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
     (let ((select-list (%value select)))
       (when select-list
         (setq result (socket:socket-status (nreverse select-list) 10))))
     (mess 1 "Select result ~A" result)
     result)

   #+allegro
   (mp:wait-for-input-available
    (%value select)
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


(defmacro %select-stream-status (select-res cookie)
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


;;;; IO Layer

(defstruct io-descriptor
  status
  error
  ;; -- internal --
  stream
  (read-buffer  nil :type (or null octets))
  (read-pos       0 :type buffer-index)
  (read-limit     0 :type buffer-index)
  (write-buffer nil :type (or null octets))
  (write-pos      0 :type buffer-index)
  (write-limit    0 :type buffer-index))

;; FIXME: define and document status codes
;; - define what error contain (e.g. a condition)
;; ~ status member nil :connected :broken


(defvar *io-socket* nil
  "Listner socket (or similar). Producer of connections.")

(defvar *io-descriptions* nil
  "List of all io-descriptors that we will do I/O on.")


(defun io-fix-broken ()
  (setq *io-descriptions*
        (loop for desc in *io-descriptions*
              if (eq :broken (io-descriptor-status desc))
              do (close (io-descriptor-stream desc))
              else collect desc)))

(defun io-reset (&key (listner t))
  (when (and listner *io-socket*)
    (ignore-errors
      (socket-close *io-socket*))
    (setq *io-socket* nil))
  (dolist (desc *io-descriptions*)
    (ignore-errors
      (close (io-descriptor-stream desc)))
    (setf (io-descriptor-status desc) :broken))
  (setq *io-descriptions* nil))


(defun io-create-listner (&optional port)
  (setq *io-socket* (open-passive-socket port))
  (setq port (passive-socket-port *io-socket*))
  (mess 3 "listner created on ~A" port)
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

(defun io-descriptor-connect (desc host port)
  (setf (io-descriptor-stream desc)
        (open-active-socket host port))
  (mess 3 "connect to ~A:~A = ~A" host port (io-descriptor-stream desc))
  (setf (io-descriptor-status desc) :connected))


(defun io-descriptor-set-read (desc buf start end)
  (setf (io-descriptor-read-buffer desc) buf
        (io-descriptor-read-pos desc) start
        (io-descriptor-read-limit desc) end))


(defun io-descriptor-set-write (desc buf start end)
  (setf (io-descriptor-write-buffer desc) buf
        (io-descriptor-write-pos desc) start
        (io-descriptor-write-limit desc) end))


(defun io-read-bytes-no-hang (seq start end stream)
  (declare (optimize speed)
           (type octets seq)
           (type buffer-index start end))
  (let ((read-pos start))
    (declare (type buffer-index read-pos))
    (%SYSDEP
     "read many bytes without hanging"
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

(defun io-write-bytes-no-hang (seq start end stream)
  "returns number of bytes written"
  (%SYSDEP
   "write bytes without hanging"
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

   #+mcl
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
           (let ((n (io-read-bytes-no-hang read-buffer read-pos
                                           read-limit stream)))
             (incf read-pos n)
             (if (>= read-pos read-limit)
                 :read-ready))
         (error (e)
           (setf (io-descriptor-status desc) :broken)
           (setf (io-descriptor-error desc)  e)
           :error)))
     (when (and write-buffer (< write-pos write-limit)
                (member status '(:output :io)))
       (handler-case
           (let ((n (io-write-bytes-no-hang write-buffer write-pos
                                            write-limit stream)))
             (incf write-pos n)
             (if (>= write-pos write-limit)
                 :write-ready))
         (error (e)
           (setf (io-descriptor-status desc) :broken)
           (setf (io-descriptor-error desc)  e)
           :error)))
     (if (eq status :error)
         :error))))


(defun io-driver ()
  (declare (optimize debug))
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
         #+ignore-not-needed
         (let ((event (io-poll-desc desc :io)))
           (if event
               (return-from io-driver (values event desc))))
         (let ((input (and read-buffer (< read-pos read-limit)))
               (output (and write-buffer (< write-pos write-limit))))
           (when (or input output)
             (push (cons (select-add-stream select stream input output)
                         desc)
                   cookies))))))
   (when *io-socket*
     (%add-listner select *io-socket*))
   (setq select (select-wait select))
   (loop for (cookie . desc) in cookies
         for status = (%select-stream-status select cookie)
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
