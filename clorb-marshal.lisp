;;;; Marshal

(in-package :clorb)

(defun marshal-octet (n buffer)
  (declare (type buffer buffer)
           (optimize (speed 3) (debug 0)))
  (vector-push-extend n (buffer-octets buffer) 400))

(defun marshal-bool (s buffer)
  (marshal-octet (if s 1 0) buffer))

(defun marshal-align (n buffer)
  (declare (type buffer buffer)
           (type (integer 0 8) n)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((octets (buffer-octets buffer))
         (start-pos (buffer-start-pos buffer))
         (skip (- n (logand (- (the buffer-index (fill-pointer octets))
                               start-pos) 
                            (- n 1)))))
    (when (< skip n)
      (dotimes (x skip)
        (declare (type (integer 0 8) x))
        (vector-push-extend 0 octets) ))))

#-clisp
(defun marshal-number (n size buffer)
  (declare (fixnum size)
           (integer n)
           (optimize (speed 3) (debug 0) (safety 1)))
  (marshal-align size buffer)
  (loop for p from 0 by 8
	for c fixnum below size
	do (marshal-octet (ldb (byte 8 p) n) buffer)))

;;
(#-clisp define-compiler-macro #+clisp defmacro
         marshal-number (&whole form n size buffer)
  (if (numberp size)
      (let ((nvar '#:nvar)
            (nnvar '#:nnvar)
            (bvar '#:bvar))
        `(let* (,@(if (> size 3) 
                      `((,nnvar ,n) 
                        (,nvar (logand ,nnvar #16rFFFFFF)))
                      `((,nvar ,n)))
                (,bvar ,buffer))
          ,@(if (> size 3)
                `((declare (type (integer 0 #16rFFFFFF) ,nvar))))
          (marshal-align ,size ,bvar)
          (marshal-octet (logand #16rff ,nvar) ,bvar)
          ,@(loop for p from 8 by 8
                  for c from 1 below size
                  collect `(marshal-octet (ldb (byte 8 ,p) 
                                           ,(if (>= c 3) nnvar nvar)) 
                            ,bvar))))
      form))

(defun marshal-ushort (n buffer)
  (declare (type (or CORBA:ushort CORBA:short) n)
           (optimize speed))
  (marshal-number n 2 buffer))

(defun marshal-ulong (n buffer)
  (declare (type CORBA:ulong n)
           (optimize speed))
  (marshal-number n 4 buffer))

(defun marshal-long (n buffer)
  (declare (type CORBA:long n)
           (optimize speed))
  (marshal-number n 4 buffer))

(defun float-as-ieee-integer (number sign-bit fraction-bits bias)
  (multiple-value-bind (frac expn sign)
                       (integer-decode-float number)
    (if (zerop frac)
      (ash (if (< sign 0) 1 0) sign-bit)
      (let* ((len (integer-length frac))
             (shift (+ 1 (- fraction-bits len))))
        (unless (zerop shift)
          (mess 2 "Shift=~D" shift))
        (logior (ash (if (< sign 0) 1 0) sign-bit)
                (ash (+ expn (- shift) fraction-bits bias) fraction-bits)
                (- (ash frac shift) (ash 1 fraction-bits)))))))

(defun marshal-string (s buffer)
  (marshal-ulong (1+ (length s)) buffer)
  (loop for c across s
        do (marshal-octet (char-code c) buffer))
  (marshal-octet 0 buffer))

(defun marshal-osequence (s buffer)
  (marshal-ulong (length s) buffer)
  (if (stringp s)
      (doseq (c s) (marshal-octet (char-code c) buffer))
      (doseq (c s) (marshal-octet c buffer))))

(defun marshal-sequence (s el-cdr buffer)
  (marshal-ulong (length s) buffer)
  (doseq (e s) (funcall el-cdr e buffer)))

(defun marshal-make-encapsulation (closure)
  (let ((buffer (get-work-buffer)))
   (marshal-octet 1 buffer)			;byte order
   (funcall closure buffer)
   (buffer-contents buffer)))

(defun marshal-add-encapsulation (closure buffer)
  (declare (optimize speed))
  (marshal-align 4 buffer)
  (let* ((octets (buffer-octets buffer))
         (len-pos (fill-pointer octets))
         (old-start-pos (buffer-start-pos buffer)))
    (cond ((< (array-total-size octets)
             (+ len-pos 50))
           (adjust-array octets (+ len-pos 200)
                         :fill-pointer (+ len-pos 4)))
          (t
           (incf (fill-pointer octets) 4)))
    (setf (buffer-start-pos buffer) (fill-pointer octets))
    (marshal-octet 1 buffer)            ;byte order
    (funcall closure buffer)
    (let ((pos (fill-pointer octets)))
      (setf (fill-pointer octets) len-pos)
      (marshal-ulong (- pos (buffer-start-pos buffer)) buffer)
      (setf (fill-pointer octets) pos))
    (setf (buffer-start-pos buffer) old-start-pos)))


(defvar *marshal-typecode-record* nil)

(defun marshal-typecode (tc buffer)
  (let ((recursive-typecode-pos
         (loop for (rtc octets pos) on *marshal-typecode-record* by #'cdddr
             thereis (and (eq tc rtc)
                          (eq (buffer-octets buffer) octets)
                          pos))))
    (cond
     (recursive-typecode-pos
      (marshal-ulong #16rFFFFFFFF buffer)
      (marshal-long (- recursive-typecode-pos
                       (fill-pointer (buffer-octets buffer)))
                    buffer))
     (t
      (let ((*marshal-typecode-record*
             (list* tc (buffer-octets buffer)
                    (fill-pointer (buffer-octets buffer))
                    *marshal-typecode-record*))
            (kind (typecode-kind tc))
            (params (typecode-params tc)))
        (marshal-ulong (position kind TCKind) buffer)
        (let ((pspec (get kind 'tk-params)))
          (cond ((null pspec))
                ((eq 'complex (car pspec))
                 (marshal-add-encapsulation
                  (lambda (buffer)
                    (marshal-multiple params (cdr pspec) buffer)
                    (when (eq kind :tk_union)
                      (let ((label-tc (third params)))
                        (marshal-sequence (fifth (typecode-params tc))
                                          (lambda (el buffer)
                                            (marshal (first el) label-tc buffer)
                                            (marshal-string (second el) buffer)
                                            (marshal-typecode (third el) buffer))
                                          buffer))))
                  buffer))
                (t
                 (marshal-multiple params pspec buffer)))))))))



(defun marshal-tagged-component (component buffer)
  (marshal-ulong (car component) buffer)
  (marshal-osequence (cdr component) buffer))


(defparameter *nil-objref*
  (make-instance 'CORBA:Proxy :id "" :raw-profiles '() :key nil))

(defun marshal-ior (objref buffer)
  (cond ((null objref) (setq objref *nil-objref*))
        ((not (typep objref 'CORBA:Proxy))
         ;; Implicit activation is implemented by this
         (setq objref (op:_this objref))))
  (unless (object-raw-profiles objref)
    (setf (object-raw-profiles objref)
          (map 'list
               (lambda (p)
                 (cons iop:tag_internet_iop
                       (marshal-make-encapsulation
                        (lambda (buffer)
                          (let ((version (iiop-profile-version p)))
                            (marshal-octet (car version) buffer)
                            (marshal-octet (cdr version) buffer))
                          (marshal-string (iiop-profile-host p) buffer)
                          (marshal-ushort (iiop-profile-port p) buffer)
                          (marshal-osequence (iiop-profile-key p) buffer)))))
               (object-profiles objref))))
  (marshal-string (object-id objref) buffer)
  (marshal-sequence (object-raw-profiles objref) #'marshal-tagged-component buffer))



(defun marshal-union (union params buffer)
  (let* ((discriminant (union-discriminator union))
         (value (union-value union))
         (discriminant-type (third params))
         (default-used (fourth params))
         (members (fifth params))
         (member (find discriminant members :key #'car)))
    (when (and (null member)
               (>= default-used 0))
      (setq member (aref members default-used)))
    (assert (not (null member)))  ; FIXME: raise MARSHAL ?
    (marshal discriminant discriminant-type buffer)
    (marshal value (third member) buffer)))

#+unused-defuns
(defun construct-typecode-for-value (value)
  (etypecase value
    ;;((integer -32768 32766) ':tk_short)
    ((integer 0 65535) ':tk_ushort)
    ((signed-byte 32) ':tk_long)
    ((unsigned-byte 32) ':tk_ulong)
    ((signed-byte 64) ':tk_longlong)
    ((unsigned-byte 64) ':tk_ulonglong)
    (number    ':tk_float)
    (string    ':tk_string)
    (sequence
     (let ((elem (construct-typecode-for-value (elt value 0))))
       (make-typecode :tk_sequence elem 0)))
    (struct (typecode value))))

(defun marshal-any (arg buffer)
  (let ((tc (any-typecode arg)))
    (marshal tc :tk_typecode buffer)
    (marshal (any-value arg) tc buffer)))

(defun marshal-enum (arg enum-tc buffer)
  (check-type arg (or symbol integer)
    "a CORBA enum (ingeger or keyword)")
  (let ((symbols (tcp-member-symbols (typecode-params enum-tc))))
    (marshal-ulong 
     (if (integerp arg)
         arg
       (or (position arg symbols)
           (error 'type-error 
                  :datum arg 
                  :expected-type (concatenate 'list '(member) symbols))))
     buffer)))

(defun marshal (arg type buffer)
  (multiple-value-bind (kind params) 
      (type-expand type)
    (case kind
      ((:tk_any) (marshal-any arg buffer))
      ((:tk_octet) (marshal-octet arg buffer))
      ((:tk_char) (marshal-octet (char-code arg) buffer))
      ((:tk_boolean bool) (marshal-bool arg buffer))
      ((:tk_ushort :tk_short) (marshal-ushort arg buffer))
      ((:tk_ulong ulong) (marshal-ulong arg buffer))
      ((:tk_long) (marshal-long arg buffer))
      ((:tk_enum) (marshal-enum arg type buffer))
      ((:tk_longlong :tk_ulonglong) (marshal-number arg 8 buffer))
      ((:tk_float) (marshal-ulong (float-as-ieee-integer (float arg 1.0s0)
                                                         31 23 127)
                                  buffer))
      ((:tk_double) (marshal-number (float-as-ieee-integer (float arg 1.0d0)
                                                           63 52 1023)
                                    8 buffer))
      ((:tk_longdouble) (marshal-number (float-as-ieee-integer (float arg)
                                                               127 112 16383)
                16 buffer))
      ((:tk_string string) (marshal-string arg buffer))
      ((osequence) (marshal-osequence arg buffer))
      ((:tk_objref object) (marshal-ior arg buffer))
      ((:tk_alias) (marshal arg (third params) buffer))
      ((:tk_typecode) (marshal-typecode arg buffer))
      ((:tk_null))
      ((sequence :tk_sequence)
       (let ((el_type (first params)))
         (marshal-sequence 
          arg 
          (lambda (arg buffer) (marshal arg el_type buffer)) 
          buffer)))
      ((:tk_array)
       (let ((tc (first params))
             (len (second params)))
         (unless (= len (length arg))
           (error 'CORBA:MARSHAL))
         (doseq (el arg) (marshal el tc buffer))))
      ((:tk_struct)
       (unless (typep arg 'CORBA:struct)
         (error 'CORBA:MARSHAL))
       (doseq (el (tcp-members params))
	      (marshal (struct-get arg (first el)) (second el) buffer)))
      ((:tk_union)
       (marshal-union arg params buffer))
      ((anon-struct)
       (marshal-multiple arg params buffer))
      (t
       (marshal arg 
                (or (get kind 'corba-typecode)
                    (error "MARSHAL: ~S" kind)) 
                buffer)))))

(defun marshal-multiple (values types buffer)
  (loop for val in values
      for tc in types
      collect (marshal val tc buffer)))


;;;; GIOP extras

(defun marshal-giop-header (type buffer)
  (loop for c across "GIOP"
	do (marshal-octet (char-code c) buffer))
  (marshal-octet 1 buffer)				;Version 
  (marshal-octet 0 buffer)
  (marshal-octet 1 buffer)				;byte-order
  (marshal-octet (cond ((numberp type) type)
		   ((eq type 'request) 0)
		   ((eq type 'reply) 1)
		   (t (error "Message type ~S" type))) buffer)
  ;; Place for message length to be patched in later
  (marshal-ulong 0 buffer))

(defun marshal-giop-set-message-length (buffer)
  (let ((len (fill-pointer (buffer-octets buffer))))
    (setf (fill-pointer (buffer-octets buffer)) 8)
    (marshal-ulong (- len 12) buffer)
    (setf (fill-pointer (buffer-octets buffer)) len)))


(defun marshal-service-context (ctx buffer)
  (marshal-sequence ctx #'marshal-tagged-component buffer))
