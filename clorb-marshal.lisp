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
(defun marshal-number (n size buffer &optional (align size))
  (declare (fixnum size)
           (integer n)
           (optimize (speed 3) (debug 0) (safety 1)))
  (marshal-align align buffer)
  (loop for p from 0 by 8
	for c fixnum below size
	do (marshal-octet (ldb (byte 8 p) n) buffer)))

;;
(#-clisp define-compiler-macro #+clisp defmacro
         marshal-number (&whole form n size buffer &optional (align size))
  (if (numberp size)
      (let ((nvar '#:nvar)
            (nnvar '#:nnvar)
            (bvar '#:bvar))
        `(let* (,@(if (> size 3) 
                      `((,nnvar ,n) 
                        (,nvar (logand ,nnvar #16rffffff)))
                      `((,nvar ,n)))
                (,bvar ,buffer))
          ,@(if (> size 3)
                `((declare (type (integer 0 #16rFFFFFF) ,nvar))))
          (marshal-align ,align ,bvar)
          (marshal-octet (logand #16rff ,nvar) ,bvar)
          ,@(loop for p from 8 by 8
                  for c from 1 below size
                  collect `(marshal-octet (ldb (byte 8 ,p) 
                                           ,(if (>= c 3) nnvar nvar)) 
                            ,bvar))))
      form))

(defun marshal-short (n buffer)
  (declare (type (or CORBA:ushort CORBA:short) n)
           (optimize speed))
  (marshal-number n 2 buffer))

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
          (mess 1 "Shift=~D" shift))
        (logior (ash (if (< sign 0) 1 0) sign-bit)
                (ash (+ expn (- shift) fraction-bits bias) fraction-bits)
                (- (ash frac shift) (ash 1 fraction-bits)))))))


(defun marshal-float (arg buffer)
  (marshal-ulong (float-as-ieee-integer (coerce arg 'corba:float)
                                        31 23 127)
                 buffer))

(defun marshal-double (arg buffer)
  (marshal-number (float-as-ieee-integer (coerce arg 'corba:double)
                                         63 52 1023)
                  8 buffer))

(defun marshal-longdouble (arg buffer)
  (marshal-number (float-as-ieee-integer (coerce arg 'corba:longdouble)
                                         127 112 16383)
                  16 buffer 8))


;; From Paul Foley

(defun single-float-bits (float)
  (declare (type single-float float))
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (unless (= (float-radix float) 2)
      (setq exponent (* exponent (floor (log (float-radix float) 2)))))
    (when (and (<= 0.5f0 significand) (< significand 1.0f0))
      (setq significand (* significand 2.0f0)
            exponent (1- exponent)))
    (unless (and (= significand 0f0) (= exponent 0))
      (decf significand 1.0) (incf exponent 127))
    (logior (if (> sign 0f0) 0 (ash -1 31))
            (ash exponent 23)
            (truncate (* significand 8388608f0)))))

(defun double-float-high-bits (float)
  (declare (type double-float float))
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (unless (= (float-radix float) 2)
      (setq exponent (* exponent (floor (log (float-radix float) 2)))))
    (when (and (<= 0.5f0 significand) (< significand 1.0f0))
      (setq significand (* significand 2.0f0)
            exponent (1- exponent)))
    (unless (and (= significand 0f0) (= exponent 0))
      (decf significand 1.0) (incf exponent 1023))
    (logior (if (> sign 0d0) 0 (ash -1 31))
            (ash exponent 20)
            (truncate (* significand 1048576d0)))))

(defun double-float-low-bits (float)
  (declare (type double-float float))
  (multiple-value-bind (significand exponent) (decode-float float)
    (unless (= (float-radix float) 2)
      (setq exponent (* exponent (floor (log (float-radix float) 2)))))
    (when (and (<= 0.5f0 significand) (< significand 1.0f0))
      (setq significand (* significand 2.0f0)
            exponent (1- exponent)))
    (unless (and (= significand 0f0) (= exponent 0))
      (decf significand 1.0) (incf exponent 1023))
    (values (truncate (* (nth-value 1 (truncate (* significand 1048576d0)))
                         4294967296d0)))))


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

(defvar *typecode-params*)

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
            (*typecode-params* (typecode-params tc)))
        (marshal-ulong (position kind TCKind) buffer)
        (marshal-spec *typecode-params* (get kind 'tk-params) buffer))))))


(defun marshal-spec (params pspec buffer)
  (cond ((null pspec))
        ((numberp pspec)
         (marshal params (elt *typecode-params* pspec) buffer))
        ((consp pspec)
         (case (car pspec)
           (complex
            (marshal-add-encapsulation
             (lambda (buffer) (marshal-spec params (cdr pspec) buffer))
             buffer))
           (sequence
            (marshal-sequence params 
                              (lambda (val buf)
                                (marshal-spec val (second pspec) buf))
                              buffer))
           (otherwise
            (mapc #'marshal-spec params pspec (repeted buffer)))))
        (t
         (ecase pspec
           (:tk_string (marshal-string params buffer))
           (:tk_long   (marshal-long params buffer))
           (:tk_ulong  (marshal-ulong params buffer))
           (:tk_short  (marshal-short params buffer))
           (:tk_ushort (marshal-ushort params buffer))
           (:tk_typecode (marshal-typecode params buffer))))))


(defun marshal-tagged-component (component buffer)
  (marshal-ulong (car component) buffer)
  (marshal-osequence (cdr component) buffer))



;;(defparameter *nil-objref*
;;  (make-instance 'CORBA:Proxy :id "" :raw-profiles '()))

(defmethod marshal-object ((object null) buffer)
  (marshal-string "" buffer)
  (marshal-ulong 0 buffer))

(defmethod marshal-object ((object t) buffer)
  (marshal-object (op:_this object) buffer))



(defun marshal-any (arg buffer)
  (let ((tc (any-typecode arg)))
    (marshal-typecode tc buffer)
    (marshal (any-value arg) tc buffer)))

(defun marshal-enum (arg enum-tc buffer)
  (declare (optimize speed))
  ;;(check-type arg (or symbol integer) "a CORBA enum (ingeger or keyword)")
  (let ((symbols (tc-keywords enum-tc)))
    (marshal-ulong 
     (if (integerp arg)
         arg
       (or (position arg symbols)
           (error 'type-error 
                  :datum arg 
                  :expected-type (concatenate 'list '(member) symbols))))
     buffer))) 




(defgeneric marshal (arg tc buffer))

(defmethod marshal (arg (type CORBA:TypeCode) buffer)
  (let ((kind (typecode-kind type))
        (params (typecode-params type)))
    (ecase kind
      ((:tk_any) (marshal-any arg buffer))
      ((:tk_octet) (marshal-octet arg buffer))
      ((:tk_char) (marshal-octet (char-code arg) buffer))
      ((:tk_boolean) (marshal-bool arg buffer))
      ((:tk_ushort :tk_short) (marshal-ushort arg buffer))
      ((:tk_ulong) (marshal-ulong arg buffer))
      ((:tk_long) (marshal-long arg buffer))
      ((:tk_enum) (marshal-enum arg type buffer))
      ((:tk_longlong :tk_ulonglong) (marshal-number arg 8 buffer))
      ((:tk_float) (marshal-float arg buffer))
      ((:tk_double) (marshal-double arg buffer))
      ((:tk_longdouble) (marshal-longdouble arg buffer))
      ((:tk_string) (marshal-string arg buffer))
      ((:tk_objref object) (marshal-object arg buffer))
      ((:tk_alias) (marshal arg (third params) buffer))
      ((:tk_typecode) (marshal-typecode arg buffer))
      ((:tk_null))
      ((:tk_sequence)
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
         (doseq (el arg) (marshal el tc buffer)))))))

(defun marshal-multiple (values types buffer)
  (loop for val in values
     for tc in types
     do (marshal val tc buffer)))


;;;; GIOP extras

(defun marshal-giop-set-message-length (buffer)
  (let ((len (fill-pointer (buffer-octets buffer))))
    (setf (fill-pointer (buffer-octets buffer)) 8)
    (marshal-ulong (- len 12) buffer)
    (setf (fill-pointer (buffer-octets buffer)) len)))


(defun marshal-service-context (ctx buffer)
  (marshal-sequence ctx #'marshal-tagged-component buffer))
