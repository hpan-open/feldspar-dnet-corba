;;;; UnMarshal

(in-package :clorb)


(declaim (ftype (function (buffer) CORBA:octet) unmarshal-octet))
(defun unmarshal-octet (buffer)
  (declare (type buffer buffer)
           (optimize speed (debug 0)))
  (let ((pos (buffer-position buffer)))
    (setf (buffer-position buffer) (1+ pos))
    (aref (buffer-octets buffer) pos)))

(defun unmarshal-char (buffer)
  (code-char (unmarshal-octet buffer)))

(defun unmarshal-bool (buffer)
  (/= (unmarshal-octet buffer) 0))

(defun unmarshal-align (n buffer)
  (declare (type (integer 0 100) n)
           (type buffer buffer)
           (optimize speed))
  (incf (buffer-position buffer)
        (logand (- n (logand (the buffer-index (buffer-rel-pos buffer)) (- n 1))) (- n 1))))


(defmacro with-encapsulation (buffer &body body)
  `(with-sub-buffer (,buffer (unmarshal-ulong ,buffer))
     (setf (buffer-byte-order ,buffer) (unmarshal-octet ,buffer))
     ,@body))

(defun unmarshal-encapsulation (encaps thunk)
  (if (typep encaps 'buffer)
      (with-encapsulation encaps
        (funcall thunk))
    (let ((buffer (make-buffer :octets encaps)))
      (setf (buffer-byte-order buffer) (unmarshal-octet buffer))
      (funcall thunk buffer))))

(defmacro unmarshal-number (size signed buffer &optional (align size))
  (let ((code1 `(the corba:octet (unmarshal-octet buffer)))
        (code2 nil)
        (code nil))
    (loop for c from 1 below size 
          do (setf code1
                   `(logior ,code1
                            (ash (the corba:octet (unmarshal-octet buffer))
                                 ,(* c 8)))))
    (setf code2 `(logior
                  ,@(loop for c from (1- size) downto 1 
                          collect
                          `(ash (the corba:octet (unmarshal-octet buffer)) ,(* c 8)))
                  (the corba:octet (unmarshal-octet buffer))))
    
    (setf code `(if (= (buffer-byte-order buffer) 1)
                  ,code1 ,code2))
    (when signed
      (setf code `(let ((n ,code))
                    (if (>= n ,(expt 2 (1- (* size 8))))
                      (- n ,(expt 2 (* size 8)))
                      n))))
    `(let ((buffer ,buffer))
       (unmarshal-align ,align buffer)
       ,code)))

(defun unmarshal-ushort (buffer)
  (declare (optimize speed))
  (the CORBA:short
    (unmarshal-number 2 nil buffer)))

(defun unmarshal-short (buffer)
  (declare (optimize speed))
  (the CORBA:Short
    (unmarshal-number 2 t buffer)))

(defun unmarshal-ulong (buffer)
  (declare (optimize speed))
  (the CORBA:ULong
    (unmarshal-number 4 nil buffer)))

(defun unmarshal-long (buffer)
  (unmarshal-number 4 t buffer))

(defun unmarshal-longlong (buffer) 
  (unmarshal-number 8 t buffer))

(defun unmarshal-ulonglong (buffer)
  (unmarshal-number 8 nil buffer))


(defun ieee-integer-to-float (raw float-type-zero sign-bit expn-bits fraction-bits bias)
  (if (zerop raw)
    float-type-zero
    (let ((fraction (+ (ldb (byte fraction-bits 0) raw)
                       (ash 1 fraction-bits)))
          (exponent (ldb (byte expn-bits fraction-bits) raw))
          (sign  (logbitp sign-bit raw)))
      (* (if sign -1 1)
         (scale-float (float fraction float-type-zero)
                      (+ exponent 
                         (- bias)
                         (- fraction-bits)))))))

(defun unmarshal-float (buffer)
  (ieee-integer-to-float (unmarshal-ulong buffer) 
                         (coerce 0 'corba:float)
                         31 8 23 127))

(defun unmarshal-double (buffer)
  (ieee-integer-to-float (unmarshal-number 8 nil buffer) 
                         (coerce 0 'corba:double)
                         63 11 52 1023))

(defun unmarshal-longdouble (buffer)
  (ieee-integer-to-float (unmarshal-number 16 nil buffer 8) 
                         (coerce 0 'corba:longdouble)
                         127 15 112 16383))




(defmacro unmarshal-sequence-m ((buffer &key (el-type t)) &body el-read)
  (let ((len (gensym))
        (r (gensym))
        (i (gensym)))
    `(loop with buffer = ,buffer
           with ,len = (unmarshal-ulong buffer) 
           with ,r = (make-array ,len :element-type ,el-type)
           for ,i from 0 below ,len
           do (setf (aref ,r ,i) (progn ,@el-read))
           finally (return ,r))))

(defun unmarshal-sequence (el-reader buffer &optional (el-type t))
  (unmarshal-sequence-m (buffer :el-type el-type)
                        (funcall el-reader buffer)))

(defun unmarshal-string (buffer)
  (declare (optimize (speed 3) (safety 1) (space 0))
           (type buffer buffer))
  (let* ((len (unmarshal-ulong buffer))
         (str (make-string (- len 1)))
         (octets (buffer-octets buffer)))
    (loop for pos from (buffer-position buffer)
          for i from 0 below (1- len)
          do (setf (aref str i) (code-char (aref octets pos))))
    (incf (buffer-position buffer) len)
    str))


(defun unmarshal-osequence (buffer)
  (let ((len (unmarshal-ulong buffer)))
    (subseq (buffer-octets buffer)
	    (buffer-position buffer)
	    (incf (buffer-position buffer) len))))

(defvar *indirection-record* nil)

(defun unmarshal-typecode (buffer)
  (let* ((start (buffer-abs-pos buffer))
         (tki (unmarshal-ulong buffer))
	 (tk (if (= tki #16rFFFFFFFF)
                 :indirection
               (svref TCKind tki))))
    (if (eq tk :indirection)
        (let ((abs-pos (buffer-abs-pos buffer))
              (indirection (unmarshal-long buffer)))
          (incf abs-pos indirection)
          (mess 1 "Indirection: ~S abs-pos: ~S" indirection abs-pos)
          (or
           (loop for (pos tc) on *indirection-record* by #'cddr
               do (mess 1 "Searching ~S" pos)
               when (= pos abs-pos)
               return tc)
           (error 'CORBA:MARSHAL)))
      (let* ((typecode (make-typecode tk))
             (*indirection-record*
              (list* start typecode *indirection-record*)))
        (setf (typecode-params typecode)
              (unmarshal-spec (get tk 'tk-params) buffer))
        
        typecode))))

(defun unmarshal-spec (pspec buffer &optional top-level)
  (cond ((null pspec) nil)
        ((consp pspec)
         (case (car pspec)
           (complex
            (with-encapsulation buffer
              (unmarshal-spec (cdr pspec) buffer top-level)))
           (sequence
            (unmarshal-sequence-m (buffer)
              (unmarshal-spec (second pspec) buffer top-level)))
           (otherwise 
            ;; normal parameter record, declared special to allow
            ;; reference from unmarshaling sub-parts of the record
            (let ((record (cons nil nil)))
              (flet ((add (x)
                       (let ((new (cons x nil)))
                         (cond ((car record)
                                (setf (cdr (car record)) new
                                      (car record) new))
                               (t
                                (setf (car record) new
                                      (cdr record) new))))))
                
                (dolist (s pspec)
                  (add (unmarshal-spec s buffer (or top-level record))))
                (cdr record))))))
        ((numberp pspec)
         ;; reference to enclosing record
         (unmarshal (elt (cdr top-level) pspec) buffer))
        (t
         (ecase pspec
           (:tk_string (unmarshal-string buffer))
           (:tk_long   (unmarshal-long buffer))
           (:tk_ulong  (unmarshal-ulong buffer))
           (:tk_short  (unmarshal-short buffer))
           (:tk_ushort (unmarshal-ushort buffer))
           (:tk_typecode (unmarshal-typecode buffer))))))


(defun unmarshal-any (buffer)
  (let ((tc (unmarshal-typecode buffer)))
    (if *explicit-any*
        (corba:any :any-typecode tc :any-value (unmarshal tc buffer))
      (unmarshal tc buffer))))



(defun unmarshal-tagged-component (buffer)
  (cons (unmarshal-ulong buffer)
	(unmarshal-osequence buffer)))

(defun unmarshal-iiop-profile-body (buffer)
  (make-iiop-profile
   :version (cons (unmarshal-octet buffer) 
                  (unmarshal-octet buffer))
   :host (unmarshal-string buffer)
   :port (unmarshal-ushort buffer)
   :key (unmarshal-osequence buffer)))


(defgeneric decode-ior-profile (tag encaps))

(defun unmarshal-object (buffer &optional expected-id)
  (let* ((type-id (unmarshal-string buffer))
         (n-profiles (unmarshal-ulong buffer))
         (profiles '()))
    (let ((raw-profiles 
           (loop repeat n-profiles
                 for tag = (unmarshal-ulong buffer)
                 for encaps = (unmarshal-osequence buffer)
                 for profile = (decode-ior-profile tag encaps)
                 collect (cons tag encaps)
                 when profile do (push profile profiles))))
      (unless (zerop n-profiles)
        (setq profiles (nreverse profiles))
        (unless profiles
          (error 'omg.org/corba:inv_objref))
        (create-objref :ior-id type-id :expected-id expected-id
                       :profiles profiles :raw-profiles raw-profiles)))))

(defun create-objref (&key ior-id expected-id raw-profiles profiles)
  (make-instance (find-proxy-class 
                  (if (equal ior-id "") expected-id ior-id))
    :id ior-id
    :profiles profiles
    :raw-profiles raw-profiles ))

(defmethod decode-ior-profile ((tag (eql 0)) encaps)
  (unmarshal-encapsulation encaps #'unmarshal-iiop-profile-body))



;;; Enum

(defun unmarshal-enum (tc buffer)
  (let ((index (unmarshal-ulong buffer)))
    (elt (tc-keywords tc) index)))



;;; Main entry

(defgeneric unmarshal (type buffer))

(defmethod unmarshal ((type CORBA:TypeCode) buffer)
  (declare (optimize (speed 2)))
  (let ((kind (typecode-kind type))
        (params (typecode-params type)))
    (ecase kind
      ((:tk_octet) (unmarshal-octet buffer))
      ((:tk_char) (unmarshal-char buffer))
      ((:tk_boolean) (unmarshal-bool buffer))
      ((:tk_ushort) (unmarshal-ushort buffer))
      ((:tk_short) (unmarshal-short buffer))
      ((:tk_ulong) (unmarshal-ulong buffer))
      ((:tk_enum) (unmarshal-enum type buffer))
      ((:tk_long) (unmarshal-long buffer))
      ((:tk_longlong) (unmarshal-longlong buffer))
      ((:tk_ulonglong) (unmarshal-ulonglong buffer))
      ((:tk_float) (unmarshal-float buffer))
      ((:tk_double) (unmarshal-double buffer))
      ((:tk_longdouble) (unmarshal-longdouble buffer))
      ((:tk_string) (unmarshal-string buffer))
      ((:tk_any) (unmarshal-any buffer))
      ((:tk_sequence)
       (let ((eltype (car params)))
	 (if (or (eq eltype :tk_octet)
		 (and (consp eltype)
		      (eq (car eltype) :tk_octet)))
           (unmarshal-osequence buffer)
	   (unmarshal-sequence-m (buffer) (unmarshal eltype buffer)))))
      ((:tk_array)
       (let ((tc (first params))
             (len (second params)))
         (let ((arr (make-array len)))
           (loop for i below len 
                 do (setf (aref arr i) (unmarshal tc buffer)))
           arr)))
      ((:tk_alias) (unmarshal (third params) buffer))
      ((:tk_null :tk_void) nil)
      ((:tk_objref) (unmarshal-object buffer (first params)))
      ((:tk_typecode) (unmarshal-typecode buffer)))))


(defun unmarshal-multiple (typecodes buffer)
  (declare (optimize speed))
  (mapcar #'(lambda (tc) (unmarshal tc buffer))
          typecodes))


;;;; GIOP extras

(defun unmarshal-service-context (buffer)
  (unmarshal-sequence-m (buffer) 
    (IOP:ServiceContext :context_id (unmarshal-ulong buffer)
	                :context_data (unmarshal-osequence buffer))))
