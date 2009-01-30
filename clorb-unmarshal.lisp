;;;; clorb-unmarshal.lisp -- Unmarshalling primitives

(in-package :clorb)



;;;; Basic macros


(defmacro %with-in-buffer ((buffer) &body body)
  `(progn
     (let ((buffer ,buffer))
       (declare (type buffer buffer))
       (let ((octets (buffer-octets buffer)))
         (declare (type octets octets))
         (symbol-macrolet ((pos (the buffer-index (buffer-in-pos buffer))))
           (macrolet
             ((get-octet ()
                `(the CORBA:Octet
                   (prog1 (aref octets pos)
                     (setf pos (the buffer-index (1+ pos)))))))
             (progn ,@body)))))))


(defmacro with-sub-buffer ((buffer length) &body body)
  (let ((old-start '#:old-start)
        (old-pos   '#:old-pos)
        (old-byteorder '#:old-byteorder)
        (buffervar '#:buffervar)
        (lengthvar '#:lengthvar))
    `(let* ((,buffervar ,buffer)
            (,lengthvar ,length)
            (,old-start (buffer-start-pos ,buffervar))
            (,old-pos   (buffer-in-pos ,buffervar))
            (,old-byteorder (buffer-byte-order ,buffervar)))
       (unwind-protect
           (progn
             (setf (buffer-start-pos ,buffervar) (buffer-in-pos ,buffervar))
             ,@body)
         (setf (buffer-start-pos ,buffervar) ,old-start)
         (setf (buffer-in-pos ,buffervar) (+ ,old-pos ,lengthvar))
         (setf (buffer-byte-order ,buffervar) ,old-byteorder)))))


(defmacro %%unmarshal-number (size signed)
  ;; to be used withing a %with-in-buffer
  (let ((code1 `(get-octet))
        (code2 nil)
        (code nil))
    (loop for c from 1 below size 
          do (setf code1 `(logior ,code1 (ash (get-octet) ,(* c 8)))))
    (setf code2 `(logior
                  ,@(loop for c from (1- size) downto 1 
                          collect `(progn (ash (get-octet) ,(* c 8))))
                  (get-octet)))
    (setf code `(if (= (buffer-byte-order buffer) 1)
                  ,code1 ,code2))
    (when signed
      (setf code `(let ((n ,code))
                    (if (>= n ,(expt 2 (1- (* size 8))))
                      (- n ,(expt 2 (* size 8)))
                      n))))
    code))



;;;; Reading from Buffer


(defun %long (buffer)
  (declare (optimize (speed 3) (space 1)))
  (%with-in-buffer (buffer)
    (the CORBA:Long (%%unmarshal-number 4 t))))

(defun %ulong (buffer)
  (declare (optimize (speed 3) (space 1)))
  (%with-in-buffer (buffer)
    (the CORBA:ULong (%%unmarshal-number 4 t))))


(defun begin-in-buffer (buffer n)
  ;; Start reading a primitive CDR encoded value
  ;; - align to n if needed
  ;; - start chunk
  (symbol-macrolet ((pos (buffer-in-pos buffer)))
    (let ((apos (+ pos (align-amount (buffer-rel-pos buffer) n))))
      (cond ((and chunking-p (or (null *chunk-end*) (>= apos *chunk-end*)))
             ;; Need to start new chunk
             ;; chunk break in encapsulation seems unclear
             (assert (zerop (buffer-start-pos buffer))) 
             (let ((chunk-pos (if *chunk-end*
                                  (align-pos *chunk-end* 4)
                                  (align-pos pos 4))))
               (setf pos chunk-pos)
               (setf *chunk-end* nil)
               (let ((chunk-size (%long buffer)))
                 (cond ((< 0 chunk-size #x7FFFFF00) ; limits for chunk size
                        (setf *chunk-end* (+ pos chunk-size))
                        (setf pos (align-pos pos n)))
                       (t               ; not a chunk start, unmarshal normally
                        (setf pos apos))))))
            (t                          ; normal
             (setf pos apos))))))


(defmacro with-in-buffer ((buffer &key (check nil) (align nil))
                            &body body)
  `(progn
     ,@(if check `((check-type ,buffer buffer "a buffer")))
     (%with-in-buffer (,buffer)
       ,@(if align `((begin-in-buffer buffer ,align)))
       ,@body)))




;;;; Unmarshal number macro


(defmacro %unmarshal-number (size signed buffer &optional (align size))
  ;; The spec seems to say that primitives are never broken by chunks ...
  ;; (15.3.4)
  `(with-in-buffer (,buffer :check nil :align ,align)
     (%%unmarshal-number ,size ,signed)))



;;;; Primitive Types


(defun unmarshal-void (buffer)
  (declare (ignore buffer))
  nil)

(declaim (ftype (function (buffer) CORBA:octet) unmarshal-octet))
(defun unmarshal-octet (buffer)
  (declare (type buffer buffer)
           (optimize speed (debug 0)))
  (with-in-buffer (buffer :check nil :align 0)
    (get-octet)))

(defun unmarshal-char (buffer)
  (code-char (unmarshal-octet buffer)))

(defun unmarshal-bool (buffer)
  (/= (unmarshal-octet buffer) 0))

(defun unmarshal-ushort (buffer)
  (declare (optimize speed))
  (the CORBA:ushort
    (%unmarshal-number 2 nil buffer)))

(defun unmarshal-short (buffer)
  (declare (optimize speed))
  (the CORBA:Short
    (%unmarshal-number 2 t buffer)))

(defun unmarshal-ulong (buffer)
  (declare (optimize speed)
           (type buffer buffer))
  (the CORBA:ULong
    (%unmarshal-number 4 nil buffer)))

(defun unmarshal-long (buffer)
  (%unmarshal-number 4 t buffer))

(defun unmarshal-ulonglong (buffer) 
  (%unmarshal-number 8 nil buffer))

(defun unmarshal-longlong (buffer) 
  (%unmarshal-number 8 t buffer))

(defun unmarshal-ulonglong (buffer)
  (%unmarshal-number 8 nil buffer))


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
  (ieee-integer-to-float (%unmarshal-number 8 nil buffer) 
                         (coerce 0 'corba:double)
                         63 11 52 1023))

(defun unmarshal-longdouble (buffer)
  (ieee-integer-to-float (%unmarshal-number 16 nil buffer 8) 
                         (coerce 0 'corba:longdouble)
                         127 15 112 16383))


(defun unmarshal-fixed (multiplier buffer)
  (let ((n 0))
    (macrolet ((accumulate (digit) `(setf n (+ (* n 10) ,digit))))
      (with-in-buffer (buffer :align 0)
        (loop
          (let ((octet (get-octet)))
            (accumulate (ash octet -4))
            (let ((digit (logand octet #xF)))
              (if (< digit 10) 
                (accumulate digit)
                (progn (when (= digit #xD) (setf n (- n)))
                       (return))))))))
    (/ n multiplier)))


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
  (with-in-buffer (buffer :check t :align 4)
    (let* ((len (%ulong buffer))
           (str (make-string (- len 1))))
      (loop for i from 0 below (1- len)
            do (setf (aref str i) (code-char (get-octet))))
      (get-octet)
      str)))


(defun unmarshal-osequence (buffer)
  (declare (type buffer buffer)
           (optimize speed))
  (with-in-buffer (buffer :check nil :align 4)
    (let ((len (%ulong buffer)))
      (subseq octets pos 
              (setf pos (the buffer-index (+ pos len)))))))


(defmacro with-encapsulation (buffer &body body)
  `(with-sub-buffer (,buffer (unmarshal-ulong ,buffer))
     (setf (buffer-byte-order ,buffer) (unmarshal-octet ,buffer))
     ,@body))

(defun unmarshal-encapsulation (encaps thunk)
  (if (typep encaps 'buffer)
      (with-encapsulation encaps
        (funcall thunk encaps))
    (let ((buffer (make-buffer :octets encaps)))
      (setf (buffer-byte-order buffer) (unmarshal-octet buffer))
      (funcall thunk buffer))))


(defvar *indirection-record* nil)

(defun unmarshal-typecode (buffer)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((kind-index (unmarshal-ulong buffer))
         (start (- (buffer-in-pos buffer) 4))
	 (kind-symbol (if (= kind-index #xFFFFFFFF)
                          :indirection
                          (svref TCKind kind-index))))
    (if (eq kind-symbol :indirection)
        (let ((abs-pos (buffer-abs-pos buffer))
              (indirection (unmarshal-long buffer)))
          (incf abs-pos indirection)
          (mess 1 "Indirection: ~S abs-pos: ~S" indirection abs-pos)
          (let ((obj (cdr (assoc abs-pos (car *indirection-record*)))))
            (cond (obj obj)
                  ((or (< abs-pos (buffer-start-pos buffer))
                       (>= abs-pos (buffer-length buffer)))
                   (break "wrong indirection ~d" abs-pos)
                   (raise-system-exception 'CORBA:MARSHAL))
                  (t (let ((old-pos (buffer-in-pos buffer)))
                       (break "abs-pos=~d" abs-pos)
                       (setf (buffer-in-pos buffer) abs-pos)
                       (prog1 (unmarshal-typecode buffer)
                         (setf (buffer-in-pos buffer) old-pos)))))))
        (let ((*indirection-record* (or *indirection-record* (list nil))))
          (let ((typecode (make-typecode kind-symbol)))
            (push (cons start typecode) (car *indirection-record*))
            (setf (typecode-params typecode)
                  (unmarshal-spec (get kind-symbol 'tk-params) buffer))
            typecode)))))

(defun unmarshal-spec (pspec buffer &optional top-level)
  (declare (optimize (debug 3) (speed 0) (space 0)))
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


(defun unmarshal-tagged-component (buffer)
  (cons (unmarshal-ulong buffer)
	(unmarshal-osequence buffer)))


(defgeneric decode-ior-profile (tag encaps))
(defgeneric create-objref (orb &key &allow-other-keys))

(defun unmarshal-object (buffer &optional expected-id)
  (let* ((type-id (unmarshal-string buffer))
         (n-profiles (unmarshal-ulong buffer)))
    (let ((raw-profiles 
           (loop repeat n-profiles
                 for tag = (unmarshal-ulong buffer)
                 for encaps = (unmarshal-osequence buffer)
                 collect (IOP:TaggedProfile
                          :tag tag :profile_data encaps))))
      (unless (zerop n-profiles)
        (create-objref (the-orb buffer)
                       :ior-id type-id :expected-id expected-id
                       :raw-profiles raw-profiles)))))


(defmethod decode-ior-profile ((tag (eql 1)) encaps)
  ;; A multi component profile
  (unmarshal-encapsulation encaps
                           (lambda (buffer)
                             (unmarshal-sequence #'unmarshal-tagged-component buffer))))



;;;; GIOP Support


(defun unmarshal-giop-header-raw (buffer)
  ;; Returns: msgtype# fragmentedp version-major version-minor
  (with-in-buffer (buffer :align nil)
    (if (loop for c in '#.(mapcar #'char-code '(#\G #\I #\O #\P))
              always (eql c (get-octet)))
      (let ((major (get-octet))
            (minor (get-octet))
            (flags (get-octet))
            (msgtype (get-octet)))
        (setf (buffer-byte-order buffer) (logand flags 1))
        (values msgtype
                (if (> minor 0)
                    (logbitp 1 flags))
                major minor))
      (progn (warn "Not a GIOP message: ~/net.cddr.clorb::stroid/..." 
                   (subseq octets 0 (min 10 (length octets))))
             (values :unknown)))))



;;; clorb-unmarshal.lisp ends here
