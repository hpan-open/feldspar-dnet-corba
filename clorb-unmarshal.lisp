;;;; UnMarshal

(in-package :clorb)


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
  (loop while (/= 0 (rem (buffer-rel-pos buffer) n))
	do (incf (buffer-position buffer))))



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

(defmacro unmarshal-number (size signed buffer)
  `(let ((buffer ,buffer))
     (unmarshal-align ,size buffer)
     (if (= (buffer-byte-order buffer) 1)
         (+
          ,@(loop for c below size collect
                  `(* ,(expt 2 (* c 8))
                      ,(if (and signed (= c (- size 1)))
                           '(let ((b (unmarshal-octet buffer)))
                             (if (> b 127) (- b 256) b))
                         '(unmarshal-octet buffer)))))
       (loop with b = (unmarshal-octet buffer)
           with r = (if (and ,signed (> b 127)) -1 0)
           for p from (* (1- ,size) 8) by -8
           do (setq r (dpb b (byte 8 p) r))
           while (> p 0)
           do (setq b (unmarshal-octet buffer))
           finally (return r)))))

(defun unmarshal-ushort (buffer)
  (unmarshal-number 2 nil buffer))

(defun unmarshal-short (buffer)
  (unmarshal-number 2 t buffer))

(defun unmarshal-ulong (buffer)
  (declare (optimize speed))
  (unmarshal-number 4 nil buffer))

(defun unmarshal-long (buffer)
  (unmarshal-number 4 t buffer))

(defmacro unmarshal-sequence-m ((buffer &key (el-type t))
                                &body el-read)
  `(loop with buffer = ,buffer
       with len = (unmarshal-ulong buffer) 
       with r = (make-array len :element-type ,el-type)
       for i from 0 below len
       do (setf (svref r i) (progn ,@el-read))
       finally (return r)))

(defun unmarshal-sequence (el-reader buffer &optional (el-type t))
  (unmarshal-sequence-m (buffer :el-type el-type)
                        (funcall el-reader buffer)))

(defun unmarshal-string (buffer)
  (loop with len = (1- (unmarshal-ulong buffer))
	with r = (make-string len)
	for i from 0 below len
	do (setf (char r i) (unmarshal-char buffer))
	finally (progn
		  (unmarshal-octet buffer)
		  (return r))))


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
               (svref TCKind tki)))
	 (params (get tk 'tk-params)))
    (if (eq tk :indirection)
        (let ((abs-pos (buffer-abs-pos buffer))
              (indirection (unmarshal-long buffer)))
          (incf abs-pos indirection)
          (mess 2 "Indirection: ~S abs-pos: ~S" indirection abs-pos)
          (or
           (loop for (pos tc) on *indirection-record* by #'cddr
               do (mess 1 "Searching ~S" pos)
               when (= pos abs-pos)
               return tc)
           (error 'CORBA:MARSHAL)))
      (let* ((typecode (make-typecode tk))
             (*indirection-record*
              (list* start typecode *indirection-record*)))
        (cond
         ((eq t params)
          (setf (typecode-params typecode)
            (unmarshal-osequence buffer)))
         ((eq 'complex (car params))
          (with-encapsulation buffer
            (let ((vals (unmarshal-multiple (cdr params) buffer)))
              (when (eq tk :tk_union)
                (nconc vals
                       (list (unmarshal-sequence-m
                              (buffer :el-type t)
                              (list (unmarshal (third vals) buffer)
                                    (unmarshal-string buffer)
                                    (unmarshal-typecode buffer))))))
              (setf (typecode-params typecode) vals))))
         (t
          (setf (typecode-params typecode) 
            (unmarshal-multiple params buffer))))
        typecode))))


(defun unmarshal-any (buffer)
  (let ((tc (unmarshal-typecode buffer)))
    (if *explicit-any*
        (corba:any :any-typecode tc :any-value (unmarshal tc buffer))
      (unmarshal tc buffer))))

(defun unmarshal-union (params buffer)
  (let* ((discriminant-type (third params))
         (default-used (fourth params))
         (members      (fifth params))
         (discriminant (unmarshal discriminant-type buffer))
         (index
          (do ((i 0 (1+ i)))
              ((or (>= i (length members))
                   (and (not (eql i default-used))
                        (eql discriminant (first (aref members i)))))
               (if (>= i (length members))
                   default-used
                 i))))
         (tc (third (aref members index))))
    (corba:union :union-discriminator discriminant
                 :union-value (unmarshal tc buffer))))

(defun unmarshal-tagged-component (buffer)
  (cons (unmarshal-ulong buffer)
	(unmarshal-osequence buffer)))

(defun unmarshal-iiop-profile-body (reference buffer)
   (unmarshal-octet buffer)			;Version (ignored for now)
   (unmarshal-octet buffer)
   (setf (object-host reference) (unmarshal-string buffer))
   (setf (object-port reference) (unmarshal-ushort buffer))
   (setf (object-key reference) (unmarshal-osequence buffer)))

(defun unmarshal-ior (buffer &optional expected-id)
  (let* ((type-id (unmarshal-string buffer))
         (n-profiles (unmarshal-ulong buffer))
         reference)
    (when (> n-profiles 0)
      (setq reference (make-instance 
                          (find-proxy-class 
                           (if (equal type-id "")
                               expected-id type-id))
                        :id type-id))
      (setf (object-profiles reference)
        (loop repeat n-profiles
            for tag = (unmarshal-ulong buffer)
            for encaps = (unmarshal-osequence buffer)
            collect (cons tag encaps)
            when (= tag 0)
            do (unmarshal-encapsulation 
                encaps 
                (lambda (buffer) 
                  (unmarshal-iiop-profile-body reference buffer))))))
    reference))


;;; Enum

(defun unmarshal-enum (params buffer)
  (let ((index (unmarshal-ulong buffer)))
    (elt (tcp-member-symbols params) index)))


;;; Main entry

(defun unmarshal (type buffer)
  (multiple-value-bind (kind params) 
      (type-expand type)
    (case kind
      ((octet :tk_octet) (unmarshal-octet buffer))
      ((:tk_char char) (unmarshal-char buffer))
      ((bool :tk_boolean) (unmarshal-bool buffer))
      ((ushort :tk_ushort) (unmarshal-ushort buffer))
      ((short :tk_short) (unmarshal-short buffer))
      ((ulong :tk_ulong) (unmarshal-ulong buffer))
      ((:tk_enum) (unmarshal-enum params buffer))
      ((long :tk_long) (unmarshal-long buffer))
      ((:tk_longlong) (unmarshal-number 8 t buffer))
      ((:tk_ulonglong) (unmarshal-number 8 nil buffer))
      ((string :tk_string) (unmarshal-string buffer))
      ((:tk_any) (unmarshal-any buffer))
      ((:tk_sequence sequence)
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
      ((:tk_alias)
       (unmarshal (third params) buffer))
      ((:tk_null :tk_void) nil)
      ((:tk_struct)
       (make-struct-internal 
        (first params) 
        (map 'list (lambda (nt-pair)
                     (cons (lispy-name (first nt-pair))
                           (unmarshal (second nt-pair) buffer)))
             (third params))))
      ((:tk_except)
       (make-condition 'corba:userexception
	 :id (first params)
	 :values (map 'list
		   (lambda (nt-pair) 
		     (unmarshal (second nt-pair) buffer))
		   (third params))))
      ((object :tk_objref) (unmarshal-ior buffer (first params)))
      ((anon-struct)
       (loop for type in params
           collect (unmarshal type buffer)))
      ((:tk_TypeCode) (unmarshal-typecode buffer))
      ((:tk_union)    (unmarshal-union params buffer))
      (t
       (unmarshal (or (get kind 'corba-typecode)
                      (error "Can't handle TypeCode of kind ~a" kind))
                  buffer)))))

(defun unmarshal-multiple (typecodes buffer)
  (declare (optimize speed))
  (mapcar #'(lambda (tc) (unmarshal tc buffer))
          typecodes))


;;;; GIOP extras

(defvar *giop-version* )

(defun unmarshal-giop-header (buffer)
  (unless (loop for c in '(#\G #\I #\O #\P)
		always (eql c (unmarshal-char buffer)))
    (error "Not a GIOP message"))
  (let* ((major (unmarshal-octet buffer))
	 (minor (unmarshal-octet buffer))
	 (byte-order (unmarshal-octet buffer))
	 (msgtype (unmarshal-octet buffer)))
    (setq *giop-version* (+ (* 100 major) minor))
    (setf (buffer-byte-order buffer) byte-order)
    msgtype))


(defun unmarshal-service-context (buffer)
  (unmarshal-sequence-m (buffer) (unmarshal-tagged-component buffer)))
