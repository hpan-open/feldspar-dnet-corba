;;;; Buffers for marshaling

(in-package :clorb)

(deftype buffer-index ()
  '(integer 0 #.array-dimension-limit))

;;(deftype octets ()  '(vector CORBA:octet))

(defvar *empty-octets*
  (make-array 0 :element-type 'CORBA:octet))

(defstruct BUFFER
  (octets *empty-octets* :type octets)
  (position    0         :type buffer-index)
  (byte-order  1         :type bit)
  (start-pos   0         :type buffer-index))

(defun buffer-contents (buffer)
  (copy-seq (buffer-octets buffer)))

(defun buffer-abs-pos (buffer)
  (buffer-position buffer))

(defun buffer-rel-pos (buffer)
  (- (buffer-position buffer) (buffer-start-pos buffer)))

(defmacro with-sub-buffer ((buffer length) &body body)
  (let ((old-start '#:old-start)
        (old-pos   '#:old-pos)
        (old-byteorder '#:old-byteorder)
        (buffervar '#:buffervar)
        (lengthvar '#:lengthvar))
    `(let* ((,buffervar ,buffer)
            (,lengthvar ,length)
            (,old-start (buffer-start-pos ,buffervar))
            (,old-pos   (buffer-position ,buffervar))
            (,old-byteorder (buffer-byte-order ,buffervar)))
       (unwind-protect
           (progn
             (setf (buffer-start-pos ,buffervar) (buffer-position ,buffervar))
             ,@body)
         (setf (buffer-start-pos ,buffervar) ,old-start)
         (setf (buffer-position ,buffervar) (+ ,old-pos ,lengthvar))
         (setf (buffer-byte-order ,buffervar) ,old-byteorder)))))

(defun get-work-buffer ()
  (make-buffer
   :octets (make-array 2000
                       :adjustable t
                       :fill-pointer 0
                       :element-type 'CORBA:octet)))



(defun buffer-pos (buffer)
  (fill-pointer (buffer-octets buffer)))


(defmacro with-out-buffer ((buffer &key (resize 400)) &body body)
  "Open upp buffer.
In body: 
- buffer     the buffer,
- octets     the octet vector,
- start-pos  start pos in octets for this work,
- pos        current pos in octets (setf-able),
- (align n)  align output to n octets (n must be litteral),
- (put-octet n)  output octet n. "
  `(let* ((buffer ,buffer)
          (start-pos (buffer-start-pos buffer))
          (octets (buffer-octets buffer)))
     (declare (ignorable start-pos octets)
              (type buffer-index start-pos))
     (macrolet ((put-octet (n)
                  `(vector-push-extend ,n octets ,',resize))
                (align (n) 
                  `(let ((skip (- ,n (logand (the buffer-index (- (fill-pointer octets) start-pos))
                                             (- ,n 1)))))
                     (declare (fixnum skip))
                     (when (< skip ,n)
                       (dotimes (x skip)
                         (declare (fixnum x))
                         (vector-push-extend 0 octets ,',resize))))))
       (symbol-macrolet ((pos (fill-pointer octets)))
         ,@body))))


(defmacro with-in-buffer ((buffer &key (check t)) &body body)
  `(progn
     ,@(if check `((check-type ,buffer buffer "a buffer")))
     (let ((buffer ,buffer))
       (declare (type buffer buffer))
       (let ((octets (buffer-octets buffer)))
         (symbol-macrolet ((pos (buffer-position buffer)))
           (macrolet ((get-octet () 
                        `(the CORBA:Octet
                           (prog1 (aref octets pos)
                             (setf pos (the buffer-index (1+ pos))))))

                      (align (n)
                        `(incf pos (logand (- ,n (logand (the buffer-index (buffer-rel-pos buffer))
                                                         (- ,n 1)))
                                           (- ,n 1)))))
             ,@body ))))))

