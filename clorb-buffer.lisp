;;;; Buffers for marshaling

(in-package :clorb)

(deftype buffer-index ()
  '(integer 0 #.array-dimension-limit))

(deftype octets ()
  '(vector CORBA:octet))

(defvar *empty-octets*
  (make-array 0 :element-type 'CORBA:octet))

(defstruct buffer
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
   :octets (make-array 200 
                       :adjustable t
                       :fill-pointer 0
                       :element-type 'CORBA:octet)))
