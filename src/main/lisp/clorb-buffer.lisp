;;;; clorb-buffer.lisp -- Buffers for marshaling

(in-package :clorb)

(deftype buffer-index ()
  '(integer 0 #.array-dimension-limit))

;;(deftype octets ()  '(vector CORBA:octet))

(defvar *empty-octets*
  (make-array 0 :element-type 'CORBA:octet))



(defstruct buffer
  (giop-version :giop-1-0 :type symbol)
  (orb          nil)
  (octets       *empty-octets* :type octets)
  (in-pos       0         :type buffer-index)
  (byte-order   1         :type bit)
  (start-pos    0         :type buffer-index)
  (ind-hash     nil))



;;;; Chunking Support


(defvar *chunking-level* 0)
(declaim (type fixnum *chunking-level*))

(define-symbol-macro chunking-p (> *chunking-level* 0))


(defvar *chunk-end* nil
  "Where current chunk is ending in the buffer. If nil, not in a chunk.")


(defmacro with-in-chunking ((chunking) &body body)
  `(let ((*chunking-level* 
          (if (or ,chunking (not (zerop *chunking-level*)))
            (1+ (abs *chunking-level*))
            *chunking-level*))
         (*chunk-end* nil))
    ,@body))


(defmacro without-chunking ((buffer) &body body)
  `(progn
     (when (and chunking-p *chunk-end*)
       (assert (= *chunk-end* (buffer-in-pos ,buffer)))
       (setq *chunk-end* nil))
     (assert (>= *chunking-level* 0))
     (let ((*chunking-level* (- *chunking-level*)))
       ,@body )))



;;; clorb-buffer.lisp ends here
