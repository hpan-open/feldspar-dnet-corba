;;;; clorb-buffer-fns.lisp -- marshalling buffer functions


(in-package :clorb)


;;;; Alignment support


(defun align-amount (pos n)
  (if (zerop n) 0
      (let ((d (logand pos (1- n))))
        (if (zerop d) 0 (- n d)))))


(defun align-pos (pos n)
  (if (zerop n)
      pos
      (let ((d (logand pos (1- n))))
        (if (zerop d) pos (+ pos (- n d))))))



;;;; Accessors


(defmethod the-orb ((obj buffer))
  (or (buffer-orb obj)
      *the-orb*))

(defun buffer-contents (buffer)
  (copy-seq (buffer-octets buffer)))

(defun buffer-out-pos (buffer)
  (fill-pointer (buffer-octets buffer)))

(defun buffer-abs-pos (buffer)
  (buffer-in-pos buffer))

(defun buffer-rel-pos (buffer)
  (declare (optimize speed)
           (type buffer buffer))
  (- (buffer-in-pos buffer) (buffer-start-pos buffer)))

(defun buffer-length (buffer)
  (length (buffer-octets buffer)))

(defun buffer-record (buffer)
  (or (buffer-ind-hash buffer)
      (setf (buffer-ind-hash buffer) (make-hash-table :test #'eql))))



;;;; Creation

(defun get-work-buffer (orb)
  (make-buffer
   :orb orb
   :octets (make-array 2000
                       :adjustable t
                       :fill-pointer 0
                       :element-type 'CORBA:octet)))


;;;; Printing

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :identity t :type t)
    (format stream "~D/~S/~D ~A"
            (buffer-in-pos buffer)
            (ignore-errors (fill-pointer (buffer-octets buffer)))
            (buffer-start-pos buffer)
            (if (buffer-byte-order buffer) "L" "B"))))



;;; clorb-buffer-fns.lisp ends here
