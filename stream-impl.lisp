(in-package :clorb)

(defparameter *str-stream-idef*
    (lookup-name "Streams_0::str_stream"))

(defclass STR-STREAM-IMPL (servant)
  ((acc :initform ""))
  (:default-initargs
      :interface-id (op:id *str-stream-idef*)
    :interface-def *str-stream-idef*))


(define-method clear ((obj str-stream-impl))
  (setf (slot-value obj 'acc) ""))

(define-method contents ((obj str-stream-impl))
  (slot-value obj 'acc))

(define-method put_string ((obj str-stream-impl) string)
  (setf (slot-value obj 'acc)
    (concatenate 'string (slot-value obj 'acc)
                 string)))

(define-method put_long ((obj str-stream-impl) n)
  (op:put_string obj (format nil "~S" n)))


(defun setup-stream ()
  (rebind (make-instance 'str-stream-impl)
          "acc"))
