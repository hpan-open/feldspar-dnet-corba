;; hello-servant

(in-package :clorb)

(defclass hello-servant (PortableServer:Servant)
  ())

(define-method "GREET" ((s hello-servant))
  (error 'omg.org/corba:no_implement))


(defmethod servant-invoke ((servant hello-servant) operation input handler)
  (declare (ignorable input))
  (cond ((string= operation "greet")
         (let ((output (funcall handler :no_exception)))
           (marshal-string (op:greet servant) output)
           output))
        (t
         (call-next-method))))


