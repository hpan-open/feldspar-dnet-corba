
(in-package :cl-user)

(defclass my-class (PortableServer:DynamicImplementation)
  ())

(corba:define-method primary_interface ((x my-class) oid poa)
  (declare (ignore oid poa))
  "IDL:some.example/MyClass:1.0")

(defmethod foobar ((self my-class) x)
  (let ((result 1)
        (y (1+ x)))
    (values result y)))


;;; long foobar(in long x, out long y);

(corba:define-method invoke ((self my-class) r)
  (cond
    ((equal (op:operation r) "foobar")
     (let ((args
            (list (corba:NamedValue
                   :argument (corba:any :any-typecode corba:tc_long)
                   :arg_modes corba:ARG_IN)
                  (corba:NamedValue :arg_modes corba:ARG_OUT))))
       (setq args (op:arguments r args))
       (multiple-value-bind (res y)
           (foobar self (corba:any-value (op:argument (first args))))
         (let ((_y (op:argument (second args))))
           (setf (corba:any-typecode _y) corba:tc_long)
           (setf (corba:any-value _y) y))
         (op:set_result r (corba:any :any-typecode corba:tc_long
                                     :any-value res)))))
    (t
     (op:set_exception r (make-condition 'CORBA:BAD_OPERATION
                                         :completed :COMPLETED_NO)))))


(defun setup-dsi-example ()
  (clorb:rebind (make-instance 'my-class) "dsi"))

