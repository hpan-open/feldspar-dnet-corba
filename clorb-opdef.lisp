;;;; Operation definitions (opdef)

(in-package :clorb)

(defstruct opdef
  name
  (params nil )
  (result CORBA:tc_void)
  raises)

;;(define-slot-dumper opdef)

(defmethod op:type :before ((s CORBA:ParameterDescription) &rest x)
  (declare (ignore x))
  (unless (slot-value s 'type)
    (setf (slot-value s 'type)
      (op:type (op:type_def s)))))


(setf (get :in 'param-mode) :param_in) 
(setf (get :out 'param-mode) :param_out)
(setf (get :inout 'param-mode) :param_inout)

(defun make-param (name mode typecode)
  (unless (typep mode 'CORBA::ParameterMode)
    (setq mode (get mode 'param-mode))
    (assert mode))
  (make-instance 'CORBA::ParameterDescription
    :name name
    :type typecode
    :mode mode))

(defun param-name (param)
  (op:name param))

(defun param-mode (param)
  (op:mode param))

(defun param-typecode (param)
  (op:type param))

(defun opdef-params-with-mode (opdef modes)
  (loop for param in (opdef-params opdef)
        when (member (param-mode param) modes)
        collect param))

(defun opdef-inparams (opdef)
  (opdef-params-with-mode opdef '(:param_in :param_inout)))

(defmethod opdef-inparam-typecodes ((opdef opdef))
  (mapcar #'param-typecode (opdef-inparams opdef)))

(defun opdef-outparams (opdef)
  (nconc
   (if (eq (typecode-kind (opdef-result opdef)) :tk_void)
       nil
     (list (make-param "" :param_out (opdef-result opdef))))
   (opdef-params-with-mode opdef '(:param_out :param_inout))))

(defmethod opdef-outparam-typecodes ((opdef opdef))
  (mapcar #'param-typecode (opdef-outparams opdef)))
