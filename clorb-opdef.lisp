;;;; clorb-opdef.lisp -- Operation definitions (opdef)

(in-package :clorb)

(defstruct opdef
  name
  (params nil )
  (result CORBA:tc_void)
  raises)

;;(define-slot-dumper opdef)

#|
(defmethod op:type :before ((s CORBA:ParameterDescription) &rest x)
  (declare (ignore x))
  (unless (slot-value s 'type)
    (setf (slot-value s 'type)
      (op:type (op:type_def s)))))
|#

(defun make-param (name mode typecode)
  (check-type mode CORBA::ParameterMode)
  (CORBA:ParameterDescription :name name :type typecode :mode mode))

(defun make-getter-opdef (name attdef)
  (make-opdef
   :name name
   :result (op:type attdef)))

(defun make-setter-opdef (name attdef)
  (make-opdef
   :name name
   :params (list (make-param "" :param_in (op:type attdef)))))


(defun opdef-params-with-mode (opdef modes)
  (loop for param in (opdef-params opdef)
        when (member (op:mode param) modes)
        collect param))

(defun opdef-inparams (opdef)
  (opdef-params-with-mode opdef '(:param_in :param_inout)))

(defmethod opdef-inparam-typecodes ((opdef opdef))
  (mapcar #'op:type (opdef-inparams opdef)))

(defun opdef-outparams (opdef)
  (nconc
   (if (eq (typecode-kind (opdef-result opdef)) :tk_void)
       nil
     (list (make-param "" :param_out (opdef-result opdef))))
   (opdef-params-with-mode opdef '(:param_out :param_inout))))

(defmethod opdef-outparam-typecodes ((opdef opdef))
  (mapcar #'op:type (opdef-outparams opdef)))
