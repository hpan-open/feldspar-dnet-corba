(in-package :clorb)

(defun ir-add-all (seq &optional (level 0))
  (map nil 
    (lambda (x &aux (kind (invoke x "_get_def_kind"))
                    (id   (invoke x "_get_id"))
                    (indent (make-string level :initial-element #\. )))
      (mess 3 "-- ~A ~A - ~S - ~A" 
            indent
            (invoke x "_get_absolute_name") kind id)
      (case kind
           (:dk_Module 
            (ir-add-all (invoke x "contents" :dk_All t)
                        (1+ level)))
           (:dk_Interface
            (or (known-interface id)
                (add-interface (interface-from-def x))))
           ((:dk_Struct :dk_Alias :dk_Exception :dk_Enum)
            ;; IDL - types
            (or (known-idl-type id)
                (typecode-from-def x)))))
    seq))

(defun download-ir ()
  (ir-add-all (corba:funcall "contents" (get-ir) :dk_all t)))
