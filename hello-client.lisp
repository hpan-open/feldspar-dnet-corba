(in-package :cl-user)

(defun hello-client (&key file name count)
  (let* ((orb (CORBA:ORB_init))
         (object (cond
                  (file
                   (op:string_to_object 
                    orb (with-open-file (rd file :direction :input)
                          (read-line rd))))
                  (name
                   (clorb:resolve name))
                  (t
                   (error "Supply :file or :name")))))
    (if count
        (dotimes (n count)
          (corba:funcall "greet" object))
      (corba:funcall "greet" object))))
