(in-package :cl-user)

(defun hello-client (&key file name)
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
    (corba:funcall "greet" object)))
