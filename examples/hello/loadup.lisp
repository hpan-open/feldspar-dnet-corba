;;;
;;; Load the hello world example
;;;

(in-package :cl-user)

(defvar *hello-defaults*
  (make-pathname :name nil :type nil :defaults *load-pathname*))


(CORBA:IDL (make-pathname :name "hello" :type "idl"
                          :defaults *hello-defaults*))

(load (merge-pathnames "hello-server" *hello-defaults*))
(load (merge-pathnames "hello-client" *hello-defaults*))


#|

(defun hh ()
  (setup-hello :file "hello.ior")
  (hello-client :file "hello.ior"))

(defun hhn ()
  (setup-hello :name "hello")
  (hello-client :name "hello"))


|#
