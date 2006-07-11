;;;
;;; Load the Simple-Queue Example
;;;

(in-package :cl-user)

(defvar *sq-defaults*
  (make-pathname :name nil :type nil :defaults *load-pathname*))

(CORBA:IDL (merge-pathnames
             (make-pathname :name "cddr-01-queue" :type "idl"
                            :directory '(:relative :up :up "idl"))
             *sq-defaults*))

(load (merge-pathnames "sq-impl" *sq-defaults*))

