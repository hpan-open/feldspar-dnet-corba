;;;
;;; Load the hello world example
;;;

(in-package :cl-user)

(CORBA:IDL (merge-pathnames ";examples;hello;hello.idl"
                            net.cddr.clorb.system::*source-pathname-defaults*))
