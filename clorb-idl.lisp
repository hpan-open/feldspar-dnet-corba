;; clorb-idl

(in-package :clorb)

(defclass idl-compiler () ())

(defgeneric load-repository (idl-compiler repository file))

(defvar *default-idl-compiler*)

(defun CORBA:IDL (file &key print)
  (let ((repository (make-instance 'repository)))
    (load-repository *default-idl-compiler* repository file)
    (let* ((target (make-instance 'code-target :dynamic-stubs nil))
           (code (target-code repository target)))
      (dolist (x (remove nil (cdr code)))
        (when print 
          (terpri)
          (pprint x))
        (eval x))
      (when print (terpri)))))
