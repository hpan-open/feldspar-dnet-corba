;; clorb-idl

(in-package :clorb)

(defclass idl-compiler () 
  ((include-directories :initform *default-include-directories*
                        :initarg :include-directories
                        :accessor include-directories)))


(defgeneric load-repository (idl-compiler repository file))

(defvar *default-idl-compiler* nil)

(defvar *default-exclude* '("::CORBA"))

(defun CORBA:IDL (file &key print (eval t) 
                         only (exclude *default-exclude*)
                         (skeleton t)
                         (compiler *default-idl-compiler*))
  (let ((repository (make-instance 'repository)))
    (load-repository compiler repository file)
    (flet ((lookup (name)
             (op:lookup repository name)))
      (let* ((target (make-instance (if skeleton 'all-target 'stub-target)
                       :excludes (mapcar #'lookup exclude)))
             (code (if only
                     (make-progn (mapcar (lambda (name)
                                           (target-code (lookup name) target))
                                         (mklist only)))
                     (target-code repository target))))
        (dolist (x (remove nil (cdr code)))
          (when print 
            (terpri)
            (pprint x))
          (when eval
            (eval x)))
        (when print (terpri)))
      repository)))

(defun mklist (x)
  (if (consp x) x (list x)))


#|
(mapcar (lambda (name)
          (target-code (lookup name) target))
        (mklist only))

(map-of 'list (target-code (:of (map-of 'list (lookup (:of (mklist only)))))
                           target))

(mapcar-of (target-code (:of (mapcar-of (lookup (:of (mklist only))))) target))

(mapcar-of (target-code (:of (lookup (:of (mklist only)))) target))
(mapcar-of (target-code (lookup (:of (mklist only))) target))

(target-code @(lookup @(mklist only)) target)

|#