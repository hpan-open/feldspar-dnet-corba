;;;-*- Mode: Lisp; Package: (NET.CDDR.CLORB.SYSTEM) -*-

(defpackage "NET.CDDR.CLORB.SYSTEM"
  (:use "COMMON-LISP")
  (:export "SET-LOAD-OPTS" "RELOAD"))

(in-package "NET.CDDR.CLORB.SYSTEM")


;;; Load options

(defmacro def-load-opts (setter tester &rest opts)
  (let ((vars (loop for (key) in opts
                    collect (intern (symbol-name key) *package*))))
    `(let ((opts (make-hash-table)))
       (defun ,tester (key)
         (if (eq key 'print-it)
           (loop for (key nil comment) in ',opts do
                 (format t "~22S ~5S ~A~%" key (gethash key opts) comment))
           (multiple-value-bind (val good) (gethash key opts)
             (unless good (warn "Undefined load opt: ~S" key))
             val)))
       (defun ,setter (&key ,@(loop for (nil def) in opts for var in vars 
                                    collect `(,var ,def)))
         ,(format nil "Set load options~:{~%~A (~A) - ~A~}" opts)
         ,@(loop for (key) in opts for var in vars
                 collect `(setf (gethash ,key opts) ,var)))
       (,setter))))

(def-load-opts set-load-opts load-opt
  (:server t "Load the code needed to server objects")
  (:idlcomp nil "Load the old IDL parser")
  (:my-idlparser t "Load the new IDL parser")
  (:portable-interceptor nil "Load support for Portable Interceptors"))


;;; Files

(defparameter *base-files*
  '(("clorb-pkgdcl")
    "clorb-options"
    "clorb-supp"
    ("clorb-macros" t)
    "clorb-sysdep"
    "clorb-basetypes"
    "clorb-typecode-1"
    ("clorb-buffer" t)
    "clorb-marshal"
    "clorb-unmarshal"
    "clorb-typecode-2"
    ("clorb-macros2" t)
    "clorb-any"
    "clorb-struct"
    "clorb-union"
    "clorb-exceptions"
    "clorb-object"
    "clorb-request"
    "clorb-iop"
    "clorb-value"
    "clorb-io"
    "clorb-ifr-base"
    "clorb-orb"
    "clorb-iiop"
    "cosnaming-stub"
    "clorb-util"
    "clorb-ifr"
    "clorb-target"
    "clorb-idl"
    "clorb-idlcpp" ))

(defparameter *server-files*
  '("clorb-objkey"
    "clorb-servant"
    "clorb-trie"
    "clorb-poa-base"
    "clorb-poa"
    "clorb-srv"
    "clorb-ifr-info"))

(defparameter *redpas-files*
  '("redpas;package"
    "redpas;lexer"
    "redpas;parsys"))

(defparameter *dev-pre-files*
  '("luna;package"
    "luna;pattern"
    "luna;testsuite"))

(defparameter *dev-post-files*
  '("support-test"))

(defparameter *portable-interceptor-files*
  '("clorb-pi-base"
    "clorb-pi-impl"))

(defparameter *x-files*
  '(;; Experimental
    "idef-read"
    "idef-write"
    ("idef-macros" t)
    "clorb-ifr-export"
    ;; Services
    ;;"cosnaming-idl"
    "cosnaming-skel"
    "pns-server"))

(defparameter *idlcomp*
  '( ;; IDL Compiler
    "idlcomp;lisp-scanner;scanner-support"
    "idlcomp;idl-compiler-support"
    "idlcomp;idl-scanner-parser"
    "idlcomp;idl-compiler" ))

(defparameter *my-idlparser*
  '("clorb-idllexer"
    "clorb-idlparser"))


(defvar *load-dates* (make-hash-table :test #'equal))
(defvar *source-pathname-defaults* (make-pathname :name nil :type nil
                                                  :defaults *load-pathname*))

(defvar *binary-folder* (or #+clisp "fasl"))


(defun dir-split (base)
  (declare (string base))
  (let ((dir-pos (position #\; base)))
    (if dir-pos
      (cons (subseq base 0 dir-pos)
            (dir-split (subseq base (1+ dir-pos))))
      (list base))))

(defun compile-changed (files)
  (loop
    with defs-date = 0
    for x in files
    for (base defs) = (if (consp x) x (list x nil))
    do (let* ((names (dir-split base))
              (name (car (last names)))
              (dir (nbutlast names))
              (sf (merge-pathnames
                   (make-pathname :name name :type "lisp"
                                  :directory (if dir (cons :relative dir)))
                   *source-pathname-defaults*))
              (cf-tail (if *binary-folder*
                           (list :output-file
                                 (merge-pathnames
                                  (make-pathname
                                   :name nil :type #-sbcl nil #+sbcl "fasl"
                                   :directory (list* :relative
                                                     *binary-folder* dir))
                                  *source-pathname-defaults*)) ))
              (cf (apply #'compile-file-pathname sf cf-tail)))
         (when (or (not (probe-file cf))
                   (let ((dcf (file-write-date cf))
                         (dsf (file-write-date sf)))
                     (or (null dcf) (null dsf)
                         (progn
                           #+(or)
                           (format t "~&;; Exam comp ~A(~A):~A(~A)~%"
                                   sf dsf cf dcf )
                           nil)
                         (> dsf dcf)
                         (> defs-date dcf))))
           (format t "~&;;;; Compiling ~A ~%" sf )
           (ensure-directories-exist cf)
           (apply #'compile-file sf cf-tail))
         (let ((dcf (file-write-date cf))
               (last (gethash base *load-dates*)))
           (when defs
             (setq defs-date (max (or dcf 0) defs-date)))
           (when (or (null last) (null dcf)
                     (> dcf last))
             (format t "~&;;;; Loading ~A ~%" base)
             (load cf)
             (setf (gethash base *load-dates*) dcf))))))


(defun reload ()
  #+clorb-packer-redpas (packer:require-package :net.cddr.redpas)
  (compile-changed 
   (append #+clorb-dev *dev-pre-files*
           #-clorb-packer-redpas *redpas-files*
           *base-files*
           (if (load-opt :server) *server-files*)
           *x-files*
           (if (load-opt :portable-interceptor) *portable-interceptor-files*)
           #+clorb-dev *dev-post-files*
           (if (load-opt :idlcomp) *idlcomp*)
           (if (load-opt :my-idlparser) *my-idlparser*) )))


(defun acl-defsys ()
  (format t "(defsystem :clorb ()~%")
  (let ((groups '((base . *base-files*)
                  (server . *server-files*)
                  (x-files . *x-files*))))
    (format t "~2t(:definitions (:serial . ~S)~%~8t(:serial . ~A))~%"
            '("clorb-pkgdcl" "clorb-macros")
            (mapcar 'car groups))
    (loop
     for (name . var) in groups
     do (format t "~&~2t(:module-group ~A~%~4t(:serial ~{~14,1t~S~^~%~}))"
                name
                (mapcar (lambda (x)
                          (if (consp x)
                              (car x)
                              x))
                        (symbol-value var)))))
  (format t ")~%"))
