;;;-*- Mode: Lisp; Package: (NET.CDDR.CLORB.SYSTEM) -*-

(defpackage "NET.CDDR.CLORB.SYSTEM"
  (:use "COMMON-LISP")
  (:export "RELOAD"))

(in-package "NET.CDDR.CLORB.SYSTEM")

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
    "clorb-value"
    "clorb-iop"
    "clorb-pi-base"
    "clorb-object"
    "clorb-io"
    "clorb-ifr-base"
    "clorb-iiop"
    "clorb-pi-impl"
    "clorb-orb"
    ;;("clorb-opdef" t)
    ;;"clorb-iir"
    "cosnaming-stub"
    "clorb-util"
    "clorb-ifr"
    "clorb-target"
    "clorb-idl"
    "clorb-idlcpp"
    ;;"dumpir"
    ))

(defparameter *server-files*
  '("clorb-objkey"
    "clorb-servant"
    "clorb-trie"
    "clorb-poa-base"
    "clorb-poa"
    "clorb-srv"
    "clorb-ifr-info"))

(defparameter *dev-pre-files*
  '("luna;package"
    "luna;testsuite"
    "luna;pattern"))

(defparameter *dev-post-files*
  '("support-test"))

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
  '("my-lexer"
    "my-idlparser"))


(defvar *load-dates* (make-hash-table :test #'equal))
(defvar *source-pathname-defaults* *load-pathname*)


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
              (cf (compile-file-pathname sf)))
         
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
           (compile-file sf))
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
  (compile-changed (append #+clorb-dev *dev-pre-files*
                           *base-files*
                           *server-files*
                           *x-files*
                           #+clorb-dev *dev-post-files*
                           #-no-idlcomp *idlcomp*
                           #+use-my-idlparser *my-idlparser* )))


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
