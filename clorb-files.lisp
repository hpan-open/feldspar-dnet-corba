(in-package :clorb)

(defparameter *base-files*
  '(("clorb-pkgdcl")
    "clorb-options"
    "clorb-supp"
    ("clorb-macros" t)
    "clorb-sysdep"
    "clorb-basetypes"
    "clorb-typecode"
    "clorb-any"
    "clorb-struct"
    "clorb-union"
    ("clorb-macros2" t)
    "clorb-exceptions"
    "clorb-iop"
    "clorb-object"
    ("clorb-buffer" t)
    "clorb-marshal"
    "clorb-unmarshal"
    "clorb-io"
    "clorb-ifr-base"
    "clorb-iiop"
    "clorb-orb"
    ("clorb-opdef" t)
    "clorb-iir"
    "cosnaming-stub"
    "clorb-util"
    "clorb-ifr"
    "clorb-target"
    "clorb-idl"
    "dumpir"))

(defparameter *server-files*
  '("clorb-objkey"
    ;;("clorb-srvreq" t)
    "clorb-servant"
    "clorb-trie"
    "clorb-poa-base"
    "clorb-poamgr"
    "clorb-poa"
    "clorb-srv"))


(defparameter *x-files*
  '("test-suite"
    ;; Experimental
    "internalize"
    "idef-read"
    "idef-write"
    ("idef-macros" t)
    "z-sexp-idl"
    ;; Services
    "cosnaming-idl"
    "cosnaming-skel"
    "pns-server"
    ;"ec-server"
    ;; Example
    "hello-idl"
    "hello-new-servant"
    "hello-server"
    "hello-client"
    ;; IDL Compiler
    "scanner-support"
    "idl-compiler"
    "idl-scanner-parser"
    ))


(defvar *load-dates* (make-hash-table :test #'equal))
(defvar *clorb-pathname-defaults* *load-pathname*)

(defun compile-changed (files)
  (loop
    with defs-date = 0
    for x in files
    for (base defs) = (if (consp x) x (list x nil))
    do (let* ((sf (make-pathname :name base :type "lisp" 
                                 :defaults *clorb-pathname-defaults*))
              (cf (compile-file-pathname sf))
              )
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
  (compile-changed (append *base-files*
                           *server-files*
                           *x-files*
)))


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

(defun gen-package-decl (&optional (package :op))
  (let ((exports nil))
    (do-external-symbols (op package)
      (push op exports))
    (format t "(defpackage ~S~%  (:use)~%" (package-name package))
    (let ((nicks (package-nicknames package)))
      (when nicks 
        (format t "  (:nicknames~{ ~S~})~%" nicks)))
    (setq exports (sort exports #'string<))
    (setq exports (mapcar #'symbol-name exports))
    (let ((*package* (find-package package)))
      #-clisp
      (pprint-logical-block (*standard-output* exports
                                               :prefix "  ("
                                               :suffix ")")
        (princ ":export")
        (pprint-newline :mandatory *standard-output*)
        (pprint-fill *standard-output* exports nil))
      #+clisp
      (loop for op in exports and n from 0
            initially (terpri) (princ "  (:export")
            finally   (princ ")")
            when (zerop (rem n 3))
            do (terpri) (princ "   ")
            do (prin1 op) (princ " ")))
    (format t ")~%")
    (values)))

#|
(gen-package-decl "CORBA")
(gen-package-decl "OP")
(gen-package-decl "IOP")
(gen-package-decl "OMG.ORG/PORTABLESERVER")
|#
