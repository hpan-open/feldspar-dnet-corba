(in-package :clorb)


(defparameter *base-files*
  '(("clorb-pkgdcl")
    "clorb-options"
    "clorb-supp"
    ("clorb-macros" t)
    "clorb-sysdep"
    "clorb-basetypes"
    ("clorb-exceptions" t)
    ("clorb-typecode" t)
    ("clorb-struct" t)
    "clorb-union"
    "clorb-any"
    "clorb-object"
    ("clorb-buffer" t)
    "clorb-marshal"
    ("clorb-unmarshal" )
    ("clorb-opdef" t)
    "clorb-io"
    "clorb-iiop"
    "clorb-orb"
    ("clorb-iir" t)
    "clorb-util"
    "dumpir"))

(defparameter *server-files*
  '("clorb-objkey"
    ("clorb-srvreq" t)
    "clorb-servant"
    "clorb-trie"
    "clorb-poamgr"
    ("clorb-poa" t)
    "clorb-srv"))


(defparameter *x-files*
  '("test-suite"
    ;; Experimental
    "orb-export"
    "orb-structs"
    ("local-ir" t)
    "idef-read"
    "idef-write"
    ("idef-macros" t)
    "test-suite"
    "internalize"
    ;"code-gen"
    ;; Services
    "cosnaming-idl"
    "cosnaming-stub"
    "cosnaming-skel"
    "pns-server"
    ;"ec-server"
    ;; Example
    "hello-idl"
    "hello-server"
    "hello-client"
    ;; Code Gen
    ;;"target"
    ))


(defvar *load-dates* (make-hash-table :test #'equal))


(defun compile-changed (files)
  (loop
    with *default-pathname-defaults* = (make-pathname :host "clorb")
    with defs-date = 0
    for x in files
    for (base defs) = (if (consp x) x (list x nil))
    do (let* ((cf (compile-file-pathname base))
              (sf (make-pathname :type "lisp" :defaults base)))
         (when (or (not (probe-file cf))
                   (let ((dcf (file-write-date cf))
                         (dsf (file-write-date sf)))
                     (or (null dcf) (null dsf)
                         (progn
                           #+ignore
                           (format t "~&;; Exam comp ~A(~A):~A(~A)~%"
                                   sf dsf cf dcf )
                           nil)
                         (> dsf dcf)
                         (> defs-date dcf))))
           (format t "~&;;;; Compiling ~A ~%" sf )
           (compile-file base))
         (let ((dcf (file-write-date cf))
               (last (gethash base *load-dates*)))
           (when defs
             (setq defs-date (max (or dcf 0) defs-date)))
           (when (or (null last) (null dcf)
                     (> dcf last))
             (format t "~&;;;; Loading ~A ~%" base)
             (load base)
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

