(in-package :clorb)


(defparameter *base-files*
  '(("clorb-pkgdcl")
    ("clorb-macros" t)
    "clorb-options"
    "clorb-supp"
    "clorb-basetypes"
    ("clorb-exceptions" t)
    "clorb-object"
    ("clorb-typecode" t)
    "clorb-any"
    ("clorb-struct" t)
    "clorb-union"
    ("clorb-buffer" t)
    "clorb-marshal"
    ("clorb-unmarshal" t)
    ("clorb-opdef" t)
    "clorb-client"
    "clorb-orb"
    "clorb-request"
    ("clorb-iir" t)
    "socket"
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
    ;; Services
    "cosnaming-idl"
    "cosnaming-stub"
    "cosnaming-skel"
    "pns-server"
    "ec-server"
    ;; Example
    "hello-server"
    "hello-client"))


(defvar *load-dates* (make-hash-table :test #'equal))


(defun compile-changed (files)
  (loop
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
                           *x-files*)))
