
(defparameter *files*
  '((:file "clorb-pkgdcl")
    (:file "clorb-macros"  :depends-on ("clorb-pkgdcl"))
    (:file "clorb-options" :depends-on ("clorb-pkgdcl"))
    (:file "clorb-supp")
    (:file "clorb-basetypes")
    (:file "clorb-exceptions")
    (:file "clorb-object")
    (:file "clorb-typecode" :depends-on ("clorb-macros"))
    (:file "clorb-any")
    (:file "clorb-struct"   :depends-on ("clorb-macros"))
    (:file "clorb-union")
    (:file "clorb-buffer")
    (:file "clorb-marshal"  :depends-on ("clorb-buffer"))
    (:file "clorb-unmarshal" :depends-on ("clorb-buffer"))
    (:file "clorb-opdef")
    (:file "clorb-client")
    (:file "clorb-orb")
    (:file "clorb-request")
    (:file "clorb-iir")
    (:file "clorb-objkey")
    (:file "clorb-srvreq")
    (:file "clorb-servant" :depends-on ("clorb-iir"))
    (:file "clorb-trie")
    (:file "clorb-poamgr")
    (:file "clorb-poa")
    (:file "socket")
    (:file "clorb-util")
    (:file "clorb-srv" :depends-on ("socket"))
    (:file "dumpir"    :depends-on ("clorb-pkgdcl"))
    (:file "test-suite")
    ;; Experimental
    (:file "orb-export")
    (:file "orb-structs")
    (:file "local-ir")
    (:file "idef-read")
    (:file "idef-write")
    (:file "idef-macros")
    ;; Services
    (:file "cosnaming-idl")
    (:file "cosnaming-stub")
    (:file "cosnaming-skel")
    (:file "pns-server")
    (:file "ec-server" :depends-on ("clorb-srv"))
    ;; Example
    (:file "hello-server")
    (:file "hello-client")
    ;;                  (:file "cmucl-macros")
    ;;                  (:file "i386-linux-cmucl" :depends-on ("cmucl-macros"))
    ;;                  (:file "cmucl" :depends-on ("i386-linux-cmucl")
    ))


(require :db-sockets)

(defun loadem ()
  (loop for x in *files*
        when (eq (car x) :file)
        do (format t "~&;;;; Loading ~A ~%" (second x))
           (load (second x) :verbose t)))


(defun compileem (&optional load)
  (loop
   for x in *files*
   when (eq (car x) :file)
   do (let* ((base (second x))
             (cf (compile-file-pathname base))
             (sf (make-pathname :type "lisp" :defaults base)))
        (when (or (not (probe-file cf))
                  (let ((dcf (file-write-date cf))
                        (dsf (file-write-date sf)))
                    (or (null dcf) (null dsf)
                        (progn
                          #+ignore
                          (format t "~&;; Examining Compiling ~A (~A) : ~A (~A) ~%"
                                  sf dsf cf dcf )
                          nil)
                        (> dsf dcf))))
          (format t "~&;;;; Compiling ~A ~%" sf )
          (compile-file base))
        (when load
          (load base)))))

(compileem t)
(clorb::load-ir)


;; Local variables:
;; inferior-lisp-program: "sbcl"
;; End:
