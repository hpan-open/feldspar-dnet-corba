;;;; clorb-supp.lisp
;; $Id: clorb-supp.lisp,v 1.6 2002/05/24 10:06:58 lenst Exp $

(in-package :clorb)

(defvar *log-output* t)

(defun mess (level fmt &rest args)
  (when (>= level *log-level*)
    (apply #'cl:format *log-output*
           (format nil "~~&~A ~A~~%" 
                   (make-string level :initial-element #\;)
                   fmt)
           args)
    #-clisp
    (finish-output *log-output*)))

(defun stroid (stream oid colon-p at-p)
  (declare (ignore colon-p at-p))
  (map nil
    (lambda (octet)
      (if (< 31 octet 127)
          (princ (code-char octet) stream)
        (format stream "<~x>" octet)))
    oid))


(defun ensure-corba-package (name &key nicknames export)
  (let ((package (find-package name)))
    (unless package
      (setq package (make-package name :nicknames nicknames :use '())))
    (export (mapcar (lambda (sym-name) (intern sym-name package)) export)
            package)))


;;; clorb-supp.lisp ends here
