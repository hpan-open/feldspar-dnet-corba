;;;; clorb-supp.lisp
;; $Id: clorb-supp.lisp,v 1.1 2000/11/14 22:50:28 lenst Exp $

(in-package :clorb)


(defun mess (level fmt &rest args)
  (when (>= level *log-level*)
    (apply #'cl:format t
           (format nil "~&~A ~A~%" 
                   (make-string level :initial-element #\;)
                   fmt)
           args)))

(defun stroid (stream oid colon-p at-p)
  (declare (ignore colon-p at-p))
  (map nil
    (lambda (octet)
      (if (< 31 octet 127)
          (princ (code-char octet) stream)
        (format stream "<~x>" octet)))
    oid))


;;; clorb-supp.lisp ends here
