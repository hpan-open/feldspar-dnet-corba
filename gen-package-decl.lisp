;;; gen-package-decl.lisp

(packer:require-package "NET.CDDR.UTILS")


#|
(utils:gen-package-decl "CORBA")
(utils:gen-package-decl "OP")
(utils:gen-package-decl "IOP")
(utils:gen-package-decl "OMG.ORG/PORTABLESERVER")
|#


#+(or)
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
