(in-package :cl-user)


(defun all-ops (&optional (package :op))
  (let ((ops nil))
    (do-external-symbols (op package)
      (push op ops))
    (setq ops (sort ops #'string<))
    (let ((*package* (find-package package)))
      #-clisp
      (pprint-logical-block (*standard-output* ops
                                               :prefix "  ("
                                               :suffix ")")
        (princ ":export")
        (pprint-newline :mandatory *standard-output*)
        (pprint-fill *standard-output* ops nil))
      #+clisp
      (loop for op in ops and n from 0
            initially (terpri) (princ "  (:export")
            finally   (princ ")")
            when (zerop (rem n 3))
            do (terpri) (princ "   ")
            do (princ op) (princ " ")))
    (values)))

(all-ops)
