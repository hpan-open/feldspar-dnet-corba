(in-package :cl-user)


(defun all-ops ()
  (let ((ops nil))
    (do-external-symbols (op :op)
      (push op ops))
    (setq ops (sort ops #'string<))
    (let ((*package* (find-package :op)))
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
