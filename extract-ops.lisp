(in-package :cl-user)

(defun all-ops ()
  (let ((ops nil))
    (do-external-symbols (op :op)
      (push op ops))
    (setq ops (sort ops #'string<))
    (let ((*package* (find-package :op)))
      (pprint-logical-block (*standard-output* ops
                                               :prefix "  ("
                                               :suffix ")")
        (princ ":export")
        (pprint-newline :mandatory *standard-output*)
        (pprint-fill *standard-output* ops nil)))
    (values)))

(all-ops)
