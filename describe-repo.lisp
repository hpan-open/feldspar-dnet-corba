(in-package :clorb)

(defun describe-repo (r)
  (let ((l (coerce (op:contents r :dk_all t) 'list))
        (k (op:def_kind r)))
    (pprint-logical-block (*standard-output* l :prefix "  ")
      (loop for x in l
            for kind = (op:def_kind x)
            for f = nil then t
            do (when f (pprint-newline (if (eq k :dk_Interface)
                                         :fill :mandatory)))
            (format *standard-output* "~A ~S  "
                    (op:name x) kind)
            (when (eq kind :dk_Module)
              (pprint-newline :mandatory)
              (describe-repo x))))))
