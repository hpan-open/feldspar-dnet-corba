(in-package :clorb)

(defmacro define-test-suite (name &body body)
  `(eval-when (:load-toplevel :execute)
     (let ((tc-count 0)
         (tc-error 0)
         (tc-current nil))
     (labels
         ((tc-report (msg &rest args)
            (incf tc-error)
            (format t "~&;;; In test case ~A~%;;;! ~A~%" 
                    tc-current
                    (apply #'cl:format nil msg args)))
          (ensure-equalp (is shouldbe)
            (unless (equalp is shouldbe)
              (tc-report "~S~_ should be~_ ~S"
                         is shouldbe)))
          (ensure (bool &optional description)
            (unless bool
              (tc-report "~A fail"
                         (or description "ensure")))))
       
       (macrolet ((define-test (name &body body)
                      `(handler-case
                           (progn
                             (setq tc-current ,(string name))
                             (incf tc-count)
                             ,@body)
                         (null ()
                            ;error (exc)
                           (tc-report "Exception ~A" exc)))))
         
         ,@body
         (format t "~&;;; ------------- ~A finished -----------------"
                 ,(string name))
         
         (format t "~&;;; ~D tests executed with ~D errors"
                 tc-count tc-error))))))
