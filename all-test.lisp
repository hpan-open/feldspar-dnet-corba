(in-package :clorb)

(let ((*test-suite-result* (make-instance 'test-result
                             :parent nil
                             :suite "All Tests"))) 
  (loop for pn in (directory (or #+(or clisp openmcl) "test-*.lisp"
                                 "CLORB:SRC;test-*.lisp"))
        unless (string= "test-suite" (pathname-name pn))
        do (format t "~&;;;; ~A~%" (pathname-name pn))
        (load pn))
  (format t "~&;;; ==================================================~%" )
  (print-result *test-suite-result*))
