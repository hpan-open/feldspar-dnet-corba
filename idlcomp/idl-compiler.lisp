;; idl-compiler.lisp
;; Modified by lenst

(in-package "CLORB.IDLCOMP")


;;  this is a specialized reader for the idl-compiler

(defvar *current-idl-line*)

(defun make-line-reader (stream)
  (setq *current-idl-line* 0)
  (lambda ()
    (let ((line (read-line stream nil nil nil)))
      (incf *current-idl-line*)
      (if (not line)
        line
	(if (not (and (>= (length line) 1) (char= (char line 0) #\#)))
          (concatenate 'string line (string #\newline)) ; add a newline, since  an empty line is whitespace
	  (progn
	    (cond
             ; deal with the prefix pragma 
	     ((and (>= (length line) 14) (string= (subseq line 0 14) "#pragma prefix"))
	      (let ((from (position #\" line))
		    (to (position #\" line :from-end t)))
		(unless (and from to)  (error "wrong pragma prefix"))
		(setf (car *current-idl-prefix*) (subseq line (1+ from) to))
		" "))
             ; drop other pragmas
	     ((and (>= (length line) 7) (string= (subseq line 0 7) "#pragma")) " "))
	    ;;(pprint line)
	    " "))))))


(defun parse-stream (s)
  (let* (;;(token)
         (res)
         ;;(line 0)
         (base-tokenizer
          (make-scanner  *table* *action* *start*  (make-line-reader s))))
    (labels ((tokenizer ()
               (setf res (funcall base-tokenizer))
					;(pprint res)
               (cond
                 ((not res)  (return-from tokenizer (cons nil  nil)))
                 ((not (cdr res))  (error "unkown token"))
                 (t (return-from tokenizer res))))
	     (parser-error (shifts reduces)
               (pprint shifts)
               (pprint reduces)
               (pprint (list "in line" *current-idl-line*))
               (error "Error while parsing")))
      
      (lalr-parser #'tokenizer #'parser-error))))


(defun preprocess-command (file)
  (concatenate 'string "cpp '" (clorb::external-namestring file) "'"))

(defun parse-file (name)
  (let ((s (shell-to-string-or-stream (preprocess-command name))))
    (cond ((stringp s)
           (with-input-from-string (in s)
             (parse-stream in)))
          (t
           (with-open-stream (in s)
             (parse-stream in))))))

(defun save-idef (name1 name2)
  (let ((b (parse-file name1)))
    (with-open-stream (a (open name2 :direction :output))
      (pprint '(in-package :clorb) a)
      (pprint (append '(idef-definitions) b) a))))




;;;; Connect to CLORB

(defclass idl-compiler-impl (idl-compiler)
  ())

(defun convert-package (list)
  (let ((string (with-output-to-string (s)
                  (let ((*package* (find-package :clorb.idlcomp)))
                    (print list s)))))
    (let ((*package* (find-package :clorb)))
      (read-from-string string))))

(defmethod load-repository ((self idl-compiler-impl) repository file)
  (setf *current-idl-line* 0)
  (idef-read (convert-package (parse-file file))
             repository))

(unless *default-idl-compiler*
  (setq *default-idl-compiler* (make-instance 'idl-compiler-impl)))

#|
(with-open-file (s #P"CLORB:IDL;x-01.idl")
  (parse-stream s))

(with-open-file (s #P"CLORB:IDL;x-04.idl")
  (parse-stream s))

|#