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
	    (pprint line)
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


(defvar *idl-include-dir*)
(setf  *idl-include-dir* "/home/lenst/src/corba/interface/")


(defun preprocess-file (file)
  #+clisp
  (let ((st (make-pipe-input-stream
             (concatenate 'string "cpp -I " *idl-include-dir* " " file))))
    st)
  #-clisp
  (open file :direction :input))


(defun parse-file (name)
  (with-open-stream (s (preprocess-file name))
    (parse-stream s)))

(defun save-idef (name1 name2)
  (let ((b (parse-file name1)))
    (with-open-stream (a (open name2 :direction :output))
      (pprint '(in-package :clorb) a)
      (pprint (append '(idef-definitions) b) a))))




;;;; Connect to CLORB

(defclass idl-compiler (clorb::idl-compiler)
  ())

(defun convert-package (list)
  (let ((string (with-output-to-string (s)
                  (let ((*package* (find-package :clorb.idlcomp)))
                    (print list s)))))
    (let ((*package* (find-package :clorb)))
      (read-from-string string))))

(defmethod clorb::load-repository ((self idl-compiler) repository file)
  (setf *current-idl-line* 0)
  (clorb::idef-read (convert-package
                     (with-open-file (s file)
                       (parse-stream s)))
                    repository))

(setq clorb::*default-idl-compiler* (make-instance 'idl-compiler))

#|
(with-open-file (s #P"CLORB:IDL;x-01.idl")
  (parse-stream s))

(with-open-file (s #P"CLORB:IDL;x-04.idl")
  (parse-stream s))

|#