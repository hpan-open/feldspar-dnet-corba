;; idl-compiler.lisp
;; Modified by lenst

(in-package "CLORB.IDLCOMP")

(DEFVAR *ACTION*)
(DEFVAR *START*)
(DEFVAR *TABLE*)


;;(use-package :scanner-support)

; these procedures are needed by the parser, or will be someday

(defun id->string (id)
  (let ((s (if (eq (car id) 'colon) (concatenate 'string "::" (cadr id))  (cadr id))))
    (dolist (x (cddr id))
      (setf s (concatenate 'string s "::" x)))
    s))


(defun string->double (str)
  (read-from-string (substitute #\d #\e str)))

(defun string->integer (str)
  (if (equalp str "0") (return-from string->integer 0))
  (read-from-string 
   (if (equalp (char str 0) #\0)
       (if (member (char str 1) '(#\x #\X))
	   (concatenate 'string "#x" (subseq str 2))
	 (concatenate 'string "#o" (subseq str 1)))
	 str)))



; simple 
(defun string->string (str)
  (subseq str 1 (1- (length str))))

(defun string->char (str)
  (char str 1))

;
(defun idl-expand (l pos)
  (mapcar #'(lambda (x) (let ((new (copy-list l))) (setf (elt new pos) x) new)) (elt l pos)))


(defun idl-flatten (l)
  (let ((ans nil))
    (do ((l l (cdr l)))
      ((endp l) (reverse ans))
      (if (consp (caar l))
	  (setf ans (append (reverse (car l)) ans))
	(push (car l) ans)))))

; the current prefix, this is a list because we have to deal with included files (not done yet)

(defvar *current-idl-prefix*)
(setf  *current-idl-prefix* (list nil))


(defvar *current-idl-line*)



;;  this is a specialized reader for the idl-compiler

(defun make-line-reader (stream)
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
  (let* ((token)
         (res)
         (line 0)
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


(defvar *LALR-DEBUG* nil)





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
