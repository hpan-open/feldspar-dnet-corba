(in-package :clorb)


(defclass IDLLEX (lexer)
  ((lexer
    :initarg :lexer
    :accessor idllex-lexer)))

(defclass cppchar-lexer (lexer)
  ((stream
    :initarg :cpp
    :accessor lexer-cpp)
   (line :initform "")
   (pos  :initform 1)))


(defmethod next-token ((lexer cppchar-lexer))
  (with-slots (stream line pos) lexer
    (do ((token nil))
        (token (setf (slot-value lexer 'token) token))
      (cond ((= pos (length line))
             (incf pos)
             (setf token #\Newline))
            ((< pos (length line))
             (setf token (char line pos))
             (incf pos))
            (t
             (setf line (read-cpp-line stream))
             (setf pos 0)
             (if (null line)
               (setf token :eof)))))))

(defvar *current-cpp*)

(defun using-idllex (file thunk include-directories &optional defines)
  (check-type file (or string pathname))
  (using-cpp-stream
   file
   (lambda (cpp) 
     (let ((*current-cpp* cpp)
           (lexer (make-instance 'cppchar-lexer :cpp cpp)))
       (next-token lexer)
       (let ((*lexer* (make-instance 'idllex :lexer lexer)))
         (next-token *lexer*)
         (funcall thunk))))
   :include-directories include-directories
   :defines defines))


(defmethod next-token ((lexer idllex))
  (let ((*lexer* (idllex-lexer lexer)))
    (loop until
      (multiple-value-bind (flag token) (idl-token)
        (assert flag)
        (setf (slot-value lexer 'token) token)))))

(defun idl-token ()
  (whitespace)
  (alt (seq :eof (action :eof))
       (idl-comment)
       (identifier-token)
       (string-literal)
       (number-token)
       (char-literal)
       (char-token)))

(defun whitespace ()
  (loop
    with newline = nil
    and skip-line = nil
    for token = (token *lexer*)
    do (cond ((or (eql token #\Newline)
                  (eql token #\Linefeed))
              (setq newline t
                    skip-line nil))
             (skip-line)
             ((and newline (eql token #\#))
              (setq skip-line t))
             ((eql token #\Space))
             ((eql token #\Tab))
             (t (loop-finish)))
    (next-token *lexer*)))


(defun idl-comment ()
  (seq #\/
       (alt (seq #\*
                 (repeat (0) #'(lambda (ch) (char/= ch #\*)))
                 #\*
                 (repeat (0) 
                   #'(lambda (ch) (char/= ch #\/))
                   (repeat (0) #'(lambda (ch) (char/= ch #\*)))
                   #\*)
                 #\/
                 (action nil))
            (seq #\/ 
                 (seq* #'(lambda (ch) (char/= ch #\Newline)))
                 (action nil))
            (seq (action "/")))))


(defun ident-start-char-p (c)
  (or (alpha-char-p c)
      (char= c #\_)))

(defun ident-char-p (c)
  (or (alphanumericp c)
      (char= c #\_)))

(defun identifier-token ()
  (let (ch name)
    (seq (-> #'ident-start-char-p ch)
         (action (push ch name))
         (repeat (0)
           (-> #'ident-char-p ch)
           (action (push ch name)))
         (action (coerce (nreverse name) 'string)))))


(defun string-literal ()
  (let (ch chs)
    (seq #\"
         (repeat (0)
           (-> #'(lambda (c) (char/= c #\")) ch)
           (push ch chs))
         #\"
         (action (cons 'string (coerce (nreverse chs) 'string))))))

(defun char-literal ()
  (let (ch)
    (seq #\' (-> #'identity ch) #\'
         (action ch))))

(defun number-token ()
  (let ((chars '()))
    (macrolet ((c (&rest seq)
                 (let ((f '#:f) (c '#:c))
                   `(multiple-value-bind (,f ,c) (seq ,@seq)
                      (when ,f
                        (push ,c chars)
                        t)))))
      (seq (opt (c #\-))
           (repeat (1)
             (c #'digit-char-p))
           (opt (seq (c #\.)
                     (repeat (0)
                       (c #'digit-char-p))))
           (opt (seq (c (alt #\e #\E #\d #\D))
                     (repeat (1)
                       (c #'digit-char-p))))
           (action (read-from-string (coerce (nreverse chars) 'string)))))))

(defun char-token ()
  (alt (seq #\: (alt (seq #\: (action "::"))
                     (seq     (action ":"))))
       (seq #\< (alt (seq #\< (action "<<"))
                     (seq     (action "<"))))
       (seq #\> (alt (seq #\> (action ">>"))
                     (seq     (action ">"))))
       (let (c)
         (seq (-> #'identity c)
              (action (string c))))))



#|
(with-input-from-string (s "123.44 foo, ")
  (let ((*lexer* (make-instance 'net.cddr.redpas:streamchar-lexer
                   :stream s)))
    (next-token *lexer*)
    (number-token)
    (idl-token)
    (idl-token)))
|#