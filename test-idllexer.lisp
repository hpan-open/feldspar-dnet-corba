(in-package :clorb)

(defmacro with-test-lex (string &body body)
  `(with-input-from-string (test-stream ,string)
     (let ((*lexer* (make-instance 'net.cddr.redpas:streamchar-lexer
                                   :stream test-stream)))
       (next-token *lexer*)
       (setq *failable* t)
       ,@body)))


(defmacro with-test-lex* ((var expect cases &key (lex-func 'idl-token))
                          &body body)
  (let ((string (gensym)) (flag (gensym)))
    `(loop for (,string ,expect) in ,cases do 
           (with-sub-test (,string)
             (with-test-lex ,string
               (multiple-value-bind (,flag ,var) (,lex-func)
                 (ensure ,flag "string parsed")
                 (ensure-eql (token *lexer*) :eof)
                 ,@body))))))
   

(define-test-suite "IDL Lexer"
  (variables)


  (define-test "integer literal"
    (with-test-lex* (val n '(("123" 123) ("0" 0)
                             ("0177" #o177)
                             ("0xAA" #xaa) ("0XAA" #xaa)
                             ("0xbb" #xbb)))
      (ensure-eql val n)))

  
  (define-test "character literal"
    (with-test-lex* (val code '(("'a'" #\a)
                                ("'\\''" #\')
                                ("'\\b'" 8)
                                ("'\\x7a'" #x7a)
                                ("'\\60'" #o60)
                                ("'\\0'" 0)))
      (typecase code
        (character (ensure-eql val code))
        (integer   (ensure-eql (char-code val) code)))))


  (define-test "float literal"
    (with-test-lex* (val expect '(("123e33" 123D33)
                                  ("12.3" 12.3D0)
                                  (".5"   0.5D0))
                         :lex-func number-token)
      (ensure-typep val 'double-float)
      (ensure-eql val expect)))
  

  (define-test "string literal"
    (with-test-lex* (val code '(("\"arb\"" "arb")
                                ("\"a\\\"b\"" "a\"b")
                                ("\"\\61a\"" "1a")
                                ("\"\\x7a\"" "z")
                                ("\"\\0617,\\x7ab\"" "17,zb")
                                ("\"\\x9\" \"a\"" "	a")))
      (ensure-equalp (cdr val) code)))


  (define-test "parse-fixed"
    (loop for (string expect) in '(("1.2" 12/10)
                                   (".3" 3/10)
                                   ("4" 4)
                                   ("5." 5))
          do (ensure-eql (parse-fixed string) expect)))

  (define-test "fixed literal"
    (with-test-lex* (val expect '(("1.2D" 12/10)
                                   (".3d" 3/10)
                                   ("4d" 4)
                                   ("5.D" 5)))
      (ensure-typep val 'rational)
      (ensure-eql val expect)))



#|
end
|#)
