(in-package :clorb)

(define-module "Streams_0" ()
  (define-interface "stream" ()
    (define-operation "put_string" ((:in "str" string)))
    (define-operation "put_strings" ((:in "strs" (sequence string))))
    (define-operation "put_long" ((:in "n" long)))
    (define-operation "put_any" ((:in "x" any))))
  (define-interface "str_stream" (:bases ("stream"))
    (define-operation "clear" ())
    (define-operation "contents" ()
      :result-type string)))

