(in-package :clorb)

(define-test-suite "Test Internalize"
    (let ((r0 (make-instance 'repository)))
      
      (idef-read '((define-module "Hello" ()
                     (define-interface "World" ()
                       (define-operation "greet" ()
                         :result-type string))))
                 r0)
      
      (define-test "Test hello"
          (let ((r (make-instance 'irepository)))
            (internalize r r0)
            (setq *r r)
            (let ((h (lookup-name-in r "Hello::World")))
              (ensure h)
              (ensure (not (eq h (lookup-name-in r0 "Hello::World"))))
              (ensure (eq (op:defined_in h) (lookup-name-in r "Hello"))
                      "World defined_in Hello")
              (ensure-equalp (length (contents h)) 1))))))
