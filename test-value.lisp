(in-package :clorb)


(define-value test-value-1
  :id "IDL:test/value1:1.0"
  :name "value1"
  :members (("name" CORBA:tc_string 0)))

(define-value test-value-2
  :id "IDL:test/value2:1.0"
  :name "value2"
  :base_value test-value-1
  :members (("left" (symbol-typecode 'test-value-2) 0)
            ("right" (symbol-typecode 'test-value-2) 0)))


(define-test-suite "Value"
  (variables
   (v1 (make-instance 'test-value-1 :name "v1"))
   (v2 (make-instance 'test-value-2 :name "root" :left v1 :right nil))
   (v2b (make-instance 'test-value-2 :name "v2b" :left v1 :right v1))
   (buffer (get-work-buffer)))

  (define-test "simple"
    (marshal v1 (symbol-typecode 'test-value-1) buffer)
    (let ((obj (unmarshal (symbol-typecode 'test-value-1) buffer)))
      (ensure-pattern* obj 
                       'identity (isa 'test-value-1)
                       'op:name (op:name v1))))
  (define-test "derived"
    ;; marshalling a derived valuetype for a base type
    (marshal v2 (symbol-typecode 'test-value-1) buffer)
    (ensure-pattern* (unmarshal (symbol-typecode 'test-value-1) buffer) 
                     'identity (isa 'test-value-2)
                     'op:name (op:name v2)
                     'op:left (isa 'test-value-1)
                     'op:right (isa 'null)))
  
  




#| end test suite |#)