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

(defclass test-value-2-user (test-value-2)
  ())

(define-method "DEPTH" ((v test-value-2-user))
  (do ((count 1 (1+ count))
       (hare (op:left v) (op:left hare))
       (turtle v (if turtle-move (op:left turtle) turtle))
       (turtle-move nil (not turtle-move)))
      ((or (not (typep hare 'test-value-2))
           (eql hare turtle))
       (if hare (1+ count) count))))
#|
(op:depth (make-instance 'test-value-2-user 
            :left (make-instance 'test-value-2-user 
                    :left (make-instance 'test-value-1))))
(let ((tree (make-instance 'test-value-2-user 
            :left (make-instance 'test-value-2-user))))
  (setf (op:left (op:left tree)) tree)
  (op:depth tree))
|#


(define-value test-value-3
  :id "IDL:test/value3:1.0"
  :name "value3"
  :base_value test-value-1 
  :is_truncatable t
  :members (("next" (symbol-typecode 'test-value-3) 0)))


(define-value-box test-box-1 
  :id "IDL:test/box1:1.0"
  :name "box1"
  :version "1.0"
  :original_type CORBA:tc_long )

(define-value-box test-box-2
  :id "IDL:test/box2:1.0"
  :name "box2"
  :version "1.0"
  :original_type CORBA:tc_string
  :type string)


(define-test-suite "Value"
  (variables
   (tc1 (symbol-typecode 'test-value-1))
   (id1 (symbol-ifr-id 'test-value-1))
   (v1 (make-instance 'test-value-1 :name "v1"))
   (tc2 (symbol-typecode 'test-value-2))
   (id2 (symbol-ifr-id 'test-value-2))
   (v2 (make-instance 'test-value-2 :name "root" :left v1 :right nil))
   (v2b (make-instance 'test-value-2 :name "v2b" :left v1 :right v1))
   (buffer (get-work-buffer)))

  (define-test "simple"
    (marshal v1 (symbol-typecode 'test-value-1) buffer)
    (ensure-pattern* (unmarshal (symbol-typecode 'test-value-1) buffer) 
                     'identity (isa 'test-value-1)
                     'op:name (op:name v1)))
  (define-test "derived"
    ;; marshalling a derived valuetype for a base type
    (marshal v2 tc1 buffer)
    (ensure-pattern* (unmarshal tc1 buffer) 
                     'identity (isa 'test-value-2)
                     'op:name (op:name v2)
                     'op:left (isa 'test-value-1)
                     'op:right (isa 'null)))
  (define-test "Indirection unmarshal"
    (let (ifr-pos v-pos)
      (with-out-buffer (buffer)
        ;; first value
        (align 4)
        (setq v-pos pos)
        (marshal-long (logior #x7fffff00 2) buffer)
        (setq ifr-pos pos)
        (marshal-string (object-id v1) buffer)
        (marshal-string (op:name v1) buffer)
        ;; another value
        (marshal-long (logior #x7fffff00 2) buffer)
        (marshal-long -1 buffer) (marshal-long (- ifr-pos pos) buffer)      ; indirection to ifr id
        (marshal-string "2" buffer)
        ;; first value again
        (marshal-long -1 buffer) (marshal-long (- v-pos pos) buffer)))
    (let ((r1 (unmarshal tc1 buffer))
          (r2 (unmarshal tc1 buffer))
          (r3 (unmarshal tc1 buffer)))
      (ensure-eql r1 r3)
      (ensure-equalp (op:name r2) "2")))
  (define-test "sharing"
    (marshal v2b tc2 buffer)
    (let ((obj (unmarshal tc2 buffer)))
      (ensure-eql (op:left obj) (op:right obj))))
  (define-test "cyclic"
    (setf (op:left v2b) v2b)
    (marshal v2b tc2 buffer)
    (let ((obj (unmarshal tc2 buffer)))
      (ensure-eql (op:left obj) obj)))

  (define-test "ValueFactory"
    (let ((orb (CORBA:ORB_init)))
      (op:register_value_factory orb id2 'test-value-2-user))
    (marshal v2 tc1 buffer)
    (let ((obj (unmarshal tc1 buffer)))
      (ensure-typep obj 'test-value-2-user)
      (ensure-eql (op:depth obj) 2)))

  (define-test "boxed value"
    (let ((n1 (test-box-1 123))
          (n2 (test-box-1 99))
          (tcn (symbol-typecode 'test-box-1))
          (s1 "Hello World")
          (s2 "Foo")
          (tcs (symbol-typecode 'test-box-2)))
      (marshal n1 tcn buffer) (marshal n2 tcn buffer) (marshal n1 tcn buffer)
      (marshal s1 tcs buffer) (marshal s2 tcs buffer) (marshal s1 tcs buffer)
      (let ((r1 (unmarshal tcn buffer))
            (r2 (unmarshal tcn buffer))
            (r3 (unmarshal tcn buffer)))
        (ensure-eql (op:data r1) (op:data n1))
        (ensure-eql (op:data r2) (op:data n2))
        (ensure-eql r1 r3))
      (let ((r1 (unmarshal tcs buffer))
            (r2 (unmarshal tcs buffer))
            (r3 (unmarshal tcs buffer)))
        (ensure-equalp r1 s1)
        (ensure-equalp r2 s2)
        (ensure-equalp r1 r3))))

  (define-test "truncated"
    (let ((v3 (make-instance 'test-value-3 :name "foo" 
                             :next (make-instance 'test-value-3 :name "bar" 
                                                  :next nil)))
          (tc3 (symbol-typecode 'test-value-3)))
      (setf (gethash (op:id tc3) *ifr-id-symbol*) 'test-value-3)
      (marshal v3 tc1 buffer)
      (marshal v3 tc1 buffer)
      (remhash (op:id tc3) *ifr-id-symbol*)
      (let ((obj (unmarshal tc1 buffer)))
        (ensure-pattern* obj 'op:name "foo")
        (ensure-eql (unmarshal tc1 buffer) obj))))


;; 


#| end test suite |#)
