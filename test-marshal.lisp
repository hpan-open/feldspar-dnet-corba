(in-package :clorb)

(define-test-suite "Marshal test"
  (macrolet ((marshal-res (res &rest body)
               `(let ((buffer (get-work-buffer)))
                  ,@body
                  (ensure-equalp (buffer-octets buffer) ,res))))
    
    (define-test "Ushort 1"
      (marshal-res #(88 1) (marshal-ushort 344 buffer) ))
    
    (define-test "short 2"
      ;; ushort??  short would use the exact same marshaling code ...
      ;; but
      (marshal-res #(168 254) (marshal-ushort -344 buffer)))
    
    (define-test "Float 1"
      (let ((test-data '((1.25d0 #16R3fa00000 #16R3ff4000000000000)
                         (12.5d0 #16R41480000 #16R4029000000000000)
                         (1.1d0 #16R3f8ccccd #16R3ff199999999999a)
                         (-1.125 #16Rbf900000 #16Rbff2000000000000)
                         (-1.1d0 #16Rbf8ccccd #16Rbff199999999999a)
                         (#.pi #16R40490fdb #16R400921fb54442d18)
                         (0.0d0 #16R00000000 #16R0000000000000000))))
        (flet ((integer-octets (integer bytes)
                 (let ((octets (make-array bytes :element-type 'CORBA:Octet)))
                   (loop for i below bytes do
                         (setf (aref octets i)
                               (ldb (byte 8 (* 8 i)) integer)))
                   octets)))
          (dolist (tuple test-data)
            (marshal-res (integer-octets (second tuple) 4)
                         (marshal (first tuple) CORBA:tc_float buffer))
            (marshal-res (integer-octets (third tuple) 8)
                         (marshal (first tuple) CORBA:tc_double buffer))))))
    
    (define-test "Float 2"
      (let ((test-data '(1.25 12.5 1.1 -1.125 -1.1 #.pi 0.0 ))
            (buffer (get-work-buffer)))
        (flet ((test-roundtrip (float type typecode)
                 (setq tc-current-sub type)
                 (setf (fill-pointer (buffer-octets buffer)) 0)
                 (marshal-octet 1 buffer)
                 (marshal float typecode buffer)
                 (setf (buffer-position buffer) 0)
                 (unmarshal-octet buffer)
                 (let ((result (unmarshal typecode buffer)))
                   (ensure-equalp result (coerce float type)))
                 (setq tc-current-sub nil)))
          (dolist (float test-data)
            (test-roundtrip (float float 1f0) 
                            'CORBA:float CORBA:tc_float)
            (test-roundtrip float 'CORBA:double CORBA:tc_double)
            (test-roundtrip float 'CORBA:longdouble CORBA:tc_longdouble)))))
    
    (define-test "encapsulation"
      (marshal-res 
       #(28 0 0 0                     ; encaps size
         1                            ; byte order
         0 0 0                        ; align
         5 0 0 0                      ; string size
         72 101 106 33 0              ; "Hej!" + NUL
         0 0 0                        ; align
         8 0 0 0                      ; encaps size
         1                            ; byte order
         22                           ; octet 22
         0 0                          ; align
         44 0 0 0                     ; ulong 44
         #||#)
       (marshal-add-encapsulation
        (lambda (buffer)
          (marshal-string "Hej!" buffer)
          (marshal-add-encapsulation 
           (lambda (b) 
             (marshal-octet 22 b)
             (marshal-ulong 44 b))
           buffer))
        buffer)))
    
    (define-test "Struct"
      ;; Generic struct
      (let* ((tc (create-struct-tc
                  "IDL:foo/foo/aZZ000:1.0" "TypeDescription"
                  (list (list "name" CORBA:tc_string)
                        (list "id" corba:tc_long))))
             (obj (make-struct tc :name "hello" :id 1234)))
        (let ((buffer (get-work-buffer)))
          (marshal obj tc buffer)
          (let ((new (unmarshal tc buffer)))
            (ensure-equalp (struct-get new :name) "hello")
            (ensure-equalp (struct-get new :id)   1234))))
      ;; registered struct
      (let* ((tc (symbol-typecode 'iop:servicecontext))
             (obj (iop:servicecontext :context_id 345
                                      :context_data #(0 1 2))))
        (let ((buffer (get-work-buffer)))
          (marshal obj tc buffer)
          (let ((new (unmarshal tc buffer)))
            (ensure-typep new 'iop:servicecontext)
            (ensure-equalp (op:context_id new) 345)))))

    (define-test "Typecodes"
      (let* ((buffer (get-work-buffer))
             (tc (symbol-typecode 'CORBA::ParameterDescription)))
        (marshal-typecode tc buffer)
        (ensure-equalp (unmarshal-ulong buffer) 15)
        (with-encapsulation buffer
          (ensure-equalp (unmarshal-string buffer)
                         "IDL:omg.org/CORBA/ParameterDescription:1.0")
          (ensure-equalp (unmarshal-string buffer)
                         "ParameterDescription")
          (ensure-equalp (unmarshal-ulong buffer) 4))
        
        (setf (buffer-position buffer) 0)
        (let ((new-tc (unmarshal-typecode buffer)))
          (ensure-equalp (typecode-kind tc)
                         (typecode-kind new-tc))
          (ensure-equalp
           (map 'list #'car (tcp-members (typecode-params tc))) 
           (map 'list #'car (tcp-members (typecode-params new-tc)))))))
    
    (define-test "Typecode Union"
      (let* ((buffer (get-work-buffer))
             (members (list (list t "on" corba:tc_long)
                            (list nil "off" corba:tc_string)))
             (tc (create-union-tc "IDL:Two:1.0" "Two" corba:tc_boolean members)))
        (marshal tc omg.org/corba:tc_typecode buffer)
        (let ((tc2 (unmarshal omg.org/corba:tc_typecode buffer)))
          (ensure-eql (op:member_count tc2) (op:member_count tc))
          (ensure (op:equal tc2 tc)))))
    
    
    (define-test "RecursiveTypecode"
      (let* ((struct-filter
              (create-struct-tc "IDL:LDAP/Filter:1.0"
                                "Filter"
                                `(("and" ,corba:tc_null))))
             (buffer
              (make-buffer :octets (make-array 400 :fill-pointer 0
                                               :element-type 'CORBA:octet))))
        (setf (second (elt (tc-members struct-filter) 0)) struct-filter)
        (marshal struct-filter CORBA:tc_TypeCode buffer)
        (let ((tc (unmarshal CORBA:tc_TypeCode buffer)))
          (ensure-equalp (op:id struct-filter) (op:id tc))
          (ensure-equalp tc (op:member_type tc 0)))))
    
    t))

