;;;; 

(in-package :clorb)


(define-test-suite "my-idlparser"
  
  (define-idl-test "native"
    "native servant;"
    "servant" (def-pattern :dk_native 'op:name "servant"))
  
  (define-idl-test "Local Interface"
    "local interface Foo {
       void load (in string name);
     };"
    "Foo" (def-pattern :dk_localinterface )
    "Foo::load" (def-pattern :dk_operation))

  (define-idl-test "Abstract Interface"
    "abstract interface Foo;
     abstract interface Foo {
       void load (in string name);
     };"
    "Foo" (def-pattern :dk_abstractinterface)
    "Foo::load" (def-pattern :dk_operation))

  (define-idl-test "ValueBox"
    "valuetype Sevol long;"
    "Sevol" (def-pattern :dk_valuebox
              'op:original_type_def (def-pattern :dk_primitive
                                      'op:type CORBA:tc_long)))

  (define-idl-test "valuetype"
    "valuetype Foo {public long n; void inc();
       factory init (in string x);};"
    "Foo" (def-pattern :dk_value
            'op:name "Foo" 'op:id "IDL:Foo:1.0"
            'op:is_abstract nil
            'op:is_custom nil
            'op:initializers 
            (sequence-pattern
             (struct-pattern 'struct-class-name 'CORBA::Initializer
                             'op:name "init"
                             'op:members
                             (sequence-pattern
                              (struct-pattern 'op:name "x" 'op:type omg.org/corba:tc_string)))))
    "Foo::n" (def-pattern :dk_valuemember
               'op:access omg.org/corba:public_member
               'op:type CORBA:tc_long)
    "Foo::inc" (def-pattern :dk_operation))


  (define-idl-test "valuetype2"
    "
abstract valuetype ax {void op1();};
valuetype cx {private string name;};
valuetype bx : cx, ax {};
"
    "ax" (def-pattern :dk_value 
           'op:is_abstract t
           'contents (sequence-pattern
                      (def-pattern :dk_operation)))
    "bx" (def-pattern :dk_value 
           'op:is_abstract nil
           'op:base_value (pattern 'op:name "cx")
           'op:abstract_base_values (sequence-pattern
                                     (pattern 'op:name "ax")))
    "bx::name" (def-pattern :dk_valuemember
                 'op:access omg.org/corba:private_member)
)


  #|end|# ) ; end test suite

