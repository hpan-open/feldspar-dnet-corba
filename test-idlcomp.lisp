;;;; test-idlcomp.lisp

(in-package :clorb)

(define-test-suite "idlcomp"
  
  ;; ---------------------------------
  
  (define-idl-test "short"
    "typedef short s; typedef unsigned short us;"
    "s" (def-pattern :dk_alias  
          'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_short
                                  'op:type corba:tc_short))
    "us" (def-pattern :dk_alias  
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_ushort
                                   'op:type corba:tc_ushort)))
  
  (define-idl-test "long"
    "typedef long l; typedef unsigned long ul;"
    "l" (def-pattern :dk_alias 
          'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_long))
    "ul" (def-pattern :dk_alias 
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_ulong)))
  

  (define-idl-test "long long"
    "typedef unsigned long long ull; typedef long long ll; "
    "ll" (def-pattern :dk_alias
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_longlong))
    "ull" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_ulonglong)))

  
  (define-idl-test "string"
    "typedef string s0; typedef string<10> s10;
	typedef wstring ws0; typedef wstring<10> ws10; "
    "s0" (def-pattern :dk_alias
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_string))
    "s10" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_string 'op:bound 10))
    "ws0" (def-pattern :dk_alias
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_wstring))
    "ws10" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_wstring 'op:bound 10)))


  (define-idl-test "misc types"
    "	typedef octet o;
	typedef char c;
	typedef wchar wc;
	typedef boolean b;
	typedef Object i;
	typedef any a;
 "
    ;;
    "o" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_octet))
    "c" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_char))    
    "wc" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_wchar))
    "b" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_boolean))
    "i" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_objref))
    "a" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_any)))


  (define-idl-test "fixed point"
    "typedef fixed<10,2> fp;"
    "fp" (pattern 'op:original_type_def 
                  (def-pattern :dk_fixed 'op:digits 10 'op:scale 2)))


  (define-idl-test "float types"
    "
typedef float f;
typedef double d;
typedef long double ld;
"
    "f" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_float))
    "d" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_double))
    "ld" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_longdouble)))


  (define-idl-test "sequence"
    "typedef sequence<string> ubss;
  typedef sequence<string,20> bss;
  typedef sequence<bss> sss; "
    "ubss" (def-pattern :dk_alias
             'op:original_type_def (def-pattern :dk_sequence
                                     'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)
                                     'op:bound 0))
    "bss" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_sequence
                                    'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)
                                    'op:bound 20))
    "sss" (def-pattern :dk_alias
            'op:original_type_def
            (def-pattern :dk_sequence
              'op:element_type_def 
              (def-pattern :dk_alias
                'op:original_type_def (def-pattern :dk_sequence
                                        'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)
                                        'op:bound 20))
              'op:bound 0)))
  

  (define-idl-test "arrays"
    "
typedef long a[11];
typedef string aa[2][9];
"
    "a" (pattern 'op:original_type_def
                 (def-pattern :dk_array 'op:length 11
                   'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_long)))
    "aa" (pattern 'op:original_type_def
                  (def-pattern :dk_array 'op:length 2
                    'op:element_type_def 
                    (def-pattern :dk_array 'op:length 9
                      'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)))))


  (define-idl-test "const"
    "const long A = 1;
     const char B = 'C';
     const string S = \"wibbel\";
     const boolean D = TRUE; "
    ;;    const octet C = 1;    // not supported by orbit ?
    "A" (def-pattern :dk_constant 
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_long)
          'op:value (pattern 'any-typecode CORBA:tc_long
                             'any-value 1))
    
    "B" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_char)
          'op:value (pattern 'any-typecode CORBA:tc_char
                             'any-value #\C))
    
    "S" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_string)
          'op:value (pattern 'any-typecode omg.org/corba:tc_string
                             'any-value "wibbel"))
    
    #| "C" (def-pattern :dk_constant 
        'op:type_def (def-pattern :dk_primitive 'op:kind :pk_long)
        'op:value (pattern 'any-typecode CORBA:tc_octet
                           'any-value 1))|#  
    
    "D" (def-pattern :dk_constant 
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_boolean)
          'op:value (pattern 'any-typecode omg.org/corba:tc_boolean
                             'any-value t)))
  
  
  (define-idl-test "constant expressions"
    "
const long x = 123+4*9;
const long y = 1 << 8;
"
    "x" (pattern 'op:value (pattern 'any-value (+ 123 (* 4 9)))
                 'op:type CORBA:tc_long )
    "y" (pattern 'op:value (pattern 'any-value 256)))


  (define-idl-test "struct 1"
    "module Bar { struct foo { long x; long y; }; };"
    "Bar" (def-pattern :dk_module)
    "Bar::foo" (def-pattern :dk_struct
                 'op:name "foo"
                 'op:absolute_name "::Bar::foo"
                 'op:members (seq-pattern  
                              (struct-pattern 'struct-class-name 'omg.org/corba:structmember
                                              'op:name "x"
                                              'op:type CORBA:tc_long
                                              'op:type_def (def-pattern :dk_primitive
                                                             'op:kind :pk_long))
                              (struct-pattern 'struct-class-name 'omg.org/corba:structmember
                                              'op:name "y"
                                              'op:type CORBA:tc_long))))


  (define-idl-test "union 1"
    "union u switch(boolean) {
   case TRUE: long x;
   case FALSE: unsigned long y; };"
    "u" (def-pattern :dk_union
          'op:discriminator_type_def (def-pattern :dk_primitive 'op:kind :pk_boolean)
          'op:members (seq-pattern 
                       (struct-pattern 'op:name "x" 
                                       'op:label (pattern 'any-value t))
                       (struct-pattern 'op:name "y"
                                       'op:label (pattern 'any-value nil)
                                       'op:type CORBA:tc_ulong))))
  

  (define-idl-test "union 2"
    "const long y_tag = 1;
   union u switch(long) {
   case 0: long x;
   case y_tag: unsigned long y; 
   default: boolean flag; };"
    ;;
    "u" (def-pattern :dk_union
          'op:discriminator_type_def (def-pattern :dk_primitive 'op:kind :pk_long)
          'op:members (seq-pattern 
                       (struct-pattern 'op:name "x" 
                                       'op:label (pattern 'any-value 0
                                                          'any-typecode CORBA:tc_long))
                       (struct-pattern 'op:name "y"
                                       'op:label (pattern 'any-value 1)
                                       'op:type CORBA:tc_ulong)
                       (struct-pattern 'op:name "flag"
                                       'op:label (pattern 'any-typecode CORBA:tc_octet
                                                          'any-value 0)
                                       'op:type omg.org/corba:tc_boolean))))


  (define-idl-test "exception def"
    "exception exc { string msg; };"
    "exc" (def-pattern :dk_exception
            'op:id "IDL:exc:1.0"
            'op:type (pattern 'op:kind :tk_except 
                              'op:name "exc"
                              'op:id "IDL:exc:1.0"                            
                              'omg.org/features:member_count 1)
            'op:members (seq-pattern
                         (struct-pattern
                          'struct-class-name 'omg.org/corba:structmember
                          'op:name "msg"
                          'op:type CORBA:tc_string))))

  (define-idl-test "interface"
    "interface foo {
	readonly attribute string sa;
	exception ex {};
	foo maybe (in long n, out long rest);
	void check () raises (foo::ex);
	oneway void note (in string foo); };"
    "foo" (def-pattern :dk_interface
            'op:type (pattern 'op:kind :tk_objref 'op:name "foo"))
    "foo::sa" (def-pattern :dk_attribute
                'op:mode :attr_readonly
                'op:type CORBA:tc_string)
    "foo::maybe" (def-pattern :dk_operation
                   'op:mode :op_normal
                   'omg.org/features:result_def (def-pattern :dk_interface)
                   'op:params (seq-pattern
                               (struct-pattern 'op:name "n" 'op:mode :param_in)
                               (struct-pattern 'op:name "rest" 'op:mode :param_out))
                   'op:exceptions (seq-pattern))
    "foo::check" (def-pattern :dk_operation 
                   'op:exceptions (seq-pattern (def-pattern :dk_exception 'op:name "ex")))
    "foo::note" (def-pattern :dk_operation 'op:mode :op_oneway))

)

