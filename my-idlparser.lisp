(in-package :clorb)

(defvar *the-repository* )

(defun named-create (container create-op name &rest args)
  (apply create-op container (repo-id name *container*) name "1.0"
         args)) 

(defun repo-id (name container &optional (version "1.0"))
  (apply #'concatenate 'string
         "IDL:"
         `(,@(if *idef-current-prefix*
               (list *idef-current-prefix* "/"))
           ,@(nreverse (repo-path container))
           ,name ":" ,version)))

(defmacro parse-list (form &optional sep allow-empty)
  (let ((elem (gensym))
        (list (gensym)))
    `(let (,elem ,list)
       (seq (,(if allow-empty 'opt 'seq)
             (seq (-> ,form ,elem)
                  (action (push ,elem ,list))
                  (repeat (0)
                    (seq ,@(if sep (list sep))
                         (-> ,form ,elem)
                         (action (push ,elem ,list))))))
            (action (nreverse ,list))))))


;;;; Top Level

(defun <specification> ()
  (seq (repeat (1) (<definition>))
       :eof ))

(defun <definition> nil
 (alt (seq (<type_dcl>) ";")
      (seq (<const_dcl>) ";")
      (seq (<except_dcl>) ";")
      (seq (<interface>) ";")
      (seq (<module>) ";")))


;;;; Names

(defun <scoped_name> ()
  (let ((start nil)
        (names nil))
    (seq (opt (-> "::" start))
         (-> (parse-list (<identifier>) "::") names)
         (action (format nil "~:[~;::~]~{~A~^::~}" start names)))))

(defun <scoped_name>-lookup ()
  (let (name)
    (seq (-> (<scoped_name>) name)
         (action (or (op:lookup *container* name)
                     (error "Undefined name: ~A" name))))))



;;;; Module

(defun <module> (&aux name)
  (seq "module" (-> (<identifier>) name) "{" 
       (let ((*container*
              (or (op:lookup *container* name)
                  (named-create *container* #'op:create_module name))))
         (repeat (1) (<definition>)))
       "}")) 


;;;; Interface

(defun <interface> nil
  (let (name bases obj)
    (seq (alt (alt "abstract" "local") (seq))
         "interface"
         (-> (<identifier>) name)
         (action (setq obj (op:lookup *container* name))
                 (unless obj
                   (setq obj (named-create *container* #'op:create_interface name nil ))))
         (opt (seq (opt (-> (<interface_inheritance_spec>) bases))
                   "{"
                   (action (setf (op:base_interfaces obj) bases))
                   (let ((*container* obj))
                     (<interface_body>))
                   "}")))))

(defun <interface_body> nil
 (repeat (0) (<export>)))

(defun <interface_inheritance_spec> nil
  (seq ":" 
       (parse-list (<scoped_name>-lookup) ",")))

(defun <export> nil
 (alt (seq (<type_dcl>) ";")
      (seq (<const_dcl>) ";")
      (seq (<except_dcl>) ";")
      (seq (<attr_dcl>) ";")
      (seq (<op_dcl>) ";")))


;;;; Operation Declaration

(defun <op_dcl> nil
  (let (name result mode params exceptions contexts)
    (seq (-> (<op_attribute>) mode)
         (-> (<op_type_spec>) result)
         (-> (<identifier>) name)
         (-> (<parameter_dcls>) params)
         (opt (-> (<raises_expr>) exceptions))
         (opt (-> (<context_expr>) contexts))
         (action
           (named-create *container* #'op:create_operation name
                         result mode params exceptions contexts )))))

(defun <op_attribute> nil
  (alt (seq "oneway"  (action :op_oneway))
       (seq (action :op_normal))))

(defun <op_type_spec> nil
 (alt (seq (<param_type_spec>))
      (seq "void" (action (op:get_primitive *the-repository* :pk_void)))))

(defun <parameter_dcls> nil
 (let (list)
   (seq "("
        (-> (parse-list (<param_dcl>) "," t) list)
        ")"
        (action list))))

(defun <param_dcl> nil
  (let (name type-def mode)
    (seq (-> (<param_attribute>) mode)
         (-> (<param_type_spec>) type-def)
         (-> (<simple_declarator>) name)
         (action (CORBA:ParameterDescription 
                  :name name
                  :type_def type-def
                  :type CORBA:tc_void
                  :mode mode )))))

(defun <param_attribute> nil
  (alt (seq "in" (action :param_in))
       (seq "out" (action :param_out))
       (seq "inout" (action :param_inout))))

(defun <param_type_spec> nil
  ;; FIXME: any restrictions ?
  (<simple_type_spec>))


(defun <raises_expr> nil
  (let (list)
    (seq "raises" "(" 
         (-> (parse-list (<scoped_name>-lookup) "," t) list)
         ")"
         (action list))))

(defun <context_expr> nil
  (let (list)
    (seq "context" "(" 
         (-> (parse-list (<string_literal>) "," t) list)
         ")"
         (action list))))



;;;; Attribute Declaration

(defun <attr_dcl> nil
  (let (mode type dlist)
    (seq (-> (alt (seq "readonly" (action :attr_readonly))
                  (seq		(action :attr_normal)))
             mode)
         "attribute"
         (-> (<param_type_spec>) type)
         (-> (parse-list (<simple_declarator>) ",")
             dlist)
         (action (loop for name in dlist
                       do (named-create *container* #'op:create_attribute name
                                        type mode))))))



;;;; Exception

(defun <except_dcl> (&aux name members)
  (seq "exception" (-> (<identifier>) name)
       "{" 
       (-> (parse-list (<member>) nil t) members)
       "}"
       (action 
         (setq members (apply #'nconc members))
         (named-create *container* #'op:create_exception name
                       members))))



;;;; Type Declarations

(defun <type_dcl> nil
 (alt (seq "typedef" (<type_declarator>))
      (seq (<struct_type>))
      (seq (<union_type>))
      (seq (<enum_type>))
      (seq "native" (<simple_declarator>))))


(defun <type_declarator> nil
  (let (type decl)
    (seq (-> (<type_spec>) type)
         (-> (<declarators>) decl)
         (action 
           (loop for (name . array-spec) in decl
                 do (named-create *container* #'op:create_alias 
                                  name (convert-to-array type array-spec)))))))


(defun <declarators> nil
  (parse-list (<declarator>) ","))

(defun <declarator> nil
  (let (name array n)
    (seq (-> (<identifier>) name)
         (opt (repeat (1) (seq "[" (-> (<positive_int_const>) n) "]"
                               (action (push n array)))))
         (action (cons name (nreverse array))))))


(defun <simple_declarator> nil
 (<identifier>))




;;;; Struct

(defun <struct_type> nil
  (let (name members)
    (seq "struct" (-> (<identifier>) name)
         "{"  (-> (parse-list (<member>)) members)  "}"
         (action (named-create *container* #'op:create_struct name
                               (apply #'nconc members))))))

(defun <member> (&aux type dlist)
  (seq (-> (<type_spec>) type)
       (-> (<declarators>) dlist)
       ";"
       (action (loop for (name . array-spec) in dlist
                     collect (corba:StructMember 
                              :name name
                              :type_def (convert-to-array type array-spec)
                              :type CORBA:tc_void )))))


;;;; Union 

(defun <union_type> nil
  (let (name discriminator_type members)
    (seq "union" (-> (<identifier>) name)
         "switch" "(" (-> (<switch_type_spec>) discriminator_type) ")"
         "{" (-> (<switch_body> (op:type discriminator_type)) members) "}"
         (action (named-create *container* #'op:create_union name
                               discriminator_type members )))))


(defun <switch_type_spec> nil
  (<simple_type_spec> :allow-kind '(:tk_short :tk_ushort :tk_long :tk_ulong :tk_longlong :tk_ulonglong
                                    :tk_boolean :tk_char :tk_enum )))

(defun <switch_body> (disc-type)
  (let (list)
    (seq (-> (parse-list (<case> disc-type)) list)
         (action (apply #'nconc list)))))

(defun <case> (disc-type)
  (let (labels element)
    (seq (-> (parse-list (<case_label>)) labels)
         (-> (<element_spec>) element)
         ";"
         (action (loop for label in labels
                       collect (CORBA:UnionMember 
                                :name (car element)
                                :label (if (eq label 'default)
                                         (CORBA:Any :any-value 0
                                                    :any-typecode CORBA:tc_octet)
                                         (CORBA:Any :any-value label
                                                    :any-typecode disc-type))
                                :type_def (cdr element)))))))

(defun <element_spec> nil
  (let (name type)
    (seq (-> (<type_spec>) type)
         (-> (<declarator>) name)
         (action (cons (car name)
                       (convert-to-array type (cdr name)))))))

(defun <case_label> nil
  (let ((value 'default))
    (seq (alt (seq "case" (-> (<const_exp>) value) ":")
              (seq "default" ":"))
         (action value))))



;;;; Enum

(defun <enum_type> nil
  (let (name members)
    (seq "enum"
         (-> (<identifier>) name)
         "{" (-> (parse-list (<identifier>) ",") members) "}"
         (action
           (let ((enum (named-create *container* #'op:create_enum name members)))
             ;; introduce the members as constants in current scope
             (loop for cname in members 
                   do (named-create *container* #'op:create_constant cname enum 
                                    (corba:any :any-value (key cname)
                                               :any-typecode (op:type enum)))))))))



;;;; Litterals

(defun <literal> nil
 (alt (seq (<integer_literal>))
      (seq (<string_literal>))
      (seq (<boolean_literal>))
      (seq (<character_literal>))
      ;; not implemented yet:
      (seq (<wide_string_literal>))
      (seq (<wide_character_literal>))
      (seq (<fixed_pt_literal>))
      (seq (<floating_pt_literal>))
      ))

(defun <boolean_literal> nil
  (alt (seq "TRUE" 	(action t))
       (seq "FALSE"	(action nil))))



;;;; Constant Declaration

(defun <const_dcl> (&aux type name value)
  (seq "const" 
       (-> (<const_type>) type)
       (-> (<identifier>) name)
       "=" 
       (-> (<const_exp>) value)
       (action (named-create *container* #'op:create_constant name
                             type 
                             (corba:any :any-typecode (op:type type)
                                        :any-value value)))))

(defun <const_type> ()
  (alt
   (seq "fixed")                    ; FIXME: ???
   (<simple_type_spec> :disallow-kind '(:tk_struct :tk_union))))



;;;; Expressions

(defun <const_exp> nil
 (<or_expr>))

(defun <positive_int_const> nil
 (<const_exp>))

(defun <primary_expr> (&aux value)
  (alt (<literal>) 
       (let (obj)
         (seq (-> (<scoped_name>-lookup) obj)
              (action (check-type obj CORBA:ConstantDef)
                      (any-value (op:value obj)))))
       (seq "(" (-> (<const_exp>) value) ")" (action value))))

(defun <or_expr> nil
  (let (n1 n2)
    (seq (-> (<xor_expr>) n1)
         (repeat (0) (seq "|" (-> (<xor_expr>) n2)
                          (action (setq n1 (logior n1 n2)))))
         (action n1))))

(defun <xor_expr> nil
  (let (x y)
    (seq (-> (<and_expr>) x)
         (repeat (0) (seq "^" (-> (<and_expr>) y) (action (setq x (logxor x y)))))
         (action x))))

(defun <and_expr> nil
  (let (x y)
    (seq (-> (<shift_expr>) x)
         (repeat (0) (seq "&" (-> (<shift_expr>) y) (action (setq x (logand x y)))))
         (action x))))

(defun <shift_expr> nil
  (let (x y)
    (seq (-> (<add_expr>) x)
         (repeat (0) 
           (alt (seq ">>" (-> (<add_expr>) y) (action (setq x (>> x y))))
                (seq "<<" (-> (<add_expr>) y) (action (setq x (<< x y))))))
         (action x))))

(defun <add_expr> nil
  (let (x y)
    (seq (-> (<mult_expr>) x)
         (repeat (0) (alt (seq "+" (-> (<mult_expr>) y) (action (setq x (+ x y))))
                          (seq "-" (-> (<mult_expr>) y) (action (setq x (- x y))))))
         (action x))))

(defun <mult_expr> nil
  (let (x y)
    (seq (-> (<unary_expr>) x)
         (repeat (0)
           (alt (seq "*" (-> (<unary_expr>) y) (action (setq x (* x y))))
                (seq "/" (-> (<unary_expr>) y) (action (setq x (/ x y))))
                (seq "%" (-> (<unary_expr>) y) (action (setq x (rem x y))))))
         (action x))))

(defun <unary_expr> nil
  (let (n op)
    (alt (seq (-> (<unary_operator>) op) (-> (<primary_expr>) n)
              (action (cond ((equal op "-") (- n))
                            ((equal op "~") (lognot n))
                            (t n))))
         (seq (<primary_expr>)))))

(defun <unary_operator> nil
  (alt (seq "-") (seq "+") (seq "~")))



;;;; Type Specifications

(defun <type_spec> nil
 (alt (seq (<simple_type_spec>)) 
      (seq (<constr_type_spec>))))

(defun <simple_type_spec> (&key allow-kind disallow-kind)
  (let (type-def)
    (seq
     (-> (alt (seq (<base_type_spec>))
              (seq (<sequence_type>))
              (seq (<string_type>))
              (seq (<wide_string_type>))
              (seq (<fixed_pt_type>))
              (<scoped_name>-lookup)) 
         type-def)
     (action
       (check-type type-def CORBA:IDLType)
       (when allow-kind
         (unless (member (op:kind (op:type type-def)) allow-kind)
           (warn "Type ~A not allowed, allowed: ~S" type-def allow-kind)))
       (when disallow-kind
         (when (member (op:kind (op:type type-def)) disallow-kind)
           (warn "Type ~A not allowed, forbidden types: ~S" type-def disallow-kind)))
       type-def))))

(defun <integer_type> nil
 (<number_type>))


(defun <sequence_type> (&aux element-type bound)
  (seq "sequence"
       "<"
       (-> (<simple_type_spec>) element-type)
       (opt (seq "," (-> (<positive_int_const>) bound)))
       ">"
       (action (op:create_sequence *the-repository* (or bound 0) element-type))))

(defun <string_type> nil
  (seq "string" 
       (alt (let (len)
              (seq "<" (-> (<positive_int_const>) len) ">"
                   (action (op:create_string *the-repository* len))))
            (seq (action (op:get_primitive *the-repository* :pk_string))))))

(defun <wide_string_type> nil
  (seq "wstring"
       (alt (let (len)
              (seq "<" (-> (<positive_int_const>) len) ">" 
                   (action (op:create_wstring *the-repository* len))))
            (seq (action (op:get_primitive *the-repository* :pk_wstring))))))

(defun <fixed_pt_type> (&aux digits scale)
  (seq "fixed" "<" 
       (-> (<positive_int_const>) digits) "," (-> (<positive_int_const>) scale)
       ">"
       (action (op:create_fixed *the-repository* digits scale))))



(defun <number_type> (&aux pk)
  (seq (-> (alt (seq "float"			(action :pk_float))
                (seq "double"			(action :pk_double))
                (seq "short"			(action :pk_short))
                (seq "long" (alt (seq "double"	(action :pk_longdouble))
                                 (seq "long" 	(action :pk_longlong)) 
                                 (seq 		(action :pk_long) )))
                (seq "unsigned"
                     (alt (seq "short" 		(action :pk_ushort)) 
                          (seq "long"
                               (alt (seq "long" (action :pk_ulonglong)) 
                                    (seq	(action :pk_ulong)))))))
           pk)
       (action (op:get_primitive *the-repository* pk))))

(defun <base_type_spec> nil
  (alt (<number_type>)
       (misc-type)
       (seq "any"	(action (op:get_primitive *the-repository* :pk_any)))
       (seq "Object"	(action (op:get_primitive *the-repository* :pk_objref)))))

(defun misc-type (&aux pk)
  (seq (-> (alt (seq "char"	(action :pk_char))
                (seq "wchar"	(action :pk_wchar))
                (seq "octet"	(action :pk_octet))
                (seq "boolean"	(action :pk_boolean))) pk)
       (action (op:get_primitive *the-repository* pk))))



(defun <constr_type_spec> nil
  (alt (seq (<struct_type>)) (seq (<union_type>)) (seq (<enum_type>))))



;;;; primitive tokens

(defparameter *reserved-words*
  '("abstract" "double" "local" "raises" "typedef" "any" "exception"
    "long" "readonly" "unsigned" "attribute" "enum" "module" "sequence" "union" "boolean" "factory"
    "native" "short" "ValueBase" "case" "FALSE" "Object" "string" "valuetype" "char" "fixed" "octet"
    "struct" "void" "const" "float" "oneway" "supports" "wchar" "context" "in" "out" "switch"
    "wstring" "custom" "inout" "private" "TRUE" "default" "interface" "public" "truncatable" ) )


(defun <IDENTIFIER> ()
  ;; FIXME: an identifier can start with _ (should also be removed)
  (let ((tok (token *lexer*)))
    (when (and (stringp tok)
               (alpha-char-p (char tok 0))
               (not (member tok *reserved-words* :test #'string=)))
      (match-token *lexer* tok))))


(defun <INTEGER_LITERAL> ()
  (seq #'numberp))

(defun <STRING_LITERAL> ()
  (let (s)
    (seq (-> #'(lambda (tok) (and (consp tok) (eq (car tok) 'string))) s)
         (action (cdr s)))))

(defun <WIDE_STRING_LITERAL> ()
  ;; FIXME: check how this works
  nil)

(defun <CHARACTER_LITERAL> ()
  (seq #'characterp))

(defun <WIDE_CHARACTER_LITERAL> ()
  nil)

(defun <FIXED_PT_LITERAL> ()
  nil)

(defun <FLOATING_PT_LITERAL> ()
  nil)


(defclass my-idlparser (idl-compiler)
  ())

(defmethod load-repository ((self my-idlparser) repository file)
  (with-open-file (stream file :direction :input)
    (let* ((*the-repository* repository)
           (*container* *the-repository*))
      (let ((*lexer* (make-idllex stream)))
        (next-token *lexer*)
        (<specification>)))))

(setq *default-idl-compiler* (make-instance 'my-idlparser))


#|
(with-input-from-string (s "TRUE 
   }; ")
  (let* ((*the-repository* (make-instance 'repository))
         (*container* *the-repository*))
    (let ((*lexer* (make-idllex s)))
      (next-token *lexer*)
      (<const_exp>)
   )))
|#