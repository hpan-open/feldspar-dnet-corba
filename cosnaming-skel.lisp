
(IN-PACKAGE :CLORB)

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (ENSURE-CORBA-PACKAGE
    "COSNAMING"
    :EXPORT
    '("NAMINGCONTEXTEXT/INVALIDADDRESS" "NAMINGCONTEXTEXT-SERVANT"
      "NAMINGCONTEXTEXT" "NAMINGCONTEXT/INVALIDNAME"
      "NAMINGCONTEXT/CANNOTPROCEED" "NAMINGCONTEXT/ALREADYBOUND"
      "NAMINGCONTEXT/NOTFOUNDREASON" "NAMINGCONTEXT/NOTFOUND" "NAMECOMPONENT"
      "NAMINGCONTEXT/NOTEMPTY" "NAMINGCONTEXT-SERVANT" "NAMINGCONTEXT"
      "BINDING" "BINDINGITERATOR-SERVANT" "BINDINGITERATOR")))

(DEFINE-CORBA-CLASS
  COSNAMING:BINDINGITERATOR-SERVANT
  (COSNAMING:BINDINGITERATOR OMG.ORG/PORTABLESERVER:SERVANT)
  :ATTRIBUTES
  NIL)

(DEFMETHOD SERVANT-INVOKE ((SERVANT COSNAMING:BINDINGITERATOR-SERVANT)
                           OPERATION INPUT HANDLER)
  (DECLARE (IGNORABLE INPUT))
  (COND ((STRING= OPERATION "destroy")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND NIL
                           (OMG.ORG/FEATURES:DESTROY SERVANT)
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))))
        ((STRING= OPERATION "next_n")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT _BL)
                           (OMG.ORG/FEATURES:NEXT_N SERVANT
                                                    (UNMARSHAL-ULONG INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-BOOL RESULT OUTPUT)
                           (MARSHAL-SEQUENCE
                             _BL
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ 'COSNAMING:BINDING BUFFER))
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "next_one")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT _B)
                           (OMG.ORG/FEATURES:NEXT_ONE SERVANT)
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-BOOL RESULT OUTPUT)
                           (STRUCT-WRITE _B 'COSNAMING:BINDING OUTPUT)
                           OUTPUT))))
        (T (CALL-NEXT-METHOD))))

(DEFINE-CORBA-CLASS
  COSNAMING:NAMINGCONTEXT-SERVANT
  (COSNAMING:NAMINGCONTEXT OMG.ORG/PORTABLESERVER:SERVANT)
  :ATTRIBUTES
  NIL)

(DEFMETHOD SERVANT-INVOKE ((SERVANT COSNAMING:NAMINGCONTEXT-SERVANT) OPERATION
                           INPUT HANDLER)
  (DECLARE (IGNORABLE INPUT))
  (COND ((STRING= OPERATION "list")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (_BL _BI)
                           (OMG.ORG/FEATURES:LIST SERVANT
                                                  (UNMARSHAL-ULONG INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           (MARSHAL-SEQUENCE
                             _BL
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ 'COSNAMING:BINDING BUFFER))
                             OUTPUT)
                           (MARSHAL _BI
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:BINDINGITERATOR)
                                    OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "destroy")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND NIL
                           (OMG.ORG/FEATURES:DESTROY SERVANT)
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTEMPTY
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "bind_new_context")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT)
                           (OMG.ORG/FEATURES:BIND_NEW_CONTEXT
                             SERVANT
                             (UNMARSHAL-SEQUENCE
                               (LAMBDA (BUFFER)
                                 (STRUCT-READ 'COSNAMING:NAMECOMPONENT BUFFER))
                               INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "new_context")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT)
                           (OMG.ORG/FEATURES:NEW_CONTEXT SERVANT)
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "unbind")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND NIL
                           (OMG.ORG/FEATURES:UNBIND SERVANT
                                                    (UNMARSHAL-SEQUENCE
                                                     (LAMBDA
                                                      (BUFFER)
                                                      (STRUCT-READ
                                                       'COSNAMING:NAMECOMPONENT
                                                       BUFFER))
                                                     INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "resolve")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT)
                           (OMG.ORG/FEATURES:RESOLVE SERVANT
                                                     (UNMARSHAL-SEQUENCE
                                                      (LAMBDA
                                                       (BUFFER)
                                                       (STRUCT-READ
                                                        'COSNAMING:NAMECOMPONENT
                                                        BUFFER))
                                                      INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT OMG.ORG/CORBA:tc_object OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "rebind_context")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND NIL
                           (OMG.ORG/FEATURES:REBIND_CONTEXT
                             SERVANT
                             (UNMARSHAL-SEQUENCE
                               (LAMBDA (BUFFER)
                                 (STRUCT-READ 'COSNAMING:NAMECOMPONENT BUFFER))
                               INPUT)
                             (UNMARSHAL (SYMBOL-TYPECODE
                                         'COSNAMING:NAMINGCONTEXT)
                                        INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "bind_context")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND NIL
                           (OMG.ORG/FEATURES:BIND_CONTEXT SERVANT
                                                          (UNMARSHAL-SEQUENCE
                                                           (LAMBDA
                                                            (BUFFER)
                                                            (STRUCT-READ
                                                             'COSNAMING:NAMECOMPONENT
                                                             BUFFER))
                                                           INPUT)
                                                          (UNMARSHAL
                                                           (SYMBOL-TYPECODE
                                                            'COSNAMING:NAMINGCONTEXT)
                                                           INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "rebind")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND NIL
                           (OMG.ORG/FEATURES:REBIND SERVANT
                                                    (UNMARSHAL-SEQUENCE
                                                     (LAMBDA
                                                      (BUFFER)
                                                      (STRUCT-READ
                                                       'COSNAMING:NAMECOMPONENT
                                                       BUFFER))
                                                     INPUT)
                                                    (UNMARSHAL
                                                     OMG.ORG/CORBA:tc_object
                                                     INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "bind")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND NIL
                           (OMG.ORG/FEATURES:BIND SERVANT
                                                  (UNMARSHAL-SEQUENCE
                                                   (LAMBDA
                                                    (BUFFER)
                                                    (STRUCT-READ
                                                     'COSNAMING:NAMECOMPONENT
                                                     BUFFER))
                                                   INPUT)
                                                  (UNMARSHAL
                                                   OMG.ORG/CORBA:tc_object
                                                   INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))))
        (T (CALL-NEXT-METHOD))))

(DEFINE-CORBA-CLASS
  COSNAMING:NAMINGCONTEXTEXT-SERVANT
  (COSNAMING:NAMINGCONTEXTEXT COSNAMING:NAMINGCONTEXT-SERVANT)
  :ATTRIBUTES
  NIL)

(DEFMETHOD SERVANT-INVOKE ((SERVANT COSNAMING:NAMINGCONTEXTEXT-SERVANT)
                           OPERATION INPUT HANDLER)
  (DECLARE (IGNORABLE INPUT))
  (COND ((STRING= OPERATION "resolve_str")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT)
                           (OMG.ORG/FEATURES:RESOLVE_STR SERVANT
                                                         (UNMARSHAL-STRING
                                                          INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT OMG.ORG/CORBA:tc_object OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "to_url")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT)
                           (OMG.ORG/FEATURES:TO_URL SERVANT
                                                    (UNMARSHAL-STRING INPUT)
                                                    (UNMARSHAL-STRING INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-STRING RESULT OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXTEXT/INVALIDADDRESS
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContextExt/InvalidAddress:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "to_name")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT)
                           (OMG.ORG/FEATURES:TO_NAME SERVANT
                                                     (UNMARSHAL-STRING INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-SEQUENCE
                             RESULT
                             (LAMBDA (OBJ BUFFER)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((STRING= OPERATION "to_string")
         (HANDLER-CASE (MULTIPLE-VALUE-BIND (RESULT)
                           (OMG.ORG/FEATURES:TO_STRING SERVANT
                                                       (UNMARSHAL-SEQUENCE
                                                        (LAMBDA
                                                         (BUFFER)
                                                         (STRUCT-READ
                                                          'COSNAMING:NAMECOMPONENT
                                                          BUFFER))
                                                        INPUT))
                         (LET ((OUTPUT (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-STRING RESULT OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (DECLARE (IGNORABLE EXC))
                         (LET ((OUTPUT (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        (T (CALL-NEXT-METHOD))))
