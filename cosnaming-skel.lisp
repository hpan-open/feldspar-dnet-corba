
(in-package :clorb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ENSURE-CORBA-PACKAGE
    "COSNAMING"
    :export
    '("NAMINGCONTEXTEXT/INVALIDADDRESS" "NAMINGCONTEXTEXT-SERVANT"
      "NAMINGCONTEXTEXT" "NAMINGCONTEXT/INVALIDNAME"
      "NAMINGCONTEXT/CANNOTPROCEED" "NAMINGCONTEXT/ALREADYBOUND"
      "NAMINGCONTEXT/NOTFOUNDREASON" "NAMINGCONTEXT/NOTFOUND" "NAMECOMPONENT"
      "NAMINGCONTEXT/NOTEMPTY" "NAMINGCONTEXT-SERVANT" "NAMINGCONTEXT"
      "BINDING" "BINDINGITERATOR-SERVANT" "BINDINGITERATOR")))

(DEFINE-CORBA-CLASS
  COSNAMING:BINDINGITERATOR-SERVANT
  (COSNAMING:BINDINGITERATOR OMG.ORG/PORTABLESERVER:SERVANT)
  :attributes
  NIL)

(defmethod servant-invoke ((servant cosnaming:bindingiterator-servant)
                           operation input handler)
  (declare (ignorable INPUT))
  (cond ((string= OPERATION "destroy")
         (handler-case (multiple-value-bind NIL
                           (OMG.ORG/FEATURES:DESTROY SERVANT)
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))))
        ((string= OPERATION "next_n")
         (handler-case (multiple-value-bind (RESULT _BL)
                           (OMG.ORG/FEATURES:NEXT_N SERVANT
                                                    (UNMARSHAL-ULONG INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-BOOL RESULT OUTPUT)
                           (MARSHAL-SEQUENCE
                             _BL
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ 'COSNAMING:BINDING BUFFER))
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "next_one")
         (handler-case (multiple-value-bind (RESULT _B)
                           (OMG.ORG/FEATURES:NEXT_ONE SERVANT)
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-BOOL RESULT OUTPUT)
                           (STRUCT-WRITE _B 'COSNAMING:BINDING OUTPUT)
                           OUTPUT))))
        (t (call-next-method))))

(DEFINE-CORBA-CLASS
  COSNAMING:NAMINGCONTEXT-SERVANT
  (COSNAMING:NAMINGCONTEXT OMG.ORG/PORTABLESERVER:SERVANT)
  :attributes
  NIL)

(defmethod servant-invoke ((servant cosnaming:namingcontext-servant) operation
                           input handler)
  (declare (ignorable INPUT))
  (cond ((string= OPERATION "list")
         (handler-case (multiple-value-bind (_BL _BI)
                           (OMG.ORG/FEATURES:LIST SERVANT
                                                  (UNMARSHAL-ULONG INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           (MARSHAL-SEQUENCE
                             _BL
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ 'COSNAMING:BINDING BUFFER))
                             OUTPUT)
                           (MARSHAL _BI
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:BINDINGITERATOR)
                                    OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "destroy")
         (handler-case (multiple-value-bind NIL
                           (OMG.ORG/FEATURES:DESTROY SERVANT)
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTEMPTY
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "bind_new_context")
         (handler-case (multiple-value-bind (RESULT)
                           (OMG.ORG/FEATURES:BIND_NEW_CONTEXT
                             SERVANT
                             (UNMARSHAL-SEQUENCE
                               (lambda (buffer)
                                 (STRUCT-READ 'COSNAMING:NAMECOMPONENT BUFFER))
                               INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "new_context")
         (handler-case (multiple-value-bind (RESULT)
                           (OMG.ORG/FEATURES:NEW_CONTEXT SERVANT)
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "unbind")
         (handler-case (multiple-value-bind NIL
                           (OMG.ORG/FEATURES:UNBIND SERVANT
                                                    (UNMARSHAL-SEQUENCE
                                                     (lambda
                                                      (buffer)
                                                      (STRUCT-READ
                                                       'COSNAMING:NAMECOMPONENT
                                                       BUFFER))
                                                     INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "resolve")
         (handler-case (multiple-value-bind (RESULT)
                           (OMG.ORG/FEATURES:RESOLVE SERVANT
                                                     (UNMARSHAL-SEQUENCE
                                                      (lambda
                                                       (buffer)
                                                       (STRUCT-READ
                                                        'COSNAMING:NAMECOMPONENT
                                                        BUFFER))
                                                      INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT OMG.ORG/CORBA:tc_object OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "rebind_context")
         (handler-case (multiple-value-bind NIL
                           (OMG.ORG/FEATURES:REBIND_CONTEXT
                             SERVANT
                             (UNMARSHAL-SEQUENCE
                               (lambda (buffer)
                                 (STRUCT-READ 'COSNAMING:NAMECOMPONENT BUFFER))
                               INPUT)
                             (UNMARSHAL (SYMBOL-TYPECODE
                                         'COSNAMING:NAMINGCONTEXT)
                                        INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "bind_context")
         (handler-case (multiple-value-bind NIL
                           (OMG.ORG/FEATURES:BIND_CONTEXT SERVANT
                                                          (UNMARSHAL-SEQUENCE
                                                           (lambda
                                                            (buffer)
                                                            (STRUCT-READ
                                                             'COSNAMING:NAMECOMPONENT
                                                             BUFFER))
                                                           INPUT)
                                                          (UNMARSHAL
                                                           (SYMBOL-TYPECODE
                                                            'COSNAMING:NAMINGCONTEXT)
                                                           INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "rebind")
         (handler-case (multiple-value-bind NIL
                           (OMG.ORG/FEATURES:REBIND SERVANT
                                                    (UNMARSHAL-SEQUENCE
                                                     (lambda
                                                      (buffer)
                                                      (STRUCT-READ
                                                       'COSNAMING:NAMECOMPONENT
                                                       BUFFER))
                                                     INPUT)
                                                    (UNMARSHAL
                                                     OMG.ORG/CORBA:tc_object
                                                     INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "bind")
         (handler-case (multiple-value-bind NIL
                           (OMG.ORG/FEATURES:BIND SERVANT
                                                  (UNMARSHAL-SEQUENCE
                                                   (lambda
                                                    (buffer)
                                                    (STRUCT-READ
                                                     'COSNAMING:NAMECOMPONENT
                                                     BUFFER))
                                                   INPUT)
                                                  (UNMARSHAL
                                                   OMG.ORG/CORBA:tc_object
                                                   INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           NIL
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))))
        (t (call-next-method))))

(DEFINE-CORBA-CLASS
  COSNAMING:NAMINGCONTEXTEXT-SERVANT
  (COSNAMING:NAMINGCONTEXTEXT COSNAMING:NAMINGCONTEXT-SERVANT)
  :attributes
  NIL)

(defmethod servant-invoke ((servant cosnaming:namingcontextext-servant)
                           operation input handler)
  (declare (ignorable INPUT))
  (cond ((string= OPERATION "resolve_str")
         (handler-case (multiple-value-bind (RESULT)
                           (OMG.ORG/FEATURES:RESOLVE_STR SERVANT
                                                         (UNMARSHAL-STRING
                                                          INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL RESULT OMG.ORG/CORBA:tc_object OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/NOTFOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:WHY EXC)
                                    (SYMBOL-TYPECODE
                                      'COSNAMING:NAMINGCONTEXT/NOTFOUNDREASON)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/CANNOTPROCEED
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                             OUTPUT)
                           (MARSHAL (OMG.ORG/FEATURES:CXT EXC)
                                    (SYMBOL-TYPECODE 'COSNAMING:NAMINGCONTEXT)
                                    OUTPUT)
                           (MARSHAL-SEQUENCE
                             (OMG.ORG/FEATURES:REST_OF_NAME EXC)
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/ALREADYBOUND
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "to_url")
         (handler-case (multiple-value-bind (RESULT)
                           (OMG.ORG/FEATURES:TO_URL SERVANT
                                                    (UNMARSHAL-STRING INPUT)
                                                    (UNMARSHAL-STRING INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-STRING RESULT OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXTEXT/INVALIDADDRESS
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContextExt/InvalidAddress:1.0"
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "to_name")
         (handler-case (multiple-value-bind (RESULT)
                           (OMG.ORG/FEATURES:TO_NAME SERVANT
                                                     (UNMARSHAL-STRING INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-SEQUENCE
                             RESULT
                             (lambda (obj buffer)
                               (STRUCT-WRITE OBJ
                                             'COSNAMING:NAMECOMPONENT
                                             BUFFER))
                             OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        ((string= OPERATION "to_string")
         (handler-case (multiple-value-bind (RESULT)
                           (OMG.ORG/FEATURES:TO_STRING SERVANT
                                                       (UNMARSHAL-SEQUENCE
                                                        (lambda
                                                         (buffer)
                                                         (STRUCT-READ
                                                          'COSNAMING:NAMECOMPONENT
                                                          BUFFER))
                                                        INPUT))
                         (let ((output (GET-NORMAL-RESPONSE HANDLER)))
                           (MARSHAL-STRING RESULT OUTPUT)
                           OUTPUT))
                       (COSNAMING:NAMINGCONTEXT/INVALIDNAME
                         (EXC)
                         (declare (ignorable EXC))
                         (let ((output (GET-EXCEPTION-RESPONSE HANDLER)))
                           (MARSHAL-STRING
                             "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                             OUTPUT)
                           OUTPUT))))
        (t (call-next-method))))
