
(IN-PACKAGE :CLORB) 
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/InterfaceDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/InterfaceDescription:1.0"
          "InterfaceDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC)
          "base_interfaces"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYIDSEQ))))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:INTERFACEDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/InterfaceDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (BASE_INTERFACES NIL)))
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDEF 'IFR-ID)
      "IDL:omg.org/CORBA/InterfaceDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/InterfaceDef:1.0"
        "InterfaceDef"))
(DEFCLASS OMG.ORG/CORBA:INTERFACEDEF (OMG.ORG/CORBA:CONTAINER
                                      OMG.ORG/CORBA:CONTAINED
                                      OMG.ORG/CORBA:IDLTYPE)
  NIL)
(DEFCLASS OMG.ORG/CORBA:INTERFACEDEF-PROXY (OMG.ORG/CORBA:INTERFACEDEF
                                            OMG.ORG/CORBA:CONTAINER-PROXY
                                            OMG.ORG/CORBA:CONTAINED-PROXY
                                            OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/InterfaceDef:1.0"
  'OMG.ORG/CORBA:INTERFACEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:CREATE_OPERATION ((OBJ
                                               OMG.ORG/CORBA:INTERFACEDEF-PROXY)
                                              &REST
                                              ARGS)
  (APPLY 'INVOKE OBJ "create_operation" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ATTRIBUTE ((OBJ
                                               OMG.ORG/CORBA:INTERFACEDEF-PROXY)
                                              &REST
                                              ARGS)
  (APPLY 'INVOKE OBJ "create_attribute" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:DESCRIBE_INTERFACE ((OBJ
                                                 OMG.ORG/CORBA:INTERFACEDEF-PROXY)
                                                &REST
                                                ARGS)
  (APPLY 'INVOKE OBJ "describe_interface" ARGS))
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDEF/FULLINTERFACEDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDEF/FULLINTERFACEDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0"
          "FullInterfaceDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC)
          "operations"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:OPDESCRIPTIONSEQ)
          "attributes"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:ATTRDESCRIPTIONSEQ)
          "base_interfaces"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYIDSEQ)
          "type"
          OMG.ORG/CORBA:TC_TYPECODE)))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:INTERFACEDEF/FULLINTERFACEDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (OPERATIONS NIL)
   (ATTRIBUTES NIL) (BASE_INTERFACES NIL) (TYPE NIL)))
(DEFMETHOD OMG.ORG/FEATURES:IS_A ((OBJ OMG.ORG/CORBA:INTERFACEDEF-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "is_a" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:BASE_INTERFACES ((OBJ
                                              OMG.ORG/CORBA:INTERFACEDEF-PROXY)
                                             &REST
                                             ARGS)
  (APPLY 'INVOKE OBJ "_get_base_interfaces" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BASE_INTERFACES) (NEWVAL
                                                    (OBJ
                                                     OMG.ORG/CORBA:INTERFACEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_base_interfaces" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:ATTRDESCRIPTIONSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/AttrDescriptionSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:ATTRDESCRIPTIONSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/AttrDescriptionSeq:1.0"
          "AttrDescriptionSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:ATTRIBUTEDESCRIPTION)
            0))))
(DEFTYPE OMG.ORG/CORBA:ATTRDESCRIPTIONSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:OPDESCRIPTIONSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/OpDescriptionSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:OPDESCRIPTIONSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/OpDescriptionSeq:1.0"
          "OpDescriptionSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:OPERATIONDESCRIPTION)
            0))))
(DEFTYPE OMG.ORG/CORBA:OPDESCRIPTIONSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:REPOSITORYIDSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/RepositoryIdSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:REPOSITORYIDSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/RepositoryIdSeq:1.0"
          "RepositoryIdSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
            0))))
(DEFTYPE OMG.ORG/CORBA:REPOSITORYIDSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:OPERATIONDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/OperationDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:OPERATIONDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/OperationDescription:1.0"
          "OperationDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC)
          "result"
          OMG.ORG/CORBA:TC_TYPECODE
          "mode"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:OPERATIONMODE)
          "contexts"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:CONTEXTIDSEQ)
          "parameters"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:PARDESCRIPTIONSEQ)
          "exceptions"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:EXCDESCRIPTIONSEQ))))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:OPERATIONDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/OperationDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (RESULT NIL) (MODE NIL)
   (CONTEXTS NIL) (PARAMETERS NIL) (EXCEPTIONS NIL)))
(SETF (GET 'OMG.ORG/CORBA:OPERATIONDEF 'IFR-ID)
      "IDL:omg.org/CORBA/OperationDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:OPERATIONDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/OperationDef:1.0"
        "OperationDef"))
(DEFCLASS OMG.ORG/CORBA:OPERATIONDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:OPERATIONDEF-PROXY (OMG.ORG/CORBA:OPERATIONDEF
                                            OMG.ORG/CORBA:CONTAINED-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/OperationDef:1.0"
  'OMG.ORG/CORBA:OPERATIONDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:EXCEPTIONS ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY)
                                        &REST
                                        ARGS)
  (APPLY 'INVOKE OBJ "_get_exceptions" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:EXCEPTIONS) (NEWVAL
                                               (OBJ
                                                OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_exceptions" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:CONTEXTS ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY)
                                      &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_contexts" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:CONTEXTS) (NEWVAL
                                             (OBJ
                                              OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_contexts" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:MODE ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "_get_mode" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MODE) (NEWVAL
                                         (OBJ
                                          OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_mode" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:PARAMS ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY)
                                    &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_params" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:PARAMS) (NEWVAL
                                           (OBJ
                                            OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_params" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:RESULT_DEF ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY)
                                        &REST
                                        ARGS)
  (APPLY 'INVOKE OBJ "_get_result_def" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:RESULT_DEF) (NEWVAL
                                               (OBJ
                                                OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_result_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:RESULT ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY)
                                    &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_result" ARGS))
(SETF (GET 'OMG.ORG/CORBA:EXCDESCRIPTIONSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/ExcDescriptionSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:EXCDESCRIPTIONSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/ExcDescriptionSeq:1.0"
          "ExcDescriptionSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:EXCEPTIONDESCRIPTION)
            0))))
(DEFTYPE OMG.ORG/CORBA:EXCDESCRIPTIONSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:EXCEPTIONDEFSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/ExceptionDefSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:EXCEPTIONDEFSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/ExceptionDefSeq:1.0"
          "ExceptionDefSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:EXCEPTIONDEF)
            0))))
(DEFTYPE OMG.ORG/CORBA:EXCEPTIONDEFSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:CONTEXTIDSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/ContextIdSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTEXTIDSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/ContextIdSeq:1.0"
          "ContextIdSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:CONTEXTIDENTIFIER)
            0))))
(DEFTYPE OMG.ORG/CORBA:CONTEXTIDSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:CONTEXTIDENTIFIER 'IFR-ID)
      "IDL:omg.org/CORBA/ContextIdentifier:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTEXTIDENTIFIER 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/ContextIdentifier:1.0"
          "ContextIdentifier"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER))))
(DEFTYPE OMG.ORG/CORBA:CONTEXTIDENTIFIER () 'OMG.ORG/CORBA:IDENTIFIER)
(SETF (GET 'OMG.ORG/CORBA:PARDESCRIPTIONSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/ParDescriptionSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:PARDESCRIPTIONSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"
          "ParDescriptionSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:PARAMETERDESCRIPTION)
            0))))
(DEFTYPE OMG.ORG/CORBA:PARDESCRIPTIONSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:PARAMETERDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/ParameterDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:PARAMETERDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/ParameterDescription:1.0"
          "ParameterDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "type"
          OMG.ORG/CORBA:TC_TYPECODE
          "type_def"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDLTYPE)
          "mode"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:PARAMETERMODE))))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:PARAMETERDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/ParameterDescription:1.0"
  :MEMBERS
  ((NAME NIL) (TYPE NIL) (TYPE_DEF NIL) (MODE NIL)))
(DEFTYPE OMG.ORG/CORBA:PARAMETERMODE ()
  '(MEMBER :PARAM_IN :PARAM_OUT :PARAM_INOUT))
(DEFTYPE OMG.ORG/CORBA:OPERATIONMODE () '(MEMBER :OP_NORMAL :OP_ONEWAY))
(SETF (GET 'OMG.ORG/CORBA:ATTRIBUTEDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/AttributeDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:ATTRIBUTEDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/AttributeDescription:1.0"
          "AttributeDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC)
          "type"
          OMG.ORG/CORBA:TC_TYPECODE
          "mode"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:ATTRIBUTEMODE))))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:ATTRIBUTEDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/AttributeDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL) (MODE NIL)))
(SETF (GET 'OMG.ORG/CORBA:ATTRIBUTEDEF 'IFR-ID)
      "IDL:omg.org/CORBA/AttributeDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:ATTRIBUTEDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/AttributeDef:1.0"
        "AttributeDef"))
(DEFCLASS OMG.ORG/CORBA:ATTRIBUTEDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY (OMG.ORG/CORBA:ATTRIBUTEDEF
                                            OMG.ORG/CORBA:CONTAINED-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/AttributeDef:1.0"
  'OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MODE ((OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "_get_mode" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MODE) (NEWVAL
                                         (OBJ
                                          OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_mode" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:TYPE_DEF ((OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY)
                                      &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_type_def" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:TYPE_DEF) (NEWVAL
                                             (OBJ
                                              OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:TYPE ((OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "_get_type" ARGS))
(DEFTYPE OMG.ORG/CORBA:ATTRIBUTEMODE () '(MEMBER :ATTR_NORMAL :ATTR_READONLY))
(SETF (GET 'OMG.ORG/CORBA:EXCEPTIONDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/ExceptionDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:EXCEPTIONDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/ExceptionDescription:1.0"
          "ExceptionDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC)
          "type"
          OMG.ORG/CORBA:TC_TYPECODE)))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:EXCEPTIONDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/ExceptionDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL)))
(SETF (GET 'OMG.ORG/CORBA:EXCEPTIONDEF 'IFR-ID)
      "IDL:omg.org/CORBA/ExceptionDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:EXCEPTIONDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/ExceptionDef:1.0"
        "ExceptionDef"))
(DEFCLASS OMG.ORG/CORBA:EXCEPTIONDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:EXCEPTIONDEF-PROXY (OMG.ORG/CORBA:EXCEPTIONDEF
                                            OMG.ORG/CORBA:CONTAINED-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/ExceptionDef:1.0"
  'OMG.ORG/CORBA:EXCEPTIONDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS ((OBJ OMG.ORG/CORBA:EXCEPTIONDEF-PROXY)
                                     &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_members" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS) (NEWVAL
                                            (OBJ
                                             OMG.ORG/CORBA:EXCEPTIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:TYPE ((OBJ OMG.ORG/CORBA:EXCEPTIONDEF-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "_get_type" ARGS))
(SETF (GET 'OMG.ORG/CORBA:ARRAYDEF 'IFR-ID) "IDL:omg.org/CORBA/ArrayDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:ARRAYDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/ArrayDef:1.0" "ArrayDef"))
(DEFCLASS OMG.ORG/CORBA:ARRAYDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:ARRAYDEF-PROXY (OMG.ORG/CORBA:ARRAYDEF
                                        OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/ArrayDef:1.0"
  'OMG.ORG/CORBA:ARRAYDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE_DEF ((OBJ
                                               OMG.ORG/CORBA:ARRAYDEF-PROXY)
                                              &REST
                                              ARGS)
  (APPLY 'INVOKE OBJ "_get_element_type_def" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ELEMENT_TYPE_DEF) (NEWVAL
                                                     (OBJ
                                                      OMG.ORG/CORBA:ARRAYDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_element_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE ((OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY)
                                          &REST
                                          ARGS)
  (APPLY 'INVOKE OBJ "_get_element_type" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LENGTH ((OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY) &REST
                                    ARGS)
  (APPLY 'INVOKE OBJ "_get_length" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:LENGTH) (NEWVAL
                                           (OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_length" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:SEQUENCEDEF 'IFR-ID)
      "IDL:omg.org/CORBA/SequenceDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:SEQUENCEDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/SequenceDef:1.0"
        "SequenceDef"))
(DEFCLASS OMG.ORG/CORBA:SEQUENCEDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:SEQUENCEDEF-PROXY (OMG.ORG/CORBA:SEQUENCEDEF
                                           OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/SequenceDef:1.0"
  'OMG.ORG/CORBA:SEQUENCEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE_DEF ((OBJ
                                               OMG.ORG/CORBA:SEQUENCEDEF-PROXY)
                                              &REST
                                              ARGS)
  (APPLY 'INVOKE OBJ "_get_element_type_def" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ELEMENT_TYPE_DEF) (NEWVAL
                                                     (OBJ
                                                      OMG.ORG/CORBA:SEQUENCEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_element_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE ((OBJ OMG.ORG/CORBA:SEQUENCEDEF-PROXY)
                                          &REST
                                          ARGS)
  (APPLY 'INVOKE OBJ "_get_element_type" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:BOUND ((OBJ OMG.ORG/CORBA:SEQUENCEDEF-PROXY) &REST
                                   ARGS)
  (APPLY 'INVOKE OBJ "_get_bound" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BOUND) (NEWVAL
                                          (OBJ
                                           OMG.ORG/CORBA:SEQUENCEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_bound" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:FIXEDDEF 'IFR-ID) "IDL:omg.org/CORBA/FixedDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:FIXEDDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/FixedDef:1.0" "FixedDef"))
(DEFCLASS OMG.ORG/CORBA:FIXEDDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:FIXEDDEF-PROXY (OMG.ORG/CORBA:FIXEDDEF
                                        OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/FixedDef:1.0"
  'OMG.ORG/CORBA:FIXEDDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:SCALE ((OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY) &REST
                                   ARGS)
  (APPLY 'INVOKE OBJ "_get_scale" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:SCALE) (NEWVAL
                                          (OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_scale" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:DIGITS ((OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY) &REST
                                    ARGS)
  (APPLY 'INVOKE OBJ "_get_digits" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:DIGITS) (NEWVAL
                                           (OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_digits" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:WSTRINGDEF 'IFR-ID)
      "IDL:omg.org/CORBA/WstringDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:WSTRINGDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/WstringDef:1.0"
        "WstringDef"))
(DEFCLASS OMG.ORG/CORBA:WSTRINGDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:WSTRINGDEF-PROXY (OMG.ORG/CORBA:WSTRINGDEF
                                          OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/WstringDef:1.0"
  'OMG.ORG/CORBA:WSTRINGDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:BOUND ((OBJ OMG.ORG/CORBA:WSTRINGDEF-PROXY) &REST
                                   ARGS)
  (APPLY 'INVOKE OBJ "_get_bound" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BOUND) (NEWVAL
                                          (OBJ OMG.ORG/CORBA:WSTRINGDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_bound" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:STRINGDEF 'IFR-ID) "IDL:omg.org/CORBA/StringDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:STRINGDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/StringDef:1.0" "StringDef"))
(DEFCLASS OMG.ORG/CORBA:STRINGDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:STRINGDEF-PROXY (OMG.ORG/CORBA:STRINGDEF
                                         OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/StringDef:1.0"
  'OMG.ORG/CORBA:STRINGDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:BOUND ((OBJ OMG.ORG/CORBA:STRINGDEF-PROXY) &REST
                                   ARGS)
  (APPLY 'INVOKE OBJ "_get_bound" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BOUND) (NEWVAL
                                          (OBJ OMG.ORG/CORBA:STRINGDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_bound" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:PRIMITIVEDEF 'IFR-ID)
      "IDL:omg.org/CORBA/PrimitiveDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:PRIMITIVEDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/PrimitiveDef:1.0"
        "PrimitiveDef"))
(DEFCLASS OMG.ORG/CORBA:PRIMITIVEDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:PRIMITIVEDEF-PROXY (OMG.ORG/CORBA:PRIMITIVEDEF
                                            OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/PrimitiveDef:1.0"
  'OMG.ORG/CORBA:PRIMITIVEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:KIND ((OBJ OMG.ORG/CORBA:PRIMITIVEDEF-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "_get_kind" ARGS))
(SETF (GET 'OMG.ORG/CORBA:ALIASDEF 'IFR-ID) "IDL:omg.org/CORBA/AliasDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:ALIASDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/AliasDef:1.0" "AliasDef"))
(DEFCLASS OMG.ORG/CORBA:ALIASDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:ALIASDEF-PROXY (OMG.ORG/CORBA:ALIASDEF
                                        OMG.ORG/CORBA:TYPEDEFDEF-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/AliasDef:1.0"
  'OMG.ORG/CORBA:ALIASDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:ORIGINAL_TYPE_DEF ((OBJ
                                                OMG.ORG/CORBA:ALIASDEF-PROXY)
                                               &REST
                                               ARGS)
  (APPLY 'INVOKE OBJ "_get_original_type_def" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ORIGINAL_TYPE_DEF) (NEWVAL
                                                      (OBJ
                                                       OMG.ORG/CORBA:ALIASDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_original_type_def" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:ENUMDEF 'IFR-ID) "IDL:omg.org/CORBA/EnumDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:ENUMDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/EnumDef:1.0" "EnumDef"))
(DEFCLASS OMG.ORG/CORBA:ENUMDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:ENUMDEF-PROXY (OMG.ORG/CORBA:ENUMDEF
                                       OMG.ORG/CORBA:TYPEDEFDEF-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/EnumDef:1.0"
  'OMG.ORG/CORBA:ENUMDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS ((OBJ OMG.ORG/CORBA:ENUMDEF-PROXY) &REST
                                     ARGS)
  (APPLY 'INVOKE OBJ "_get_members" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS) (NEWVAL
                                            (OBJ OMG.ORG/CORBA:ENUMDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:UNIONDEF 'IFR-ID) "IDL:omg.org/CORBA/UnionDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:UNIONDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/UnionDef:1.0" "UnionDef"))
(DEFCLASS OMG.ORG/CORBA:UNIONDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:UNIONDEF-PROXY (OMG.ORG/CORBA:UNIONDEF
                                        OMG.ORG/CORBA:TYPEDEFDEF-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/UnionDef:1.0"
  'OMG.ORG/CORBA:UNIONDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS ((OBJ OMG.ORG/CORBA:UNIONDEF-PROXY) &REST
                                     ARGS)
  (APPLY 'INVOKE OBJ "_get_members" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS) (NEWVAL
                                            (OBJ OMG.ORG/CORBA:UNIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:DISCRIMINATOR_TYPE_DEF ((OBJ
                                                     OMG.ORG/CORBA:UNIONDEF-PROXY)
                                                    &REST
                                                    ARGS)
  (APPLY 'INVOKE OBJ "_get_discriminator_type_def" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:DISCRIMINATOR_TYPE_DEF) (NEWVAL
                                                           (OBJ
                                                            OMG.ORG/CORBA:UNIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_discriminator_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:DISCRIMINATOR_TYPE ((OBJ
                                                 OMG.ORG/CORBA:UNIONDEF-PROXY)
                                                &REST
                                                ARGS)
  (APPLY 'INVOKE OBJ "_get_discriminator_type" ARGS))
(SETF (GET 'OMG.ORG/CORBA:STRUCTDEF 'IFR-ID) "IDL:omg.org/CORBA/StructDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:STRUCTDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/StructDef:1.0" "StructDef"))
(DEFCLASS OMG.ORG/CORBA:STRUCTDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:STRUCTDEF-PROXY (OMG.ORG/CORBA:STRUCTDEF
                                         OMG.ORG/CORBA:TYPEDEFDEF-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/StructDef:1.0"
  'OMG.ORG/CORBA:STRUCTDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS ((OBJ OMG.ORG/CORBA:STRUCTDEF-PROXY) &REST
                                     ARGS)
  (APPLY 'INVOKE OBJ "_get_members" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS) (NEWVAL
                                            (OBJ
                                             OMG.ORG/CORBA:STRUCTDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:TYPEDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/TypeDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:TYPEDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/TypeDescription:1.0"
          "TypeDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC)
          "type"
          OMG.ORG/CORBA:TC_TYPECODE)))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:TYPEDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/TypeDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL)))
(SETF (GET 'OMG.ORG/CORBA:TYPEDEFDEF 'IFR-ID)
      "IDL:omg.org/CORBA/TypedefDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:TYPEDEFDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/TypedefDef:1.0"
        "TypedefDef"))
(DEFCLASS OMG.ORG/CORBA:TYPEDEFDEF (OMG.ORG/CORBA:CONTAINED
                                    OMG.ORG/CORBA:IDLTYPE)
  NIL)
(DEFCLASS OMG.ORG/CORBA:TYPEDEFDEF-PROXY (OMG.ORG/CORBA:TYPEDEFDEF
                                          OMG.ORG/CORBA:CONTAINED-PROXY
                                          OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/TypedefDef:1.0"
  'OMG.ORG/CORBA:TYPEDEFDEF-PROXY)
(SETF (GET 'OMG.ORG/CORBA:CONSTANTDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/ConstantDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONSTANTDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/ConstantDescription:1.0"
          "ConstantDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC)
          "type"
          OMG.ORG/CORBA:TC_TYPECODE
          "value"
          OMG.ORG/CORBA:TC_ANY)))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:CONSTANTDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/ConstantDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL) (VALUE NIL)))
(SETF (GET 'OMG.ORG/CORBA:CONSTANTDEF 'IFR-ID)
      "IDL:omg.org/CORBA/ConstantDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONSTANTDEF 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/ConstantDef:1.0"
        "ConstantDef"))
(DEFCLASS OMG.ORG/CORBA:CONSTANTDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:CONSTANTDEF-PROXY (OMG.ORG/CORBA:CONSTANTDEF
                                           OMG.ORG/CORBA:CONTAINED-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/ConstantDef:1.0"
  'OMG.ORG/CORBA:CONSTANTDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:VALUE ((OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY) &REST
                                   ARGS)
  (APPLY 'INVOKE OBJ "_get_value" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:VALUE) (NEWVAL
                                          (OBJ
                                           OMG.ORG/CORBA:CONSTANTDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_value" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:TYPE_DEF ((OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY)
                                      &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_type_def" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:TYPE_DEF) (NEWVAL
                                             (OBJ
                                              OMG.ORG/CORBA:CONSTANTDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:TYPE ((OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "_get_type" ARGS))
(SETF (GET 'OMG.ORG/CORBA:MODULEDESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/ModuleDescription:1.0")
(SETF (GET 'OMG.ORG/CORBA:MODULEDESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/ModuleDescription:1.0"
          "ModuleDescription"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "id"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "defined_in"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:REPOSITORYID)
          "version"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:VERSIONSPEC))))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:MODULEDESCRIPTION
  :ID
  "IDL:omg.org/CORBA/ModuleDescription:1.0"
  :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL)))
(SETF (GET 'OMG.ORG/CORBA:MODULEDEF 'IFR-ID) "IDL:omg.org/CORBA/ModuleDef:1.0")
(SETF (GET 'OMG.ORG/CORBA:MODULEDEF 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/ModuleDef:1.0" "ModuleDef"))
(DEFCLASS OMG.ORG/CORBA:MODULEDEF (OMG.ORG/CORBA:CONTAINER
                                   OMG.ORG/CORBA:CONTAINED)
  NIL)
(DEFCLASS OMG.ORG/CORBA:MODULEDEF-PROXY (OMG.ORG/CORBA:MODULEDEF
                                         OMG.ORG/CORBA:CONTAINER-PROXY
                                         OMG.ORG/CORBA:CONTAINED-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/ModuleDef:1.0"
  'OMG.ORG/CORBA:MODULEDEF-PROXY)
(SETF (GET 'OMG.ORG/CORBA:REPOSITORY 'IFR-ID)
      "IDL:omg.org/CORBA/Repository:1.0")
(SETF (GET 'OMG.ORG/CORBA:REPOSITORY 'TYPECODE)
      (MAKE-TYPECODE
        :TK_OBJREF
        "IDL:omg.org/CORBA/Repository:1.0"
        "Repository"))
(DEFCLASS OMG.ORG/CORBA:REPOSITORY (OMG.ORG/CORBA:CONTAINER) NIL)
(DEFCLASS OMG.ORG/CORBA:REPOSITORY-PROXY (OMG.ORG/CORBA:REPOSITORY
                                          OMG.ORG/CORBA:CONTAINER-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/Repository:1.0"
  'OMG.ORG/CORBA:REPOSITORY-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:GET_ANONYMOUS_TYPES ((OBJ
                                                  OMG.ORG/CORBA:REPOSITORY-PROXY)
                                                 &REST
                                                 ARGS)
  (APPLY 'INVOKE OBJ "get_anonymous_types" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_FIXED ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY)
                                          &REST
                                          ARGS)
  (APPLY 'INVOKE OBJ "create_fixed" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ARRAY ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY)
                                          &REST
                                          ARGS)
  (APPLY 'INVOKE OBJ "create_array" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_SEQUENCE ((OBJ
                                              OMG.ORG/CORBA:REPOSITORY-PROXY)
                                             &REST
                                             ARGS)
  (APPLY 'INVOKE OBJ "create_sequence" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_WSTRING ((OBJ
                                             OMG.ORG/CORBA:REPOSITORY-PROXY)
                                            &REST
                                            ARGS)
  (APPLY 'INVOKE OBJ "create_wstring" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_STRING ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY)
                                           &REST
                                           ARGS)
  (APPLY 'INVOKE OBJ "create_string" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:GET_PRIMITIVE ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY)
                                           &REST
                                           ARGS)
  (APPLY 'INVOKE OBJ "get_primitive" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LOOKUP_ID ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY)
                                       &REST ARGS)
  (APPLY 'INVOKE OBJ "lookup_id" ARGS))
(SETF (GET 'OMG.ORG/CORBA:IDLTYPESEQ 'IFR-ID)
      "IDL:omg.org/CORBA/IDLTypeSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:IDLTYPESEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/IDLTypeSeq:1.0"
          "IDLTypeSeq"
          (MAKE-SEQUENCE-TYPECODE (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDLTYPE) 0))))
(DEFTYPE OMG.ORG/CORBA:IDLTYPESEQ () 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:PRIMITIVEKIND ()
  '(MEMBER :PK_NULL
           :PK_VOID
           :PK_SHORT
           :PK_LONG
           :PK_USHORT
           :PK_ULONG
           :PK_FLOAT
           :PK_DOUBLE
           :PK_BOOLEAN
           :PK_CHAR
           :PK_OCTET
           :PK_ANY
           :PK_TYPECODE
           :PK_PRINCIPAL
           :PK_STRING
           :PK_OBJREF
           :PK_LONGLONG
           :PK_ULONGLONG
           :PK_LONGDOUBLE
           :PK_WCHAR
           :PK_WSTRING))
(SETF (GET 'OMG.ORG/CORBA:IDLTYPE 'IFR-ID) "IDL:omg.org/CORBA/IDLType:1.0")
(SETF (GET 'OMG.ORG/CORBA:IDLTYPE 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/IDLType:1.0" "IDLType"))
(DEFCLASS OMG.ORG/CORBA:IDLTYPE (OMG.ORG/CORBA:IROBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:IDLTYPE-PROXY (OMG.ORG/CORBA:IDLTYPE
                                       OMG.ORG/CORBA:IROBJECT-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/IDLType:1.0"
  'OMG.ORG/CORBA:IDLTYPE-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:TYPE ((OBJ OMG.ORG/CORBA:IDLTYPE-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_type" ARGS))
(SETF (GET 'OMG.ORG/CORBA:CONTAINER 'IFR-ID) "IDL:omg.org/CORBA/Container:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTAINER 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/Container:1.0" "Container"))
(DEFCLASS OMG.ORG/CORBA:CONTAINER (OMG.ORG/CORBA:IROBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:CONTAINER-PROXY (OMG.ORG/CORBA:CONTAINER
                                         OMG.ORG/CORBA:IROBJECT-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/Container:1.0"
  'OMG.ORG/CORBA:CONTAINER-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:ADD_CONTAINED ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                           &REST
                                           ARGS)
  (APPLY 'INVOKE OBJ "add_contained" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:REMOVE_CONTAINED ((OBJ
                                               OMG.ORG/CORBA:CONTAINER-PROXY)
                                              &REST
                                              ARGS)
  (APPLY 'INVOKE OBJ "remove_contained" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LOCATE_NAME ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                         &REST
                                         ARGS)
  (APPLY 'INVOKE OBJ "locate_name" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LOCATE_ID ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                       &REST ARGS)
  (APPLY 'INVOKE OBJ "locate_id" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_INTERFACE ((OBJ
                                               OMG.ORG/CORBA:CONTAINER-PROXY)
                                              &REST
                                              ARGS)
  (APPLY 'INVOKE OBJ "create_interface" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ALIAS ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                          &REST
                                          ARGS)
  (APPLY 'INVOKE OBJ "create_alias" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ENUM ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                         &REST
                                         ARGS)
  (APPLY 'INVOKE OBJ "create_enum" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_UNION ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                          &REST
                                          ARGS)
  (APPLY 'INVOKE OBJ "create_union" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_EXCEPTION ((OBJ
                                               OMG.ORG/CORBA:CONTAINER-PROXY)
                                              &REST
                                              ARGS)
  (APPLY 'INVOKE OBJ "create_exception" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_STRUCT ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                           &REST
                                           ARGS)
  (APPLY 'INVOKE OBJ "create_struct" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_CONSTANT ((OBJ
                                              OMG.ORG/CORBA:CONTAINER-PROXY)
                                             &REST
                                             ARGS)
  (APPLY 'INVOKE OBJ "create_constant" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_MODULE ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                           &REST
                                           ARGS)
  (APPLY 'INVOKE OBJ "create_module" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:DESCRIBE_CONTENTS ((OBJ
                                                OMG.ORG/CORBA:CONTAINER-PROXY)
                                               &REST
                                               ARGS)
  (APPLY 'INVOKE OBJ "describe_contents" ARGS))
(SETF (GET 'OMG.ORG/CORBA:CONTAINER/DESCRIPTIONSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/Container/DescriptionSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTAINER/DESCRIPTIONSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/Container/DescriptionSeq:1.0"
          "DescriptionSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:CONTAINER/DESCRIPTION)
            0))))
(DEFTYPE OMG.ORG/CORBA:CONTAINER/DESCRIPTIONSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:CONTAINER/DESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/Container/Description:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTAINER/DESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/Container/Description:1.0"
          "Description"
          "contained_object"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:CONTAINED)
          "kind"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:DEFINITIONKIND)
          "value"
          OMG.ORG/CORBA:TC_ANY)))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:CONTAINER/DESCRIPTION
  :ID
  "IDL:omg.org/CORBA/Container/Description:1.0"
  :MEMBERS
  ((CONTAINED_OBJECT NIL) (KIND NIL) (VALUE NIL)))
(DEFMETHOD OMG.ORG/FEATURES:LOOKUP_NAME ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY)
                                         &REST
                                         ARGS)
  (APPLY 'INVOKE OBJ "lookup_name" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CONTENTS ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST
                                      ARGS)
  (APPLY 'INVOKE OBJ "contents" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LOOKUP ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST
                                    ARGS)
  (APPLY 'INVOKE OBJ "lookup" ARGS))
(SETF (GET 'OMG.ORG/CORBA:ENUMMEMBERSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/EnumMemberSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:ENUMMEMBERSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/EnumMemberSeq:1.0"
          "EnumMemberSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
            0))))
(DEFTYPE OMG.ORG/CORBA:ENUMMEMBERSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:UNIONMEMBERSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/UnionMemberSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:UNIONMEMBERSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/UnionMemberSeq:1.0"
          "UnionMemberSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:UNIONMEMBER)
            0))))
(DEFTYPE OMG.ORG/CORBA:UNIONMEMBERSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:UNIONMEMBER 'IFR-ID)
      "IDL:omg.org/CORBA/UnionMember:1.0")
(SETF (GET 'OMG.ORG/CORBA:UNIONMEMBER 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/UnionMember:1.0"
          "UnionMember"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "label"
          OMG.ORG/CORBA:TC_ANY
          "type"
          OMG.ORG/CORBA:TC_TYPECODE
          "type_def"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDLTYPE))))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:UNIONMEMBER
  :ID
  "IDL:omg.org/CORBA/UnionMember:1.0"
  :MEMBERS
  ((NAME NIL) (LABEL NIL) (TYPE NIL) (TYPE_DEF NIL)))
(SETF (GET 'OMG.ORG/CORBA:STRUCTMEMBERSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/StructMemberSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:STRUCTMEMBERSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/StructMemberSeq:1.0"
          "StructMemberSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:STRUCTMEMBER)
            0))))
(DEFTYPE OMG.ORG/CORBA:STRUCTMEMBERSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:STRUCTMEMBER 'IFR-ID)
      "IDL:omg.org/CORBA/StructMember:1.0")
(SETF (GET 'OMG.ORG/CORBA:STRUCTMEMBER 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/StructMember:1.0"
          "StructMember"
          "name"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDENTIFIER)
          "type"
          OMG.ORG/CORBA:TC_TYPECODE
          "type_def"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:IDLTYPE))))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:STRUCTMEMBER
  :ID
  "IDL:omg.org/CORBA/StructMember:1.0"
  :MEMBERS
  ((NAME NIL) (TYPE NIL) (TYPE_DEF NIL)))
(SETF (GET 'OMG.ORG/CORBA:CONTAINEDSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/ContainedSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTAINEDSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/ContainedSeq:1.0"
          "ContainedSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:CONTAINED)
            0))))
(DEFTYPE OMG.ORG/CORBA:CONTAINEDSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDEFSEQ 'IFR-ID)
      "IDL:omg.org/CORBA/InterfaceDefSeq:1.0")
(SETF (GET 'OMG.ORG/CORBA:INTERFACEDEFSEQ 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/InterfaceDefSeq:1.0"
          "InterfaceDefSeq"
          (MAKE-SEQUENCE-TYPECODE
            (SYMBOL-TYPECODE 'OMG.ORG/CORBA:INTERFACEDEF)
            0))))
(DEFTYPE OMG.ORG/CORBA:INTERFACEDEFSEQ () 'SEQUENCE)
(SETF (GET 'OMG.ORG/CORBA:CONTAINED 'IFR-ID) "IDL:omg.org/CORBA/Contained:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTAINED 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/Contained:1.0" "Contained"))
(DEFCLASS OMG.ORG/CORBA:CONTAINED (OMG.ORG/CORBA:IROBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:CONTAINED-PROXY (OMG.ORG/CORBA:CONTAINED
                                         OMG.ORG/CORBA:IROBJECT-PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/Contained:1.0"
  'OMG.ORG/CORBA:CONTAINED-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MOVE ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "move" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:DESCRIBE ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST
                                      ARGS)
  (APPLY 'INVOKE OBJ "describe" ARGS))
(SETF (GET 'OMG.ORG/CORBA:CONTAINED/DESCRIPTION 'IFR-ID)
      "IDL:omg.org/CORBA/Contained/Description:1.0")
(SETF (GET 'OMG.ORG/CORBA:CONTAINED/DESCRIPTION 'TYPECODE)
      (LAMBDA NIL
        (STRUCT-TYPECODE
          "IDL:omg.org/CORBA/Contained/Description:1.0"
          "Description"
          "kind"
          (SYMBOL-TYPECODE 'OMG.ORG/CORBA:DEFINITIONKIND)
          "value"
          OMG.ORG/CORBA:TC_ANY)))
(DEFINE-CORBA-STRUCT
  OMG.ORG/CORBA:CONTAINED/DESCRIPTION
  :ID
  "IDL:omg.org/CORBA/Contained/Description:1.0"
  :MEMBERS
  ((KIND NIL) (VALUE NIL)))
(DEFMETHOD OMG.ORG/FEATURES:CONTAINING_REPOSITORY ((OBJ
                                                    OMG.ORG/CORBA:CONTAINED-PROXY)
                                                   &REST
                                                   ARGS)
  (APPLY 'INVOKE OBJ "_get_containing_repository" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:ABSOLUTE_NAME ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY)
                                           &REST
                                           ARGS)
  (APPLY 'INVOKE OBJ "_get_absolute_name" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:DEFINED_IN ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY)
                                        &REST
                                        ARGS)
  (APPLY 'INVOKE OBJ "_get_defined_in" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:VERSION ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST
                                     ARGS)
  (APPLY 'INVOKE OBJ "_get_version" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:VERSION) (NEWVAL
                                            (OBJ
                                             OMG.ORG/CORBA:CONTAINED-PROXY))
  (APPLY 'INVOKE OBJ "_set_version" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:NAME ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST
                                  ARGS)
  (APPLY 'INVOKE OBJ "_get_name" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:NAME) (NEWVAL
                                         (OBJ OMG.ORG/CORBA:CONTAINED-PROXY))
  (APPLY 'INVOKE OBJ "_set_name" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:ID ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "_get_id" ARGS))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ID) (NEWVAL
                                       (OBJ OMG.ORG/CORBA:CONTAINED-PROXY))
  (APPLY 'INVOKE OBJ "_set_id" NEWVAL))
(SETF (GET 'OMG.ORG/CORBA:VERSIONSPEC 'IFR-ID)
      "IDL:omg.org/CORBA/VersionSpec:1.0")
(SETF (GET 'OMG.ORG/CORBA:VERSIONSPEC 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/VersionSpec:1.0"
          "VersionSpec"
          OMG.ORG/CORBA:TC_STRING)))
(DEFTYPE OMG.ORG/CORBA:VERSIONSPEC () 'OMG.ORG/CORBA:STRING)
(SETF (GET 'OMG.ORG/CORBA:IROBJECT 'IFR-ID) "IDL:omg.org/CORBA/IRObject:1.0")
(SETF (GET 'OMG.ORG/CORBA:IROBJECT 'TYPECODE)
      (MAKE-TYPECODE :TK_OBJREF "IDL:omg.org/CORBA/IRObject:1.0" "IRObject"))
(DEFCLASS OMG.ORG/CORBA:IROBJECT (OBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:IROBJECT-PROXY (OMG.ORG/CORBA:IROBJECT
                                        OMG.ORG/CORBA:PROXY)
  NIL)
(REGISTER-PROXY-CLASS
  "IDL:omg.org/CORBA/IRObject:1.0"
  'OMG.ORG/CORBA:IROBJECT-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:DESTROY ((OBJ OMG.ORG/CORBA:IROBJECT-PROXY) &REST
                                     ARGS)
  (APPLY 'INVOKE OBJ "destroy" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:DEF_KIND ((OBJ OMG.ORG/CORBA:IROBJECT-PROXY) &REST
                                      ARGS)
  (APPLY 'INVOKE OBJ "_get_def_kind" ARGS))
(DEFTYPE OMG.ORG/CORBA:DEFINITIONKIND ()
  '(MEMBER :DK_NONE
           :DK_ALL
           :DK_ATTRIBUTE
           :DK_CONSTANT
           :DK_EXCEPTION
           :DK_INTERFACE
           :DK_MODULE
           :DK_OPERATION
           :DK_TYPEDEF
           :DK_ALIAS
           :DK_STRUCT
           :DK_UNION
           :DK_ENUM
           :DK_PRIMITIVE
           :DK_STRING
           :DK_SEQUENCE
           :DK_ARRAY
           :DK_REPOSITORY
           :DK_WSTRING
           :DK_FIXED))
(SETF (GET 'OMG.ORG/CORBA:REPOSITORYID 'IFR-ID)
      "IDL:omg.org/CORBA/RepositoryId:1.0")
(SETF (GET 'OMG.ORG/CORBA:REPOSITORYID 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/RepositoryId:1.0"
          "RepositoryId"
          OMG.ORG/CORBA:TC_STRING)))
(DEFTYPE OMG.ORG/CORBA:REPOSITORYID () 'OMG.ORG/CORBA:STRING)
(SETF (GET 'OMG.ORG/CORBA:SCOPEDNAME 'IFR-ID)
      "IDL:omg.org/CORBA/ScopedName:1.0")
(SETF (GET 'OMG.ORG/CORBA:SCOPEDNAME 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/ScopedName:1.0"
          "ScopedName"
          OMG.ORG/CORBA:TC_STRING)))
(DEFTYPE OMG.ORG/CORBA:SCOPEDNAME () 'OMG.ORG/CORBA:STRING)
(SETF (GET 'OMG.ORG/CORBA:IDENTIFIER 'IFR-ID)
      "IDL:omg.org/CORBA/Identifier:1.0")
(SETF (GET 'OMG.ORG/CORBA:IDENTIFIER 'TYPECODE)
      (LAMBDA NIL
        (MAKE-TC-ALIAS
          "IDL:omg.org/CORBA/Identifier:1.0"
          "Identifier"
          OMG.ORG/CORBA:TC_STRING)))
(DEFTYPE OMG.ORG/CORBA:IDENTIFIER () 'OMG.ORG/CORBA:STRING)