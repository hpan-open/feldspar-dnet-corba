(in-package :clorb)

(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:CONTAINED/DESCRIPTION
    :ID "IDL:omg.org/CORBA/Contained/Description:1.0"
    :MEMBERS ((KIND NIL)
              (VALUE NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:STRUCTMEMBER
    :ID "IDL:omg.org/CORBA/StructMember:1.0"
    :MEMBERS ((NAME NIL) (TYPE NIL) (TYPE_DEF NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:UNIONMEMBER
    :ID "IDL:omg.org/CORBA/UnionMember:1.0"
    :MEMBERS ((NAME NIL) (LABEL NIL) (TYPE NIL) (TYPE_DEF NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:CONTAINER/DESCRIPTION
    :ID "IDL:omg.org/CORBA/Container/Description:1.0"
    :MEMBERS ((CONTAINED_OBJECT NIL) (KIND NIL) (VALUE NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:MODULEDESCRIPTION
    :ID "IDL:omg.org/CORBA/ModuleDescription:1.0"
    :MEMBERS ((NAME NIL)
              (ID NIL)
              (DEFINED_IN NIL)
              (VERSION NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:CONSTANTDESCRIPTION
    :ID "IDL:omg.org/CORBA/ConstantDescription:1.0"
    :MEMBERS ((NAME NIL)
              (ID NIL)
              (DEFINED_IN NIL)
              (VERSION NIL)
              (TYPE NIL)
              (VALUE NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:TYPEDESCRIPTION
    :ID "IDL:omg.org/CORBA/TypeDescription:1.0"
    :MEMBERS ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:EXCEPTIONDESCRIPTION
    :ID "IDL:omg.org/CORBA/ExceptionDescription:1.0"
    :MEMBERS ((NAME NIL)
              (ID NIL)
              (DEFINED_IN NIL)
              (VERSION NIL)
              (TYPE NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:ATTRIBUTEDESCRIPTION
    :ID "IDL:omg.org/CORBA/AttributeDescription:1.0"
    :MEMBERS ((NAME NIL)
              (ID NIL)
              (DEFINED_IN NIL)
              (VERSION NIL)
              (TYPE NIL)
              (MODE NIL)))
;; defined in clorb-opdef.lisp
'(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:PARAMETERDESCRIPTION
    :ID "IDL:omg.org/CORBA/ParameterDescription:1.0"
    :MEMBERS ((NAME NIL) (TYPE NIL) (TYPE_DEF NIL) (MODE NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:OPERATIONDESCRIPTION
    :ID "IDL:omg.org/CORBA/OperationDescription:1.0"
    :MEMBERS ((NAME NIL)
              (ID NIL)
              (DEFINED_IN NIL)
              (VERSION NIL)
              (RESULT NIL)
              (MODE NIL)
              (CONTEXTS NIL)
              (PARAMETERS NIL)
              (EXCEPTIONS NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:INTERFACEDEF/FULLINTERFACEDESCRIPTION
    :ID "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0"
    :MEMBERS ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (OPERATIONS NIL)
              (ATTRIBUTES NIL) (BASE_INTERFACES NIL) (TYPE NIL)))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:INTERFACEDESCRIPTION
    :ID "IDL:omg.org/CORBA/InterfaceDescription:1.0"
    :MEMBERS ((NAME NIL)
              (ID NIL)
              (DEFINED_IN NIL)
              (VERSION NIL)
              (BASE_INTERFACES NIL)))
