(in-package :clorb)
(DEFTYPE OMG.ORG/CORBA:IDENTIFIER NIL 'OMG.ORG/CORBA:STRING)
(DEFTYPE OMG.ORG/CORBA:SCOPEDNAME NIL 'OMG.ORG/CORBA:STRING)
(DEFTYPE OMG.ORG/CORBA:REPOSITORYID NIL 'OMG.ORG/CORBA:STRING)
(DEFTYPE OMG.ORG/CORBA:DEFINITIONKIND NIL
  '(MEMBER :DK_NONE :DK_ALL :DK_ATTRIBUTE :DK_CONSTANT :DK_EXCEPTION
     :DK_INTERFACE :DK_MODULE :DK_OPERATION :DK_TYPEDEF :DK_ALIAS :DK_STRUCT
     :DK_UNION :DK_ENUM :DK_PRIMITIVE :DK_STRING :DK_SEQUENCE :DK_ARRAY
     :DK_REPOSITORY :DK_WSTRING :DK_FIXED))
(DEFCLASS OMG.ORG/CORBA:IROBJECT (OBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:IROBJECT-PROXY
  (OMG.ORG/CORBA:IROBJECT OMG.ORG/CORBA:PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/IRObject:1.0"
  'OMG.ORG/CORBA:IROBJECT-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:DEF_KIND ((OBJ OMG.ORG/CORBA:IROBJECT-PROXY) &REST ARGS)
 (APPLY 'INVOKE OBJ "_get_def_kind"))
(DEFMETHOD OMG.ORG/FEATURES:DESTROY
  ((OBJ OMG.ORG/CORBA:IROBJECT-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "destroy" ARGS))
(DEFTYPE OMG.ORG/CORBA:VERSIONSPEC NIL 'OMG.ORG/CORBA:STRING)
(DEFCLASS OMG.ORG/CORBA:CONTAINED (OMG.ORG/CORBA:IROBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:CONTAINED-PROXY
  (OMG.ORG/CORBA:CONTAINED OMG.ORG/CORBA:IROBJECT-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/Contained:1.0"
  'OMG.ORG/CORBA:CONTAINED-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:ID ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST args)
  (APPLY 'INVOKE OBJ "_get_id"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ID)
  (NEWVAL (OBJ OMG.ORG/CORBA:CONTAINED-PROXY))
  (APPLY 'INVOKE OBJ "_set_id" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:NAME ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST args)
  (APPLY 'INVOKE OBJ "_get_name"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:NAME)
  (NEWVAL (OBJ OMG.ORG/CORBA:CONTAINED-PROXY))
  (APPLY 'INVOKE OBJ "_set_name" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:VERSION
  ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST args)
  (APPLY 'INVOKE OBJ "_get_version"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:VERSION)
  (NEWVAL (OBJ OMG.ORG/CORBA:CONTAINED-PROXY))
  (APPLY 'INVOKE OBJ "_set_version" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:DEFINED_IN
  ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_defined_in"))
(DEFMETHOD OMG.ORG/FEATURES:ABSOLUTE_NAME
  ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_absolute_name"))
(DEFMETHOD OMG.ORG/FEATURES:CONTAINING_REPOSITORY
  ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_containing_repository"))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:CONTAINED/DESCRIPTION :ID
  "IDL:omg.org/CORBA/Contained/Description:1.0" :MEMBERS
  ((KIND NIL) (VALUE NIL)))
(DEFMETHOD OMG.ORG/FEATURES:DESCRIBE
  ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "describe" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:MOVE
  ((OBJ OMG.ORG/CORBA:CONTAINED-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "move" ARGS))
(DEFTYPE OMG.ORG/CORBA:INTERFACEDEFSEQ NIL 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:CONTAINEDSEQ NIL 'SEQUENCE)
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:STRUCTMEMBER :ID
  "IDL:omg.org/CORBA/StructMember:1.0" :MEMBERS
  ((NAME NIL) (TYPE NIL) (TYPE_DEF NIL)))
(DEFTYPE OMG.ORG/CORBA:STRUCTMEMBERSEQ NIL 'SEQUENCE)
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:UNIONMEMBER :ID
  "IDL:omg.org/CORBA/UnionMember:1.0" :MEMBERS
  ((NAME NIL) (LABEL NIL) (TYPE NIL) (TYPE_DEF NIL)))
(DEFTYPE OMG.ORG/CORBA:UNIONMEMBERSEQ NIL 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:ENUMMEMBERSEQ NIL 'SEQUENCE)
(DEFCLASS OMG.ORG/CORBA:CONTAINER (OMG.ORG/CORBA:IROBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:CONTAINER-PROXY
  (OMG.ORG/CORBA:CONTAINER OMG.ORG/CORBA:IROBJECT-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/Container:1.0"
  'OMG.ORG/CORBA:CONTAINER-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:LOOKUP
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "lookup" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CONTENTS
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "contents" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LOOKUP_NAME
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "lookup_name" ARGS))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:CONTAINER/DESCRIPTION :ID
  "IDL:omg.org/CORBA/Container/Description:1.0" :MEMBERS
  ((CONTAINED_OBJECT NIL) (KIND NIL) (VALUE NIL)))
(DEFTYPE OMG.ORG/CORBA:CONTAINER/DESCRIPTIONSEQ NIL 'SEQUENCE)
(DEFMETHOD OMG.ORG/FEATURES:DESCRIBE_CONTENTS
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "describe_contents" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_MODULE
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_module" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_CONSTANT
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_constant" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_STRUCT
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_struct" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_EXCEPTION
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_exception" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_UNION
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_union" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ENUM
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_enum" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ALIAS
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_alias" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_INTERFACE
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_interface" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LOCATE_ID
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "locate_id" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:LOCATE_NAME
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "locate_name" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:REMOVE_CONTAINED
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "remove_contained" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:ADD_CONTAINED
  ((OBJ OMG.ORG/CORBA:CONTAINER-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "add_contained" ARGS))
(DEFCLASS OMG.ORG/CORBA:IDLTYPE (OMG.ORG/CORBA:IROBJECT) NIL)
(DEFCLASS OMG.ORG/CORBA:IDLTYPE-PROXY
  (OMG.ORG/CORBA:IDLTYPE OMG.ORG/CORBA:IROBJECT-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/IDLType:1.0"
  'OMG.ORG/CORBA:IDLTYPE-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:TYPE ((OBJ OMG.ORG/CORBA:IDLTYPE-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_type"))
(DEFTYPE OMG.ORG/CORBA:PRIMITIVEKIND NIL
  '(MEMBER :PK_NULL :PK_VOID :PK_SHORT :PK_LONG :PK_USHORT :PK_ULONG
     :PK_FLOAT :PK_DOUBLE :PK_BOOLEAN :PK_CHAR :PK_OCTET :PK_ANY :PK_TYPECODE
     :PK_PRINCIPAL :PK_STRING :PK_OBJREF :PK_LONGLONG :PK_ULONGLONG
     :PK_LONGDOUBLE :PK_WCHAR :PK_WSTRING))
(DEFTYPE OMG.ORG/CORBA:IDLTYPESEQ NIL 'SEQUENCE)
(DEFCLASS OMG.ORG/CORBA:REPOSITORY (OMG.ORG/CORBA:CONTAINER) NIL)
(DEFCLASS OMG.ORG/CORBA:REPOSITORY-PROXY
  (OMG.ORG/CORBA:REPOSITORY OMG.ORG/CORBA:CONTAINER-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/Repository:1.0"
  'OMG.ORG/CORBA:REPOSITORY-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:LOOKUP_ID
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "lookup_id" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:GET_PRIMITIVE
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "get_primitive" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_STRING
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_string" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_WSTRING
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_wstring" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_SEQUENCE
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_sequence" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ARRAY
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_array" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_FIXED
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_fixed" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:GET_ANONYMOUS_TYPES
  ((OBJ OMG.ORG/CORBA:REPOSITORY-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "get_anonymous_types" ARGS))
(DEFCLASS OMG.ORG/CORBA:MODULEDEF
  (OMG.ORG/CORBA:CONTAINER OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:MODULEDEF-PROXY
  (OMG.ORG/CORBA:MODULEDEF OMG.ORG/CORBA:CONTAINER-PROXY
    OMG.ORG/CORBA:CONTAINED-PROXY)
  NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/ModuleDef:1.0"
  'OMG.ORG/CORBA:MODULEDEF-PROXY)
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:MODULEDESCRIPTION :ID
  "IDL:omg.org/CORBA/ModuleDescription:1.0" :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL)))
(DEFCLASS OMG.ORG/CORBA:CONSTANTDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:CONSTANTDEF-PROXY
  (OMG.ORG/CORBA:CONSTANTDEF OMG.ORG/CORBA:CONTAINED-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/ConstantDef:1.0"
  'OMG.ORG/CORBA:CONSTANTDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:TYPE
  ((OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_type"))
(DEFMETHOD OMG.ORG/FEATURES:TYPE_DEF
  ((OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_type_def"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:TYPE_DEF)
  (NEWVAL (OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:VALUE
  ((OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_value"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:VALUE)
  (NEWVAL (OBJ OMG.ORG/CORBA:CONSTANTDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_value" NEWVAL))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:CONSTANTDESCRIPTION :ID
  "IDL:omg.org/CORBA/ConstantDescription:1.0" :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL) (VALUE NIL)))
(DEFCLASS OMG.ORG/CORBA:TYPEDEFDEF
  (OMG.ORG/CORBA:CONTAINED OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:TYPEDEFDEF-PROXY
  (OMG.ORG/CORBA:TYPEDEFDEF OMG.ORG/CORBA:CONTAINED-PROXY
    OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/TypedefDef:1.0"
  'OMG.ORG/CORBA:TYPEDEFDEF-PROXY)
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:TYPEDESCRIPTION :ID
  "IDL:omg.org/CORBA/TypeDescription:1.0" :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL)))
(DEFCLASS OMG.ORG/CORBA:STRUCTDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:STRUCTDEF-PROXY
  (OMG.ORG/CORBA:STRUCTDEF OMG.ORG/CORBA:TYPEDEFDEF-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/StructDef:1.0"
  'OMG.ORG/CORBA:STRUCTDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS
  ((OBJ OMG.ORG/CORBA:STRUCTDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_members"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS)
  (NEWVAL (OBJ OMG.ORG/CORBA:STRUCTDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:UNIONDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:UNIONDEF-PROXY
  (OMG.ORG/CORBA:UNIONDEF OMG.ORG/CORBA:TYPEDEFDEF-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/UnionDef:1.0"
  'OMG.ORG/CORBA:UNIONDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:DISCRIMINATOR_TYPE
  ((OBJ OMG.ORG/CORBA:UNIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_discriminator_type"))
(DEFMETHOD OMG.ORG/FEATURES:DISCRIMINATOR_TYPE_DEF
  ((OBJ OMG.ORG/CORBA:UNIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_discriminator_type_def"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:DISCRIMINATOR_TYPE_DEF)
  (NEWVAL (OBJ OMG.ORG/CORBA:UNIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_discriminator_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS
  ((OBJ OMG.ORG/CORBA:UNIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_members"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS)
  (NEWVAL (OBJ OMG.ORG/CORBA:UNIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:ENUMDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:ENUMDEF-PROXY
  (OMG.ORG/CORBA:ENUMDEF OMG.ORG/CORBA:TYPEDEFDEF-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/EnumDef:1.0"
  'OMG.ORG/CORBA:ENUMDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS ((OBJ OMG.ORG/CORBA:ENUMDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_members"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS)
  (NEWVAL (OBJ OMG.ORG/CORBA:ENUMDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:ALIASDEF (OMG.ORG/CORBA:TYPEDEFDEF) NIL)
(DEFCLASS OMG.ORG/CORBA:ALIASDEF-PROXY
  (OMG.ORG/CORBA:ALIASDEF OMG.ORG/CORBA:TYPEDEFDEF-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/AliasDef:1.0"
  'OMG.ORG/CORBA:ALIASDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:ORIGINAL_TYPE_DEF
  ((OBJ OMG.ORG/CORBA:ALIASDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_original_type_def"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ORIGINAL_TYPE_DEF)
  (NEWVAL (OBJ OMG.ORG/CORBA:ALIASDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_original_type_def" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:PRIMITIVEDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:PRIMITIVEDEF-PROXY
  (OMG.ORG/CORBA:PRIMITIVEDEF OMG.ORG/CORBA:IDLTYPE-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/PrimitiveDef:1.0"
  'OMG.ORG/CORBA:PRIMITIVEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:KIND
  ((OBJ OMG.ORG/CORBA:PRIMITIVEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_kind"))
(DEFCLASS OMG.ORG/CORBA:STRINGDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:STRINGDEF-PROXY
  (OMG.ORG/CORBA:STRINGDEF OMG.ORG/CORBA:IDLTYPE-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/StringDef:1.0"
  'OMG.ORG/CORBA:STRINGDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:BOUND ((OBJ OMG.ORG/CORBA:STRINGDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_bound"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BOUND)
  (NEWVAL (OBJ OMG.ORG/CORBA:STRINGDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_bound" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:WSTRINGDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:WSTRINGDEF-PROXY
  (OMG.ORG/CORBA:WSTRINGDEF OMG.ORG/CORBA:IDLTYPE-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/WstringDef:1.0"
  'OMG.ORG/CORBA:WSTRINGDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:BOUND
  ((OBJ OMG.ORG/CORBA:WSTRINGDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_bound"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BOUND)
  (NEWVAL (OBJ OMG.ORG/CORBA:WSTRINGDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_bound" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:FIXEDDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:FIXEDDEF-PROXY
  (OMG.ORG/CORBA:FIXEDDEF OMG.ORG/CORBA:IDLTYPE-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/FixedDef:1.0"
  'OMG.ORG/CORBA:FIXEDDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:DIGITS ((OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_digits"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:DIGITS)
  (NEWVAL (OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_digits" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:SCALE ((OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_scale"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:SCALE)
  (NEWVAL (OBJ OMG.ORG/CORBA:FIXEDDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_scale" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:SEQUENCEDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:SEQUENCEDEF-PROXY
  (OMG.ORG/CORBA:SEQUENCEDEF OMG.ORG/CORBA:IDLTYPE-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/SequenceDef:1.0"
  'OMG.ORG/CORBA:SEQUENCEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:BOUND
  ((OBJ OMG.ORG/CORBA:SEQUENCEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_bound"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BOUND)
  (NEWVAL (OBJ OMG.ORG/CORBA:SEQUENCEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_bound" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE
  ((OBJ OMG.ORG/CORBA:SEQUENCEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_element_type"))
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE_DEF
  ((OBJ OMG.ORG/CORBA:SEQUENCEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_element_type_def"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ELEMENT_TYPE_DEF)
  (NEWVAL (OBJ OMG.ORG/CORBA:SEQUENCEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_element_type_def" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:ARRAYDEF (OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:ARRAYDEF-PROXY
  (OMG.ORG/CORBA:ARRAYDEF OMG.ORG/CORBA:IDLTYPE-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/ArrayDef:1.0"
  'OMG.ORG/CORBA:ARRAYDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:LENGTH ((OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_length"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:LENGTH)
  (NEWVAL (OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_length" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE
  ((OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_element_type"))
(DEFMETHOD OMG.ORG/FEATURES:ELEMENT_TYPE_DEF
  ((OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_element_type_def"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:ELEMENT_TYPE_DEF)
  (NEWVAL (OBJ OMG.ORG/CORBA:ARRAYDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_element_type_def" NEWVAL))
(DEFCLASS OMG.ORG/CORBA:EXCEPTIONDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:EXCEPTIONDEF-PROXY
  (OMG.ORG/CORBA:EXCEPTIONDEF OMG.ORG/CORBA:CONTAINED-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/ExceptionDef:1.0"
  'OMG.ORG/CORBA:EXCEPTIONDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:TYPE
  ((OBJ OMG.ORG/CORBA:EXCEPTIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_type"))
(DEFMETHOD OMG.ORG/FEATURES:MEMBERS
  ((OBJ OMG.ORG/CORBA:EXCEPTIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_members"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MEMBERS)
  (NEWVAL (OBJ OMG.ORG/CORBA:EXCEPTIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_members" NEWVAL))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:EXCEPTIONDESCRIPTION :ID
  "IDL:omg.org/CORBA/ExceptionDescription:1.0" :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL)))
(DEFTYPE OMG.ORG/CORBA:ATTRIBUTEMODE NIL
  '(MEMBER :ATTR_NORMAL :ATTR_READONLY))
(DEFCLASS OMG.ORG/CORBA:ATTRIBUTEDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY
  (OMG.ORG/CORBA:ATTRIBUTEDEF OMG.ORG/CORBA:CONTAINED-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/AttributeDef:1.0"
  'OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:TYPE
  ((OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_type"))
(DEFMETHOD OMG.ORG/FEATURES:TYPE_DEF
  ((OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_type_def"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:TYPE_DEF)
  (NEWVAL (OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_type_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:MODE
  ((OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_mode"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MODE)
  (NEWVAL (OBJ OMG.ORG/CORBA:ATTRIBUTEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_mode" NEWVAL))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:ATTRIBUTEDESCRIPTION :ID
  "IDL:omg.org/CORBA/AttributeDescription:1.0" :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (TYPE NIL) (MODE NIL)))
(DEFTYPE OMG.ORG/CORBA:OPERATIONMODE NIL '(MEMBER :OP_NORMAL :OP_ONEWAY))
(DEFTYPE OMG.ORG/CORBA:PARAMETERMODE NIL
  '(MEMBER :PARAM_IN :PARAM_OUT :PARAM_INOUT))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:PARAMETERDESCRIPTION :ID
  "IDL:omg.org/CORBA/ParameterDescription:1.0" :MEMBERS
  ((NAME NIL) (TYPE NIL) (TYPE_DEF NIL) (MODE NIL)))
(DEFTYPE OMG.ORG/CORBA:PARDESCRIPTIONSEQ NIL 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:CONTEXTIDENTIFIER NIL 'OMG.ORG/CORBA:IDENTIFIER)
(DEFTYPE OMG.ORG/CORBA:CONTEXTIDSEQ NIL 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:EXCEPTIONDEFSEQ NIL 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:EXCDESCRIPTIONSEQ NIL 'SEQUENCE)
(DEFCLASS OMG.ORG/CORBA:OPERATIONDEF (OMG.ORG/CORBA:CONTAINED) NIL)
(DEFCLASS OMG.ORG/CORBA:OPERATIONDEF-PROXY
  (OMG.ORG/CORBA:OPERATIONDEF OMG.ORG/CORBA:CONTAINED-PROXY) NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/OperationDef:1.0"
  'OMG.ORG/CORBA:OPERATIONDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:RESULT
  ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_result"))
(DEFMETHOD OMG.ORG/FEATURES:RESULT_DEF
  ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_result_def"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:RESULT_DEF)
  (NEWVAL (OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_result_def" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:PARAMS
  ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_params"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:PARAMS)
  (NEWVAL (OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_params" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:MODE
  ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_mode"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:MODE)
  (NEWVAL (OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_mode" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:CONTEXTS
  ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_contexts"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:CONTEXTS)
  (NEWVAL (OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_contexts" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:EXCEPTIONS
  ((OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_exceptions"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:EXCEPTIONS)
  (NEWVAL (OBJ OMG.ORG/CORBA:OPERATIONDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_exceptions" NEWVAL))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:OPERATIONDESCRIPTION :ID
  "IDL:omg.org/CORBA/OperationDescription:1.0" :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (RESULT NIL) (MODE NIL)
    (CONTEXTS NIL) (PARAMETERS NIL) (EXCEPTIONS NIL)))
(DEFTYPE OMG.ORG/CORBA:REPOSITORYIDSEQ NIL 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:OPDESCRIPTIONSEQ NIL 'SEQUENCE)
(DEFTYPE OMG.ORG/CORBA:ATTRDESCRIPTIONSEQ NIL 'SEQUENCE)
(DEFCLASS OMG.ORG/CORBA:INTERFACEDEF
  (OMG.ORG/CORBA:CONTAINER OMG.ORG/CORBA:CONTAINED OMG.ORG/CORBA:IDLTYPE) NIL)
(DEFCLASS OMG.ORG/CORBA:INTERFACEDEF-PROXY
  (OMG.ORG/CORBA:INTERFACEDEF OMG.ORG/CORBA:CONTAINER-PROXY
    OMG.ORG/CORBA:CONTAINED-PROXY OMG.ORG/CORBA:IDLTYPE-PROXY)
  NIL)
(REGISTER-PROXY-CLASS "IDL:omg.org/CORBA/InterfaceDef:1.0"
  'OMG.ORG/CORBA:INTERFACEDEF-PROXY)
(DEFMETHOD OMG.ORG/FEATURES:BASE_INTERFACES
  ((OBJ OMG.ORG/CORBA:INTERFACEDEF-PROXY) &rest args)
  (APPLY 'INVOKE OBJ "_get_base_interfaces"))
(DEFMETHOD (SETF OMG.ORG/FEATURES:BASE_INTERFACES)
  (NEWVAL (OBJ OMG.ORG/CORBA:INTERFACEDEF-PROXY))
  (APPLY 'INVOKE OBJ "_set_base_interfaces" NEWVAL))
(DEFMETHOD OMG.ORG/FEATURES:IS_A
  ((OBJ OMG.ORG/CORBA:INTERFACEDEF-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "is_a" ARGS))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:INTERFACEDEF/FULLINTERFACEDESCRIPTION :ID
  "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0" :MEMBERS
  ((NAME NIL) (ID NIL) (DEFINED_IN NIL) (VERSION NIL) (OPERATIONS NIL)
    (ATTRIBUTES NIL) (BASE_INTERFACES NIL) (TYPE NIL)))
(DEFMETHOD OMG.ORG/FEATURES:DESCRIBE_INTERFACE
  ((OBJ OMG.ORG/CORBA:INTERFACEDEF-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "describe_interface" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_ATTRIBUTE
  ((OBJ OMG.ORG/CORBA:INTERFACEDEF-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_attribute" ARGS))
(DEFMETHOD OMG.ORG/FEATURES:CREATE_OPERATION
  ((OBJ OMG.ORG/CORBA:INTERFACEDEF-PROXY) &REST ARGS)
  (APPLY 'INVOKE OBJ "create_operation" ARGS))
(DEFINE-CORBA-STRUCT OMG.ORG/CORBA:INTERFACEDESCRIPTION :ID
  "IDL:omg.org/CORBA/InterfaceDescription:1.0" :MEMBERS
  ((NAME NIL)
   (ID NIL)
   (DEFINED_IN NIL)
   (VERSION NIL)
   (BASE_INTERFACES NIL))))
