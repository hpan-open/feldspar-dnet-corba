
(IN-PACKAGE :CLORB)
(IDEF-DEFINITIONS
  (WITH-PREFIX "omg.org"
   (DEFINE-MODULE "CORBA" NIL (DEFINE-TYPE "Identifier" STRING)
    (DEFINE-TYPE "ScopedName" STRING) (DEFINE-TYPE "RepositoryId" STRING)
    (DEFINE-ENUM "DefinitionKind"
     ("dk_none" "dk_all" "dk_Attribute" "dk_Constant" "dk_Exception"
      "dk_Interface" "dk_Module" "dk_Operation" "dk_Typedef" "dk_Alias"
      "dk_Struct" "dk_Union" "dk_Enum" "dk_Primitive" "dk_String" "dk_Sequence"
      "dk_Array" "dk_Repository" "dk_Wstring" "dk_Fixed"))
    (DEFINE-INTERFACE "IRObject" NIL
     (DEFINE-ATTRIBUTE "def_kind" "CORBA::DefinitionKind" :READONLY T)
     (DEFINE-OPERATION "destroy" NIL :RESULT-TYPE VOID :EXCEPTIONS NIL))
    (DEFINE-TYPE "VersionSpec" STRING)
    (DEFINE-INTERFACE "Contained" (:BASES ("CORBA::IRObject"))
     (DEFINE-ATTRIBUTE "id" "CORBA::RepositoryId")
     (DEFINE-ATTRIBUTE "name" "CORBA::Identifier")
     (DEFINE-ATTRIBUTE "version" "CORBA::VersionSpec")
     (DEFINE-ATTRIBUTE "defined_in" "CORBA::Container" :READONLY T)
     (DEFINE-ATTRIBUTE "absolute_name" "CORBA::ScopedName" :READONLY T)
     (DEFINE-ATTRIBUTE "containing_repository" "CORBA::Repository" :READONLY T)
     (DEFINE-STRUCT "Description"
      (("kind" "CORBA::DefinitionKind") ("value" ANY)))
     (DEFINE-OPERATION
       "describe"
       NIL
       :RESULT-TYPE
       "CORBA::Contained::Description"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "move"
       ((:PARAM_IN "new_container" "CORBA::Container")
        (:PARAM_IN "new_name" "CORBA::Identifier")
        (:PARAM_IN "new_version" "CORBA::VersionSpec"))
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       NIL))
    (DEFINE-TYPE "InterfaceDefSeq" (SEQUENCE "CORBA::InterfaceDef" 0))
    (DEFINE-TYPE "ContainedSeq" (SEQUENCE "CORBA::Contained" 0))
    (DEFINE-STRUCT "StructMember"
     (("name" "CORBA::Identifier") ("type" TYPECODE)
      ("type_def" "CORBA::IDLType")))
    (DEFINE-TYPE "StructMemberSeq" (SEQUENCE "CORBA::StructMember" 0))
    (DEFINE-STRUCT "UnionMember"
     (("name" "CORBA::Identifier") ("label" ANY) ("type" TYPECODE)
      ("type_def" "CORBA::IDLType")))
    (DEFINE-TYPE "UnionMemberSeq" (SEQUENCE "CORBA::UnionMember" 0))
    (DEFINE-TYPE "EnumMemberSeq" (SEQUENCE "CORBA::Identifier" 0))
    (DEFINE-INTERFACE "Container" (:BASES ("CORBA::IRObject"))
     (DEFINE-OPERATION
       "lookup"
       ((:PARAM_IN "search_name" "CORBA::ScopedName"))
       :RESULT-TYPE
       "CORBA::Contained"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "contents"
       ((:PARAM_IN "limit_type" "CORBA::DefinitionKind")
        (:PARAM_IN "exclude_inherited" BOOLEAN))
       :RESULT-TYPE
       "CORBA::ContainedSeq"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "lookup_name"
       ((:PARAM_IN "search_name" "CORBA::Identifier")
        (:PARAM_IN "levels_to_search" LONG)
        (:PARAM_IN "limit_type" "CORBA::DefinitionKind")
        (:PARAM_IN "exclude_inherited" BOOLEAN))
       :RESULT-TYPE
       "CORBA::ContainedSeq"
       :EXCEPTIONS
       NIL)
     (DEFINE-STRUCT "Description"
      (("contained_object" "CORBA::Contained") ("kind" "CORBA::DefinitionKind")
       ("value" ANY)))
     (DEFINE-TYPE "DescriptionSeq"
      (SEQUENCE "CORBA::Container::Description" 0))
     (DEFINE-OPERATION
       "describe_contents"
       ((:PARAM_IN "limit_type" "CORBA::DefinitionKind")
        (:PARAM_IN "exclude_inherited" BOOLEAN)
        (:PARAM_IN "max_returned_objs" LONG))
       :RESULT-TYPE
       "CORBA::Container::DescriptionSeq"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_module"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec"))
       :RESULT-TYPE
       "CORBA::ModuleDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_constant"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "type" "CORBA::IDLType") (:PARAM_IN "value" ANY))
       :RESULT-TYPE
       "CORBA::ConstantDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_struct"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "members" "CORBA::StructMemberSeq"))
       :RESULT-TYPE
       "CORBA::StructDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_exception"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "members" "CORBA::StructMemberSeq"))
       :RESULT-TYPE
       "CORBA::ExceptionDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_union"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "discriminator_type" "CORBA::IDLType")
        (:PARAM_IN "members" "CORBA::UnionMemberSeq"))
       :RESULT-TYPE
       "CORBA::UnionDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_enum"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "members" "CORBA::EnumMemberSeq"))
       :RESULT-TYPE
       "CORBA::EnumDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_alias"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "original_type" "CORBA::IDLType"))
       :RESULT-TYPE
       "CORBA::AliasDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_interface"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "base_interfaces" "CORBA::InterfaceDefSeq"))
       :RESULT-TYPE
       "CORBA::InterfaceDef"
       :EXCEPTIONS
       NIL))
    (DEFINE-INTERFACE "IDLType" (:BASES ("CORBA::IRObject"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :READONLY T))
    (DEFINE-ENUM "PrimitiveKind"
     ("pk_null" "pk_void" "pk_short" "pk_long" "pk_ushort" "pk_ulong"
      "pk_float" "pk_double" "pk_boolean" "pk_char" "pk_octet" "pk_any"
      "pk_TypeCode" "pk_Principal" "pk_string" "pk_objref" "pk_longlong"
      "pk_ulonglong" "pk_longdouble" "pk_wchar" "pk_wstring"))
    (DEFINE-INTERFACE "Repository" (:BASES ("CORBA::Container"))
     (DEFINE-OPERATION
       "lookup_id"
       ((:PARAM_IN "search_id" "CORBA::RepositoryId"))
       :RESULT-TYPE
       "CORBA::Contained"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "get_primitive"
       ((:PARAM_IN "kind" "CORBA::PrimitiveKind"))
       :RESULT-TYPE
       "CORBA::PrimitiveDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_string"
       ((:PARAM_IN "bound" ULONG))
       :RESULT-TYPE
       "CORBA::StringDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_wstring"
       ((:PARAM_IN "bound" ULONG))
       :RESULT-TYPE
       "CORBA::WstringDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_sequence"
       ((:PARAM_IN "bound" ULONG) (:PARAM_IN "element_type" "CORBA::IDLType"))
       :RESULT-TYPE
       "CORBA::SequenceDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_array"
       ((:PARAM_IN "length" ULONG) (:PARAM_IN "element_type" "CORBA::IDLType"))
       :RESULT-TYPE
       "CORBA::ArrayDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_fixed"
       ((:PARAM_IN "digits" USHORT) (:PARAM_IN "scale" SHORT))
       :RESULT-TYPE
       "CORBA::FixedDef"
       :EXCEPTIONS
       NIL))
    (DEFINE-INTERFACE "ModuleDef"
     (:BASES ("CORBA::Container" "CORBA::Contained")))
    (DEFINE-STRUCT "ModuleDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")))
    (DEFINE-INTERFACE "ConstantDef" (:BASES ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :READONLY T)
     (DEFINE-ATTRIBUTE "type_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "value" ANY))
    (DEFINE-STRUCT "ConstantDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE) ("value" ANY)))
    (DEFINE-INTERFACE "TypedefDef"
     (:BASES ("CORBA::Contained" "CORBA::IDLType")))
    (DEFINE-STRUCT "TypeDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE)))
    (DEFINE-INTERFACE "StructDef" (:BASES ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "members" "CORBA::StructMemberSeq"))
    (DEFINE-INTERFACE "UnionDef" (:BASES ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "discriminator_type" TYPECODE :READONLY T)
     (DEFINE-ATTRIBUTE "discriminator_type_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "members" "CORBA::UnionMemberSeq"))
    (DEFINE-INTERFACE "EnumDef" (:BASES ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "members" "CORBA::EnumMemberSeq"))
    (DEFINE-INTERFACE "AliasDef" (:BASES ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "original_type_def" "CORBA::IDLType"))
    (DEFINE-INTERFACE "PrimitiveDef" (:BASES ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "kind" "CORBA::PrimitiveKind" :READONLY T))
    (DEFINE-INTERFACE "StringDef" (:BASES ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "bound" ULONG))
    (DEFINE-INTERFACE "WstringDef" (:BASES ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "bound" ULONG))
    (DEFINE-INTERFACE "FixedDef" (:BASES ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "digits" USHORT) (DEFINE-ATTRIBUTE "scale" SHORT))
    (DEFINE-INTERFACE "SequenceDef" (:BASES ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "bound" ULONG)
     (DEFINE-ATTRIBUTE "element_type" TYPECODE :READONLY T)
     (DEFINE-ATTRIBUTE "element_type_def" "CORBA::IDLType"))
    (DEFINE-INTERFACE "ArrayDef" (:BASES ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "length" ULONG)
     (DEFINE-ATTRIBUTE "element_type" TYPECODE :READONLY T)
     (DEFINE-ATTRIBUTE "element_type_def" "CORBA::IDLType"))
    (DEFINE-INTERFACE "ExceptionDef" (:BASES ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :READONLY T)
     (DEFINE-ATTRIBUTE "members" "CORBA::StructMemberSeq"))
    (DEFINE-STRUCT "ExceptionDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE)))
    (DEFINE-ENUM "AttributeMode" ("ATTR_NORMAL" "ATTR_READONLY"))
    (DEFINE-INTERFACE "AttributeDef" (:BASES ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :READONLY T)
     (DEFINE-ATTRIBUTE "type_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "mode" "CORBA::AttributeMode"))
    (DEFINE-STRUCT "AttributeDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE) ("mode" "CORBA::AttributeMode")))
    (DEFINE-ENUM "OperationMode" ("OP_NORMAL" "OP_ONEWAY"))
    (DEFINE-ENUM "ParameterMode" ("PARAM_IN" "PARAM_OUT" "PARAM_INOUT"))
    (DEFINE-STRUCT "ParameterDescription"
     (("name" "CORBA::Identifier") ("type" TYPECODE)
      ("type_def" "CORBA::IDLType") ("mode" "CORBA::ParameterMode")))
    (DEFINE-TYPE "ParDescriptionSeq"
     (SEQUENCE "CORBA::ParameterDescription" 0))
    (DEFINE-TYPE "ContextIdentifier" "CORBA::Identifier")
    (DEFINE-TYPE "ContextIdSeq" (SEQUENCE "CORBA::ContextIdentifier" 0))
    (DEFINE-TYPE "ExceptionDefSeq" (SEQUENCE "CORBA::ExceptionDef" 0))
    (DEFINE-TYPE "ExcDescriptionSeq"
     (SEQUENCE "CORBA::ExceptionDescription" 0))
    (DEFINE-INTERFACE "OperationDef" (:BASES ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "result" TYPECODE :READONLY T)
     (DEFINE-ATTRIBUTE "result_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "params" "CORBA::ParDescriptionSeq")
     (DEFINE-ATTRIBUTE "mode" "CORBA::OperationMode")
     (DEFINE-ATTRIBUTE "contexts" "CORBA::ContextIdSeq")
     (DEFINE-ATTRIBUTE "exceptions" "CORBA::ExceptionDefSeq"))
    (DEFINE-STRUCT "OperationDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("result" TYPECODE) ("mode" "CORBA::OperationMode")
      ("contexts" "CORBA::ContextIdSeq")
      ("parameters" "CORBA::ParDescriptionSeq")
      ("exceptions" "CORBA::ExcDescriptionSeq")))
    (DEFINE-TYPE "RepositoryIdSeq" (SEQUENCE "CORBA::RepositoryId" 0))
    (DEFINE-TYPE "OpDescriptionSeq" (SEQUENCE "CORBA::OperationDescription" 0))
    (DEFINE-TYPE "AttrDescriptionSeq"
     (SEQUENCE "CORBA::AttributeDescription" 0))
    (DEFINE-INTERFACE "InterfaceDef"
     (:BASES ("CORBA::Container" "CORBA::Contained" "CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "base_interfaces" "CORBA::InterfaceDefSeq")
     (DEFINE-OPERATION
       "is_a"
       ((:PARAM_IN "interface_id" "CORBA::RepositoryId"))
       :RESULT-TYPE
       BOOLEAN
       :EXCEPTIONS
       NIL)
     (DEFINE-STRUCT "FullInterfaceDescription"
      (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
       ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
       ("operations" "CORBA::OpDescriptionSeq")
       ("attributes" "CORBA::AttrDescriptionSeq")
       ("base_interfaces" "CORBA::RepositoryIdSeq") ("type" TYPECODE)))
     (DEFINE-OPERATION
       "describe_interface"
       NIL
       :RESULT-TYPE
       "CORBA::InterfaceDef::FullInterfaceDescription"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_attribute"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "type" "CORBA::IDLType")
        (:PARAM_IN "mode" "CORBA::AttributeMode"))
       :RESULT-TYPE
       "CORBA::AttributeDef"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "create_operation"
       ((:PARAM_IN "id" "CORBA::RepositoryId")
        (:PARAM_IN "name" "CORBA::Identifier")
        (:PARAM_IN "version" "CORBA::VersionSpec")
        (:PARAM_IN "result" "CORBA::IDLType")
        (:PARAM_IN "mode" "CORBA::OperationMode")
        (:PARAM_IN "params" "CORBA::ParDescriptionSeq")
        (:PARAM_IN "exceptions" "CORBA::ExceptionDefSeq")
        (:PARAM_IN "contexts" "CORBA::ContextIdSeq"))
       :RESULT-TYPE
       "CORBA::OperationDef"
       :EXCEPTIONS
       NIL))
    (DEFINE-STRUCT "InterfaceDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("base_interfaces" "CORBA::RepositoryIdSeq"))))))