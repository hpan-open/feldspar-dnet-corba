
(in-package :clorb)

(defconstant iop:unknownexceptioninfo (quote 9))

(defconstant iop:forwarded_identity (quote 8))

(defconstant iop:invocation_policies (quote 7))

(defconstant iop:sendingcontextruntime (quote 6))

(defconstant iop:bi_dir_iiop (quote 5))

(defconstant iop:logicalthreadid (quote 4))

(defconstant iop:chainbypassinfo (quote 3))

(defconstant iop:chainbypasscheck (quote 2))

(defconstant iop:codesets (quote 1))

(defconstant iop:transactionservice (quote 0))

(DEFINE-ALIAS IOP:SERVICECONTEXTLIST
 :id "IDL:IOP/ServiceContextList:1.0"
 :name "ServiceContextList"
 :type SEQUENCE
 :typecode (create-sequence-tc 0 (SYMBOL-TYPECODE 'IOP:SERVICECONTEXT)))

(DEFINE-STRUCT IOP:SERVICECONTEXT
 :id "IDL:IOP/ServiceContext:1.0"
 :name "ServiceContext"
 :members (("context_id" (SYMBOL-TYPECODE 'IOP:SERVICEID) CONTEXT_ID)
           ("context_data" (create-sequence-tc 0 OMG.ORG/CORBA:TC_OCTET)
            CONTEXT_DATA))
 :read ((BUFFER)
        (IOP:SERVICECONTEXT
          :context_id
          (UNMARSHAL-ULONG BUFFER)
          :context_data
          (UNMARSHAL-OSEQUENCE BUFFER)))
 :write ((OBJ BUFFER) (MARSHAL-ULONG (OMG.ORG/FEATURES:CONTEXT_ID OBJ) BUFFER)
         (MARSHAL-OSEQUENCE (OMG.ORG/FEATURES:CONTEXT_DATA OBJ) BUFFER)))

(DEFINE-ALIAS IOP:SERVICEID
 :id "IDL:IOP/ServiceId:1.0"
 :name "ServiceId"
 :type OMG.ORG/CORBA:ULONG
 :typecode OMG.ORG/CORBA:TC_ULONG)

(defconstant iop:tag_dce_sec_mech (quote 103))

(defconstant iop:tag_dce_no_pipes (quote 102))

(defconstant iop:tag_dce_binding_name (quote 101))

(defconstant iop:tag_dce_string_binding (quote 100))

(defconstant iop:tag_location_policy (quote 12))

(defconstant iop:tag_endpoint_id_position (quote 6))

(defconstant iop:tag_complete_object_key (quote 5))

(defconstant iop:tag_java_codebase (quote 25))

(defconstant iop:tag_generic_sec_mech (quote 22))

(defconstant iop:tag_csi_ecma_public_sec_mech (quote 21))

(defconstant iop:tag_ssl_sec_trans (quote 20))

(defconstant iop:tag_csi_ecma_hybrid_sec_mech (quote 19))

(defconstant iop:tag_csi_ecma_secret_sec_mech (quote 18))

(defconstant iop:tag_kerberosv5_sec_mech (quote 17))

(defconstant iop:tag_spkm_2_sec_mech (quote 16))

(defconstant iop:tag_spkm_1_sec_mech (quote 15))

(defconstant iop:tag_sec_name (quote 14))

(defconstant iop:tag_association_options (quote 13))

(defconstant iop:tag_alternate_iiop_address (quote 3))

(defconstant iop:tag_policies (quote 2))

(defconstant iop:tag_code_sets (quote 1))

(defconstant iop:tag_orb_type (quote 0))

(DEFINE-ALIAS IOP:MULTIPLECOMPONENTPROFILE
 :id "IDL:IOP/MultipleComponentProfile:1.0"
 :name "MultipleComponentProfile"
 :type SEQUENCE
 :typecode (create-sequence-tc 0 (SYMBOL-TYPECODE 'IOP:TAGGEDCOMPONENT)))

(DEFINE-STRUCT IOP:TAGGEDCOMPONENT
 :id "IDL:IOP/TaggedComponent:1.0"
 :name "TaggedComponent"
 :members (("tag" (SYMBOL-TYPECODE 'IOP:COMPONENTID) TAG)
           ("component_data" (create-sequence-tc 0 OMG.ORG/CORBA:TC_OCTET)
            COMPONENT_DATA))
 :read ((BUFFER)
        (IOP:TAGGEDCOMPONENT
          :tag
          (UNMARSHAL-ULONG BUFFER)
          :component_data
          (UNMARSHAL-OSEQUENCE BUFFER)))
 :write ((OBJ BUFFER) (MARSHAL-ULONG (OMG.ORG/FEATURES:TAG OBJ) BUFFER)
         (MARSHAL-OSEQUENCE (OMG.ORG/FEATURES:COMPONENT_DATA OBJ) BUFFER)))

(DEFINE-ALIAS IOP:COMPONENTID
 :id "IDL:IOP/ComponentId:1.0"
 :name "ComponentId"
 :type OMG.ORG/CORBA:ULONG
 :typecode OMG.ORG/CORBA:TC_ULONG)

(DEFINE-STRUCT IOP:IOR
 :id "IDL:IOP/IOR:1.0"
 :name "IOR"
 :members (("type_id" OMG.ORG/CORBA:TC_STRING TYPE_ID)
           ("profiles"
            (create-sequence-tc 0 (SYMBOL-TYPECODE 'IOP:TAGGEDPROFILE))
            PROFILES))
 :read ((BUFFER)
        (IOP:IOR :type_id
                 (UNMARSHAL-STRING BUFFER)
                 :profiles
                 (UNMARSHAL-SEQUENCE
                   (lambda (buffer) (STRUCT-READ 'IOP:TAGGEDPROFILE BUFFER))
                   BUFFER)))
 :write ((OBJ BUFFER) (MARSHAL-STRING (OMG.ORG/FEATURES:TYPE_ID OBJ) BUFFER)
         (MARSHAL-SEQUENCE
           (OMG.ORG/FEATURES:PROFILES OBJ)
           (lambda (obj buffer) (STRUCT-WRITE OBJ 'IOP:TAGGEDPROFILE BUFFER))
           BUFFER)))

(DEFINE-STRUCT IOP:TAGGEDPROFILE
 :id "IDL:IOP/TaggedProfile:1.0"
 :name "TaggedProfile"
 :members (("tag" (SYMBOL-TYPECODE 'IOP:PROFILEID) TAG)
           ("profile_data" (create-sequence-tc 0 OMG.ORG/CORBA:TC_OCTET)
            PROFILE_DATA))
 :read ((BUFFER)
        (IOP:TAGGEDPROFILE
          :tag
          (UNMARSHAL-ULONG BUFFER)
          :profile_data
          (UNMARSHAL-OSEQUENCE BUFFER)))
 :write ((OBJ BUFFER) (MARSHAL-ULONG (OMG.ORG/FEATURES:TAG OBJ) BUFFER)
         (MARSHAL-OSEQUENCE (OMG.ORG/FEATURES:PROFILE_DATA OBJ) BUFFER)))

(defconstant iop:tag_multiple_components (quote 1))

(defconstant iop:tag_internet_iop (quote 0))

(DEFINE-ALIAS IOP:PROFILEID
 :id "IDL:IOP/ProfileId:1.0"
 :name "ProfileId"
 :type OMG.ORG/CORBA:ULONG
 :typecode OMG.ORG/CORBA:TC_ULONG)
