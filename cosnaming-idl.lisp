
(IN-PACKAGE :CLORB)
(IDEF-DEFINITIONS
  (WITH-PREFIX "omg.org"
   (DEFINE-MODULE "CosNaming" NIL (DEFINE-TYPE "Istring" STRING)
    (DEFINE-STRUCT "NameComponent"
     (("id" "CosNaming::Istring") ("kind" "CosNaming::Istring")))
    (DEFINE-TYPE "Name" (SEQUENCE "CosNaming::NameComponent" 0))
    (DEFINE-ENUM "BindingType" ("nobject" "ncontext"))
    (DEFINE-STRUCT "Binding"
     (("binding_name" "CosNaming::Name")
      ("binding_type" "CosNaming::BindingType")))
    (DEFINE-TYPE "BindingList" (SEQUENCE "CosNaming::Binding" 0))
    (DEFINE-INTERFACE "NamingContext" NIL
     (DEFINE-ENUM "NotFoundReason" ("missing_node" "not_context" "not_object"))
     (DEFINE-EXCEPTION "NotFound"
      (("why" "CosNaming::NamingContext::NotFoundReason")
       ("rest_of_name" "CosNaming::Name")))
     (DEFINE-EXCEPTION "CannotProceed"
      (("cxt" "CosNaming::NamingContext") ("rest_of_name" "CosNaming::Name")))
     (DEFINE-EXCEPTION "InvalidName" NIL) (DEFINE-EXCEPTION "AlreadyBound" NIL)
     (DEFINE-EXCEPTION "NotEmpty" NIL)
     (DEFINE-OPERATION
       "bind"
       ((:PARAM_IN "n" "CosNaming::Name") (:PARAM_IN "obj" OBJECT))
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"
        "CosNaming::NamingContext::AlreadyBound"))
     (DEFINE-OPERATION
       "rebind"
       ((:PARAM_IN "n" "CosNaming::Name") (:PARAM_IN "obj" OBJECT))
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "bind_context"
       ((:PARAM_IN "n" "CosNaming::Name")
        (:PARAM_IN "nc" "CosNaming::NamingContext"))
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"
        "CosNaming::NamingContext::AlreadyBound"))
     (DEFINE-OPERATION
       "rebind_context"
       ((:PARAM_IN "n" "CosNaming::Name")
        (:PARAM_IN "nc" "CosNaming::NamingContext"))
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "resolve"
       ((:PARAM_IN "n" "CosNaming::Name"))
       :RESULT-TYPE
       OBJECT
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "unbind"
       ((:PARAM_IN "n" "CosNaming::Name"))
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "new_context"
       NIL
       :RESULT-TYPE
       "CosNaming::NamingContext"
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "bind_new_context"
       ((:PARAM_IN "n" "CosNaming::Name"))
       :RESULT-TYPE
       "CosNaming::NamingContext"
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::AlreadyBound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "destroy"
       NIL
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotEmpty"))
     (DEFINE-OPERATION
       "list"
       ((:PARAM_IN "how_many" ULONG) (:PARAM_OUT "bl" "CosNaming::BindingList")
        (:PARAM_OUT "bi" "CosNaming::BindingIterator"))
       :RESULT-TYPE
       VOID
       :EXCEPTIONS
       NIL))
    (DEFINE-INTERFACE "BindingIterator" NIL
     (DEFINE-OPERATION
       "next_one"
       ((:PARAM_OUT "b" "CosNaming::Binding"))
       :RESULT-TYPE
       BOOLEAN
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION
       "next_n"
       ((:PARAM_IN "how_many" ULONG)
        (:PARAM_OUT "bl" "CosNaming::BindingList"))
       :RESULT-TYPE
       BOOLEAN
       :EXCEPTIONS
       NIL)
     (DEFINE-OPERATION "destroy" NIL :RESULT-TYPE VOID :EXCEPTIONS NIL))
    (DEFINE-INTERFACE "NamingContextExt" (:BASES ("CosNaming::NamingContext"))
     (DEFINE-TYPE "StringName" STRING) (DEFINE-TYPE "Address" STRING)
     (DEFINE-TYPE "URLString" STRING)
     (DEFINE-OPERATION
       "to_string"
       ((:PARAM_IN "n" "CosNaming::Name"))
       :RESULT-TYPE
       "CosNaming::NamingContextExt::StringName"
       :EXCEPTIONS
       ("CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "to_name"
       ((:PARAM_IN "sn" "CosNaming::NamingContextExt::StringName"))
       :RESULT-TYPE
       "CosNaming::Name"
       :EXCEPTIONS
       ("CosNaming::NamingContext::InvalidName"))
     (DEFINE-EXCEPTION "InvalidAddress" NIL)
     (DEFINE-OPERATION
       "to_url"
       ((:PARAM_IN "addr" "CosNaming::NamingContextExt::Address")
        (:PARAM_IN "sn" "CosNaming::NamingContextExt::StringName"))
       :RESULT-TYPE
       "CosNaming::NamingContextExt::URLString"
       :EXCEPTIONS
       ("CosNaming::NamingContextExt::InvalidAddress"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "resolve_str"
       ((:PARAM_IN "n" "CosNaming::NamingContextExt::StringName"))
       :RESULT-TYPE
       OBJECT
       :EXCEPTIONS
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"
        "CosNaming::NamingContext::AlreadyBound"))))))