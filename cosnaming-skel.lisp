(in-package :clorb)

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (ENSURE-CORBA-PACKAGE
    "COSNAMING"
    :EXPORT '("NAMINGCONTEXT-SERVANT" "BINDINGITERATOR-SERVANT")))


;;; Define the skeleton classes


(defclass COSNAMING:NAMINGCONTEXT-SERVANT (clorb:auto-servant
                                           COSNAMING:NAMINGCONTEXT)
  ())
(defmethod clorb:servant-interface-id ((obj COSNAMING:NamingContext-Servant))
  "IDL:omg.org/CosNaming/NamingContext:1.0")

(defclass cosnaming:bindingiterator-servant (clorb:auto-servant
                                             COSNAMING:BINDINGITERATOR)
  ())
(defmethod clorb:servant-interface-id ((obj COSNAMING:Bindingiterator-Servant))
  "IDL:omg.org/CosNaming/BindingIterator:1.0")



