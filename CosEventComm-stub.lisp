(in-package :clorb)

(defpackage "COSEVENTCOMM"
  (:use)
  (:export
   DISCONNECTED PULLCONSUMER PULLCONSUMER-PROXY 
   PULLSUPPLIER PULLSUPPLIER-PROXY PUSHCONSUMER 
   PUSHCONSUMER-PROXY PUSHSUPPLIER PUSHSUPPLIER-PROXY ))


(defclass COSEVENTCOMM:PULLCONSUMER (object) Nil)
(defclass COSEVENTCOMM:PULLCONSUMER-PROXY
 (coseventcomm:pullconsumer omg.org/corba:proxy) Nil)
(REGISTER-PROXY-CLASS "IDL:omg.org/CosEventComm/PullConsumer:1.0"
 'COSEVENTCOMM:PULLCONSUMER-PROXY)
(defmethod omg.org/features:disconnect_pull_consumer
 ((obj coseventcomm:pullconsumer-proxy) &rest args)
 (apply 'INVOKE OBJ "disconnect_pull_consumer" ARGS))
(defclass COSEVENTCOMM:PULLSUPPLIER (object) Nil)
(defclass COSEVENTCOMM:PULLSUPPLIER-PROXY
 (coseventcomm:pullsupplier omg.org/corba:proxy) Nil)
(REGISTER-PROXY-CLASS "IDL:omg.org/CosEventComm/PullSupplier:1.0"
 'COSEVENTCOMM:PULLSUPPLIER-PROXY)
(defmethod omg.org/features:disconnect_pull_supplier
 ((obj coseventcomm:pullsupplier-proxy) &rest args)
 (apply 'INVOKE OBJ "disconnect_pull_supplier" ARGS))
(defmethod omg.org/features:try_pull
 ((obj coseventcomm:pullsupplier-proxy) &rest args)
 (apply 'INVOKE OBJ "try_pull" ARGS))
(defmethod omg.org/features:pull
 ((obj coseventcomm:pullsupplier-proxy) &rest args)
 (apply 'INVOKE OBJ "pull" ARGS))
(defclass COSEVENTCOMM:PUSHSUPPLIER (object) Nil)
(defclass COSEVENTCOMM:PUSHSUPPLIER-PROXY
 (coseventcomm:pushsupplier omg.org/corba:proxy) Nil)
(REGISTER-PROXY-CLASS "IDL:omg.org/CosEventComm/PushSupplier:1.0"
 'COSEVENTCOMM:PUSHSUPPLIER-PROXY)
(defmethod omg.org/features:disconnect_push_supplier
 ((obj coseventcomm:pushsupplier-proxy) &rest args)
 (apply 'INVOKE OBJ "disconnect_push_supplier" ARGS))
(defclass COSEVENTCOMM:PUSHCONSUMER (object) Nil)
(defclass COSEVENTCOMM:PUSHCONSUMER-PROXY
 (coseventcomm:pushconsumer omg.org/corba:proxy) Nil)
(REGISTER-PROXY-CLASS "IDL:omg.org/CosEventComm/PushConsumer:1.0"
 'COSEVENTCOMM:PUSHCONSUMER-PROXY)
(defmethod omg.org/features:disconnect_push_consumer
 ((obj coseventcomm:pushconsumer-proxy) &rest args)
 (apply 'INVOKE OBJ "disconnect_push_consumer" ARGS))
(defmethod omg.org/features:push
 ((obj coseventcomm:pushconsumer-proxy) &rest args)
 (apply 'INVOKE OBJ "push" ARGS))
(DEFINE-USER-EXCEPTION COSEVENTCOMM:DISCONNECTED :id
 "IDL:omg.org/CosEventComm/Disconnected:1.0" :slots NIL)
