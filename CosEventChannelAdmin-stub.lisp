(in-package :clorb)

(defpackage "COSEVENTCHANNELADMIN"
  (:use)
  (:export
   ALREADYCONNECTED CONSUMERADMIN CONSUMERADMIN-PROXY 
   EVENTCHANNEL EVENTCHANNEL-PROXY PROXYPULLCONSUMER 
   PROXYPULLCONSUMER-PROXY PROXYPULLSUPPLIER PROXYPULLSUPPLIER-PROXY 
   PROXYPUSHCONSUMER PROXYPUSHCONSUMER-PROXY PROXYPUSHSUPPLIER 
   PROXYPUSHSUPPLIER-PROXY SUPPLIERADMIN SUPPLIERADMIN-PROXY 
   TYPEERROR ))



(progn (defclass COSEVENTCHANNELADMIN:EVENTCHANNEL (object) Nil)
 (defclass COSEVENTCHANNELADMIN:EVENTCHANNEL-PROXY
  (coseventchanneladmin:eventchannel omg.org/corba:proxy) Nil)
 (REGISTER-PROXY-CLASS "IDL:omg.org/CosEventChannelAdmin/EventChannel:1.0"
  'COSEVENTCHANNELADMIN:EVENTCHANNEL-PROXY)
 (defmethod omg.org/features:destroy
  ((obj coseventchanneladmin:eventchannel-proxy) &rest args)
  (apply 'INVOKE OBJ "destroy" ARGS))
 (defmethod omg.org/features:for_suppliers
  ((obj coseventchanneladmin:eventchannel-proxy) &rest args)
  (apply 'INVOKE OBJ "for_suppliers" ARGS))
 (defmethod omg.org/features:for_consumers
  ((obj coseventchanneladmin:eventchannel-proxy) &rest args)
  (apply 'INVOKE OBJ "for_consumers" ARGS))
 (defclass COSEVENTCHANNELADMIN:SUPPLIERADMIN (object) Nil)
 (defclass COSEVENTCHANNELADMIN:SUPPLIERADMIN-PROXY
  (coseventchanneladmin:supplieradmin omg.org/corba:proxy) Nil)
 (REGISTER-PROXY-CLASS "IDL:omg.org/CosEventChannelAdmin/SupplierAdmin:1.0"
  'COSEVENTCHANNELADMIN:SUPPLIERADMIN-PROXY)
 (defmethod omg.org/features:obtain_pull_consumer
  ((obj coseventchanneladmin:supplieradmin-proxy) &rest args)
  (apply 'INVOKE OBJ "obtain_pull_consumer" ARGS))
 (defmethod omg.org/features:obtain_push_consumer
  ((obj coseventchanneladmin:supplieradmin-proxy) &rest args)
  (apply 'INVOKE OBJ "obtain_push_consumer" ARGS))
 (defclass COSEVENTCHANNELADMIN:CONSUMERADMIN (object) Nil)
 (defclass COSEVENTCHANNELADMIN:CONSUMERADMIN-PROXY
  (coseventchanneladmin:consumeradmin omg.org/corba:proxy) Nil)
 (REGISTER-PROXY-CLASS "IDL:omg.org/CosEventChannelAdmin/ConsumerAdmin:1.0"
  'COSEVENTCHANNELADMIN:CONSUMERADMIN-PROXY)
 (defmethod omg.org/features:obtain_pull_supplier
  ((obj coseventchanneladmin:consumeradmin-proxy) &rest args)
  (apply 'INVOKE OBJ "obtain_pull_supplier" ARGS))
 (defmethod omg.org/features:obtain_push_supplier
  ((obj coseventchanneladmin:consumeradmin-proxy) &rest args)
  (apply 'INVOKE OBJ "obtain_push_supplier" ARGS))
 (defclass COSEVENTCHANNELADMIN:PROXYPUSHSUPPLIER (coseventcomm:pushsupplier)
  Nil)
 (defclass COSEVENTCHANNELADMIN:PROXYPUSHSUPPLIER-PROXY
  (coseventchanneladmin:proxypushsupplier coseventcomm:pushsupplier-proxy) Nil)
 (REGISTER-PROXY-CLASS "IDL:omg.org/CosEventChannelAdmin/ProxyPushSupplier:1.0"
  'COSEVENTCHANNELADMIN:PROXYPUSHSUPPLIER-PROXY)
 (defmethod omg.org/features:connect_push_consumer
  ((obj coseventchanneladmin:proxypushsupplier-proxy) &rest args)
  (apply 'INVOKE OBJ "connect_push_consumer" ARGS))
 (defclass COSEVENTCHANNELADMIN:PROXYPULLCONSUMER (coseventcomm:pullconsumer)
  Nil)
 (defclass COSEVENTCHANNELADMIN:PROXYPULLCONSUMER-PROXY
  (coseventchanneladmin:proxypullconsumer coseventcomm:pullconsumer-proxy) Nil)
 (REGISTER-PROXY-CLASS "IDL:omg.org/CosEventChannelAdmin/ProxyPullConsumer:1.0"
  'COSEVENTCHANNELADMIN:PROXYPULLCONSUMER-PROXY)
 (defmethod omg.org/features:connect_pull_supplier
  ((obj coseventchanneladmin:proxypullconsumer-proxy) &rest args)
  (apply 'INVOKE OBJ "connect_pull_supplier" ARGS))
 (defclass COSEVENTCHANNELADMIN:PROXYPULLSUPPLIER (coseventcomm:pullsupplier)
  Nil)
 (defclass COSEVENTCHANNELADMIN:PROXYPULLSUPPLIER-PROXY
  (coseventchanneladmin:proxypullsupplier coseventcomm:pullsupplier-proxy) Nil)
 (REGISTER-PROXY-CLASS "IDL:omg.org/CosEventChannelAdmin/ProxyPullSupplier:1.0"
  'COSEVENTCHANNELADMIN:PROXYPULLSUPPLIER-PROXY)
 (defmethod omg.org/features:connect_pull_consumer
  ((obj coseventchanneladmin:proxypullsupplier-proxy) &rest args)
  (apply 'INVOKE OBJ "connect_pull_consumer" ARGS))
 (defclass COSEVENTCHANNELADMIN:PROXYPUSHCONSUMER (coseventcomm:pushconsumer)
  Nil)
 (defclass COSEVENTCHANNELADMIN:PROXYPUSHCONSUMER-PROXY
  (coseventchanneladmin:proxypushconsumer coseventcomm:pushconsumer-proxy) Nil)
 (REGISTER-PROXY-CLASS "IDL:omg.org/CosEventChannelAdmin/ProxyPushConsumer:1.0"
  'COSEVENTCHANNELADMIN:PROXYPUSHCONSUMER-PROXY)
 (defmethod omg.org/features:connect_push_supplier
  ((obj coseventchanneladmin:proxypushconsumer-proxy) &rest args)
  (apply 'INVOKE OBJ "connect_push_supplier" ARGS))
 (DEFINE-USER-EXCEPTION COSEVENTCHANNELADMIN:TYPEERROR :id
  "IDL:omg.org/CosEventChannelAdmin/TypeError:1.0" :slots NIL)
 (DEFINE-USER-EXCEPTION COSEVENTCHANNELADMIN:ALREADYCONNECTED :id
  "IDL:omg.org/CosEventChannelAdmin/AlreadyConnected:1.0" :slots NIL))
