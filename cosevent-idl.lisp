(in-package :clorb)

(IDEF-DEFINITIONS
(with-prefix "omg.org"
(DEFINE-MODULE "CosEventChannelAdmin" NIL
 (DEFINE-EXCEPTION "AlreadyConnected" NIL)
 (DEFINE-EXCEPTION "TypeError" NIL)

 (DEFINE-INTERFACE "ProxyPushConsumer" (:BASES ("CosEventComm::PushConsumer"))
  (DEFINE-OPERATION "connect_push_supplier"
   ((:PARAM_IN "push_supplier" "CosEventComm::PushSupplier")) 
   :RESULT-TYPE VOID 
   :EXCEPTIONS ("CosEventChannelAdmin::AlreadyConnected")))

 (DEFINE-INTERFACE "ProxyPullSupplier" (:BASES ("CosEventComm::PullSupplier"))
  (DEFINE-OPERATION "connect_pull_consumer"
      ((:PARAM_IN "pull_consumer" "CosEventComm::PullConsumer")) 
    :RESULT-TYPE VOID
    :EXCEPTIONS ("CosEventChannelAdmin::AlreadyConnected")))

 (DEFINE-INTERFACE "ProxyPullConsumer" (:BASES ("CosEventComm::PullConsumer"))
  (DEFINE-OPERATION "connect_pull_supplier"
      ((:PARAM_IN "pull_supplier" "CosEventComm::PullSupplier")) 
    :RESULT-TYPE VOID 
    :EXCEPTIONS ("CosEventChannelAdmin::AlreadyConnected"
    "CosEventChannelAdmin::TypeError")))

 (DEFINE-INTERFACE "ProxyPushSupplier" (:BASES ("CosEventComm::PushSupplier"))
  (DEFINE-OPERATION "connect_push_consumer"
      ((:PARAM_IN "push_consumer" "CosEventComm::PushConsumer")) 
    :RESULT-TYPE VOID
    :EXCEPTIONS ("CosEventChannelAdmin::AlreadyConnected"
                 "CosEventChannelAdmin::TypeError")))
 (DEFINE-INTERFACE "ConsumerAdmin" (:BASES NIL)
   (DEFINE-OPERATION "obtain_push_supplier" NIL 
     :RESULT-TYPE "CosEventChannelAdmin::ProxyPushSupplier" 
     :EXCEPTIONS NIL)
   (DEFINE-OPERATION "obtain_pull_supplier" NIL 
     :RESULT-TYPE "CosEventChannelAdmin::ProxyPullSupplier" 
     :EXCEPTIONS NIL))

 (DEFINE-INTERFACE "SupplierAdmin" (:BASES NIL)
   (DEFINE-OPERATION "obtain_push_consumer" NIL 
     :RESULT-TYPE "CosEventChannelAdmin::ProxyPushConsumer" 
     :EXCEPTIONS NIL)
   (DEFINE-OPERATION "obtain_pull_consumer" NIL 
     :RESULT-TYPE "CosEventChannelAdmin::ProxyPullConsumer" 
     :EXCEPTIONS NIL))

 (DEFINE-INTERFACE "EventChannel" (:BASES NIL)
   (DEFINE-OPERATION "for_consumers" NIL 
     :RESULT-TYPE "CosEventChannelAdmin::ConsumerAdmin" 
     :EXCEPTIONS NIL)
   (DEFINE-OPERATION "for_suppliers" NIL 
     :RESULT-TYPE "CosEventChannelAdmin::SupplierAdmin" 
     :EXCEPTIONS NIL)
   (DEFINE-OPERATION "destroy" NIL 
     :RESULT-TYPE VOID 
     :EXCEPTIONS NIL))) 

(DEFINE-MODULE "CosEventComm" ()
  (DEFINE-EXCEPTION "Disconnected" NIL)

  (DEFINE-INTERFACE "PushConsumer" (:BASES NIL)
    (DEFINE-OPERATION "push" ((:PARAM_IN "data" ANY)) 
      :RESULT-TYPE VOID 
      :EXCEPTIONS ("CosEventComm::Disconnected"))
    (DEFINE-OPERATION "disconnect_push_consumer" NIL 
      :RESULT-TYPE VOID 
      :EXCEPTIONS NIL))

  (DEFINE-INTERFACE "PushSupplier" (:BASES NIL)
    (DEFINE-OPERATION "disconnect_push_supplier" NIL 
      :RESULT-TYPE VOID
      :EXCEPTIONS NIL))

  (DEFINE-INTERFACE "PullSupplier" (:BASES NIL)
    (DEFINE-OPERATION "pull" NIL 
      :RESULT-TYPE ANY 
      :EXCEPTIONS ("CosEventComm::Disconnected"))
    (DEFINE-OPERATION "try_pull" ((:PARAM_OUT "has_event" BOOLEAN)) 
      :RESULT-TYPE ANY
      :EXCEPTIONS ("CosEventComm::Disconnected"))
    (DEFINE-OPERATION "disconnect_pull_supplier" NIL 
      :RESULT-TYPE VOID
      :EXCEPTIONS NIL))

  (DEFINE-INTERFACE "PullConsumer" (:BASES NIL)
    (DEFINE-OPERATION "disconnect_pull_consumer" NIL 
      :RESULT-TYPE VOID
      :EXCEPTIONS NIL))))) 
