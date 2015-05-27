;;; ec-server.lisp  --  CORBA Event Channel

(in-package :net.cddr.clorb.event)


;;;; Utilities

(defun delete-swap (list object)
  (delete object list))

(define-modify-macro purge (object) delete-swap  )


(defmacro with-cache-slot ((object slot) &body body)
  (let ((objvar (gensym)))
    `(let ((,objvar ,object))
       (if (slot-boundp ,objvar ',slot)
         (slot-value ,objvar ',slot)
         (setf (slot-value ,objvar ',slot) (progn ,@body))))))



;;;; EVENT-CHANNEL CLASS ---------------------------------------------------

;;;  ------------------------------------------------- event-channel -------

(defclass EVENT-CHANNEL (CosEventChannelAdmin:EventChannel-servant)
  ((consumers           :initform '()   :accessor consumers-of)
   (suppliers           :initform '()   :accessor suppliers-of)
   (consumer-admin  )
   (supplier-admin  )))

(define-method for_consumers ((servant event-channel))
  (with-cache-slot (servant consumer-admin)
    (make-instance 'consumer-admin :channel servant)))

(define-method for_suppliers ((servant event-channel))
  (with-cache-slot (servant supplier-admin)
    (make-instance 'supplier-admin :channel servant)))

(define-method destroy ((servant event-channel))
  (values))


(defgeneric add-event (recipient event))

(defmethod add-event ((ch event-channel) event)
  (mapc (lambda (subs) (add-event subs event))
        (consumers-of ch)))



;;;; ------------------------------------------------- consumer-admin ------

(defclass CONSUMER-ADMIN (CosEventChannelAdmin:ConsumerAdmin-servant)
  ((channel :initarg :channel :reader channel)))

(define-method obtain_push_supplier ((servant consumer-admin))
  (make-instance 'push-supplier :channel (channel servant)))

(define-method obtain_pull_supplier ((servant consumer-admin))
  (make-instance 'pull-supplier :channel (channel servant)))


;;;; ------------------------------------------------- supplier-admin ------

(defclass SUPPLIER-ADMIN (CosEventChannelAdmin:SupplierAdmin-servant)
  ((channel :initarg :channel :reader channel)))

(define-method obtain_push_consumer ((servant supplier-admin))
  (make-instance 'push-consumer :channel (channel servant)))

(define-method obtain_pull_consumer ((servant supplier-admin))
  (make-instance 'pull-consumer :channel (channel servant)))


;;;; SUPPLIER CLASSES ----------------------------------------------------

(defclass SUPPLIER () 
  ((channel :initarg :channel :accessor channel)
   (consumer :initform nil  :accessor consumer-of)))

(defmethod connect-consumer ((self supplier) consumer)
  (when (consumer-of self)
    (error (coseventchanneladmin:alreadyconnected)))
  (setf (consumer-of self) consumer)
  (pushnew self (consumers-of (channel self))))

(defmethod disconnect ((self supplier))
  (setf (consumer-of self) nil)
  (purge (consumers-of (channel self)) self)
  (op:deactivate_object (op:_poa self) (op:_object_id self)))


;;;; ------------------------------------------------- push-supplier -------

(defclass PUSH-SUPPLIER (supplier
                         CosEventChannelAdmin:ProxyPushSupplier-servant)
  ())

(defmethod add-event ((self push-supplier) event)
  (ignore-errors
   (op:push (consumer-of self) event)))

(define-method connect_push_consumer ((self push-supplier) push-consumer)
  (connect-consumer self push-consumer))

(define-method disconnect_push_supplier ((self push-supplier))
  (disconnect self))


;;;; --------------------------------------------------pull-supplier -------

(defclass PULL-SUPPLIER (supplier
                         CosEventChannelAdmin:ProxyPullSupplier-servant)
  ((pending-pull :initform nil  :accessor pending-pull)
   (events       :initform '()  :accessor events)))

(defmethod add-event ((self pull-supplier) event)
  (let ((sreq (pending-pull self)))
    (if sreq
        (progn (setf (pending-pull self) nil)
               (clorb::set-request-result-list sreq (list event))
               (clorb::request-respond sreq))
        (setf (events self)
              (nconc (events self) (list event))))))


(define-method pull ((self pull-supplier))
  (if (events self)
      (pop (events self))
    (progn (setf (pending-pull self) (clorb::current-request))
           (throw 'clorb::defer nil))))

(define-method try_pull ((self pull-supplier))
  (let ((res (load-time-value (CORBA:Any :any-typecode CORBA:tc_null)))
        (has-event nil))
    (when (events self)
      (setq res (pop (events self)))
      (setq has-event t))
    (values res has-event)))

(define-method connect_pull_consumer ((servant pull-supplier) pull-consumer)
  (connect-consumer servant pull-consumer))

(define-method disconnect_pull_supplier ((pull-supplier pull-supplier))
  (disconnect pull-supplier))


;;;; CONSUMER CLASSES ------------------------------------------------------

(defclass CONSUMER ()
  ((state :initarg :channel :accessor channel)
   (supplier :accessor supplier-of)))

(defmethod connect-supplier ((self consumer) supplier)
  (when (slot-boundp self 'supplier)
    (error (coseventchanneladmin:alreadyconnected)))
  (push self (suppliers-of (channel self)))
  (setf (supplier-of self) supplier))

(defmethod disconnect ((self consumer))
  (slot-makunbound self 'supplier)
  (purge (suppliers-of (channel self)) self)
  (op:deactivate_object (op:_poa self) (op:_object_id self)))


;;;; ------------------------------------------------- push-consumer -------

(defclass PUSH-CONSUMER (consumer
                         CosEventChannelAdmin:ProxyPushConsumer-servant)
  ())

(define-method push ((self push-consumer) any)
  (add-event (channel self) any))

(define-method connect_push_supplier ((self push-consumer) push-supplier)
  (connect-supplier self push-supplier))

(define-method disconnect_push_consumer ((self push-consumer))
  "Disconnect push consumer and clean up"
  (disconnect self))



;;;; ------------------------------------------------- pull-consumer -------

(defclass PULL-CONSUMER (consumer
                         CosEventChannelAdmin:ProxyPullConsumer-servant)
  ())

(define-method connect_pull_supplier ((self pull-consumer) supplier)
  ;; FIXME: register for poll or start thread?
  (connect-supplier self supplier))

(defun do-pull (pull-consumer)
  (let ((event (op:pull (supplier-of pull-consumer))))
    (add-event (channel pull-consumer) event)))

(define-method disconnect_pull_consumer ((self pull-consumer))
  (disconnect self))


;;;; Misc ------------------------------------------------------------------

(defvar *my-ch* nil)

;;(defvar *my-pus* (let ((subs (op::obtain_pull_supplier *my-ch*)))
;;                   (op::connect_pull_consumer subs nil)
;;                   subs))

(defclass PUSH-TRACE (push-consumer) ())
(define-method push ((servant push-trace) event)
  (format t "~&>>> Push ~S~%" event))

(defun setup-ec ()
  (unless *my-ch*
    (setq *my-ch* (make-instance 'event-channel)))
  (clorb:rebind *my-ch* "ch1")
  ;;(clorb:rebind *my-pus* "ch1b")
  (clorb:rebind (make-instance 'push-consumer :channel *my-ch*) "ch1pc")
  (clorb:rebind (make-instance 'push-supplier :channel *my-ch*) "ch1ps"))

(defun test-ec ()
  (let ((consumer (make-instance 'push-trace))
        (ch (op:resolve "ch1")))
    (let ((in-adm (op:for_suppliers ch))
          (out-adm (op:for_consumers ch)))
      (let ((in (op:obtain_push_consumer in-adm))
            (out (op:obtain_push_supplier out-adm)))
        (op:connect_push_consumer out consumer)
        (op:push in "Tjolla hoppsan")
        (op:push in 345)
        (op:push in '("how" "do"))
        (op:disconnect_push_supplier out)))))

(defun test-ec-1 ()
  (let ((ch (op:resolve "ch1")))
    (let ((in-adm (op:for_suppliers ch)))
      (let ((in (op:obtain_push_consumer in-adm)))
        ;; ? (op:connect_push_supplier in ?)
        (op:push in "Tjolla hoppsan")
        (op:push in 345)
        (op:push in '("how" "do")) ))))

(defun test-ec-2 ()
  (let ((consumer (make-instance 'push-trace))
        (ch (op:resolve "ch1")))
    (let ((out-adm (op:for_consumers ch)))
      (let ((out (op:obtain_push_supplier out-adm)))
        (op:connect_push_consumer out consumer)
        (unwind-protect
             (op:run (corba:orb_init))
          (op:disconnect_push_supplier out))))))
