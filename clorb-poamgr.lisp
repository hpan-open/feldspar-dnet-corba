;;;; clorb-poamgr.lisp --- POA Manager
;; $Id: clorb-poamgr.lisp,v 1.4 2002/11/20 16:42:06 lenst Exp $

(in-package :clorb)

;;; interface POAManager

(defclass PortableServer:POAManager (CORBA:Object)
  ((state :initform :holding)))


;;; enum State {HOLDING, ACTIVE, DISCARDING, INACTIVE}
(DEFINE-ENUM OMG.ORG/PORTABLESERVER:POAMANAGER/STATE
 :ID "IDL:omg.org/PortableServer/POAManager/State:1.0"
 :NAME "State"
 :MEMBERS ("HOLDING" "ACTIVE" "DISCARDING" "INACTIVE"))


;;; exception AdapterInactive{};
(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POAMANAGER/ADAPTERINACTIVE
 :ID "IDL:omg.org/PortableServer/POAManager/AdapterInactive:1.0"
 :NAME "AdapterInactive"
 :MEMBERS NIL)


(defun POAManager-new-state (pm new-state)
  (with-slots (state) pm
    (when (eq state :inactive)
      (error 'POAManager/AdapterInactive))
    (setf state new-state)))


;;; void activate()
;;;	raises(AdapterInactive);
(define-method activate ((pm PortableServer:POAManager))
  (POAManager-new-state pm :active))


;;; void hold_requests(in boolean wait_for_completion)
;;;     raises(AdapterInactive);
(define-method hold_requests ((pm PortableServer:POAManager) wait_for_completion)
  (POAManager-new-state pm :holding))


;;; void discard_requests(in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method discard_requests ((pm PortableServer:POAManager) wait_for_completion)
  (POAManager-new-state pm :discarding))


;;; void deactivate(	in boolean etherealize_objects,
;;;                     in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method deactivate ((pm PortableServer:POAManager) etherealize_objects 
                           wait_for_completion)
  (POAManager-new-state pm :inactive))


;;; State get_state ()
(define-method get_state ((pm PortableServer:POAManager))
  (slot-value pm 'state))



;;; clorb-poamgr.lisp ends here
