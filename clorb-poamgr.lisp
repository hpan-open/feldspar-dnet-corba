;;;; clorb-poamgr.lisp --- POA Manager
;; $Id: clorb-poamgr.lisp,v 1.3 2002/05/29 05:10:34 lenst Exp $

(in-package :clorb)

;;; interface POAManager

(defclass POAManager ()
  ((state :initform :holding)))


;;; exception AdapterInactive{};

(define-user-exception POAManager/AdapterInactive
    :id "IDL:omg.org/PortableServer/POAManager/AdapterInactive:1.0")

;;; enum State {HOLDING, ACTIVE, DISCARDING, INACTIVE}
(defparameter +valid-states+ '(:holding :active :discarding :inactive))
(deftype POAManager/State ()
  `(member ,@+valid-states+))

(defun POAManager-new-state (pm new-state)
  (with-slots (state) pm
    (when (eq state :inactive)
      (error 'POAManager/AdapterInactive))
    (setf state new-state)))


;;; void activate()
;;;	raises(AdapterInactive);
(define-method activate ((pm POAManager))
  (POAManager-new-state pm :active))


;;; void hold_requests(in boolean wait_for_completion)
;;;     raises(AdapterInactive);
(define-method hold_requests ((pm POAManager) wait_for_completion)
  (POAManager-new-state pm :holding))


;;; void discard_requests(in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method discard_requests ((pm POAManager) wait_for_completion)
  (POAManager-new-state pm :discarding))


;;; void deactivate(	in boolean etherealize_objects,
;;;                     in boolean wait_for_completion)
;;;        raises(AdapterInactive);
(define-method deactivate ((pm POAManager) etherealize_objects 
                                           wait_for_completion)
  (POAManager-new-state pm :inactive))


;;; State get_state ()
(define-method get_state ((pm POAManager))
  (slot-value pm 'state))



;;; clorb-poamgr.lisp ends here
