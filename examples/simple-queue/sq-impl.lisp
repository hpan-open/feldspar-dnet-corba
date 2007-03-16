(in-package :net.cddr.clorb)


(defclass simple-queue-impl (CDDR_01:SimpleQueue-servant synchronized)
  ((queue       :initarg :queue         :accessor queue-of
                :initform nil)
   (waiting     :initarg :waiting       :accessor waiting-of
                :initform nil)
   (next-id     :initarg :next-id       :accessor next-id-of
                :initform 1)
   (memory      :initarg :memory        :accessor memory-of
                :initform (make-hash-table))))



(defun sq-notify (self)
  (let ((wq (shiftf (waiting-of self) nil)))
    (loop until (queue-empty-p wq)
        do (funcall (deqf wq)))))


(defun %enqueue (self msg)
  (enqf (queue-of self) msg)
  (sq-notify self))



;; void enqueue (in string msg);

(define-method enqueue ((self simple-queue-impl) msg)
  (with-synchronization self
    (%enqueue self msg)))
  


;; string dequeue (in boolean non_blocking, out boolean flag);

(define-method dequeue ((self simple-queue-impl) non-blocking)
  (with-synchronization self
    (labels ((deq (result defer)
               (cond ((queue-of self)
                      (funcall result (deqf (queue-of self)) t))
                     (non-blocking
                      (funcall result "" nil))
                     (t (funcall defer)))))
      (deq #'values
           (lambda ()
             (let ((r (current-request)))
               (labels ((retry ()
                          (deq (lambda (&rest result)
                                 (set-request-result-list r result))
                               #'defer))
                        (defer ()
                          (enqf (waiting-of self) #'retry)))
                 (defer)
                 (throw 'defer nil))))))))


;; long alloc_id (in long n);

(define-method alloc_id ((self simple-queue-impl) n)
  (with-synchronization self
    (prog1 (next-id-of self)
      (incf (next-id-of self) n))))


;; boolean put (in long id, in string msg);

(define-method put ((self simple-queue-impl) id msg)
  (with-synchronization self
    (multiple-value-bind (old-obj flg) (gethash id (memory-of self))
      (declare (ignore old-obj))
      (unless flg
        (setf (gethash id (memory-of self)) msg)
        (%enqueue self msg)
        t))))


;; string get (in long id);

(define-method get ((self simple-queue-impl) id)
  (with-synchronization self
    (flet ((%get (self id)
             (multiple-value-bind (obj flg) (gethash id (memory-of self))
               (cond (flg obj)
                     ((not (queue-empty-p (queue-of self)))
                      (let ((obj (deqf (queue-of self))))
                        (setf (gethash id (memory-of self)) obj)
                        obj))
                     (t 
                      'defer)))))
      (let ((result (%get self id)))
        (cond ((eql result 'defer)
               (let ((r (current-request)))
                 (labels 
                   ((defer ()
                      (enqf (waiting-of self) #'retry))
                    (retry ()
                      (let ((result (%get self id)))
                        (if (eql result 'defer)
                          (defer)
                          (set-request-result-list r (list result))))))
                   (defer)
                   (throw 'defer nil))))
              (t
               result))))))


;; void ack (in long id);

(define-method ack ((self simple-queue-impl) id)
  (with-synchronization self
    (remhash id (memory-of self))))


;;;

(defun simple-queue-setup (&key name boot-name 
                           (orb (CORBA:ORB_init))
                           (poa (op:resolve_initial_references
                                 orb "RootPOA")))
  (assert (or name boot-name))
  (let* ((servant (make-instance 'simple-queue-impl))
         (oid (op:activate_object poa servant))
         (oref (op:servant_to_reference poa servant)))
    (declare (ignore oid))
    (when name
      (op:rebind name oref))
    (when boot-name
      (set-boot-object orb boot-name oref))
    oref))
          


;;(simple-queue-setup :name "q3")
