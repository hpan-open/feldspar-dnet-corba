;;;; clorb-mt.lisp -- System independent multi-threading and synchronization

(in-package :clorb)


(defgeneric lock (obj)
  (:documentation
   "Return the synchronization lock for object"))

(defgeneric waitqueue (obj)
  (:documentation
   "Return the waitqueue/semaphore/condition etc. for the object."))


(defclass synchronized ()
  ((lock       :initform (make-lock "syn")   :reader lock)
   (waitqueue  :initform (make-waitqueue)    :reader waitqueue)))


(defmacro with-synchronization (obj &body body)
  `(with-lock (lock ,obj)
     ,@body))


(defun synch-wait (obj)
  "Synchronize wait on unlocked object. 
Object should have (lock _) and (waitqueue _)"
  (wq-unlocked-wait (waitqueue obj) (lock obj)))


(defun synch-locked-wait (obj)
  "Synchronize wait on locked object. 
Object should have (lock _) and (waitqueue _)"
  (wq-locked-wait (waitqueue obj) (lock obj)))


(defun synch-notify (obj)
  (wq-notify (waitqueue obj)))


(defun synch-wait-on-condition (obj wait-func &rest wait-args)
  (with-synchronization obj
    (apply #'wq-locked-wait-on-condition
           (waitqueue obj) (lock obj)
           wait-func wait-args)))



;;;; Shared Queue


(defclass shared-queue (synchronized)
  ((queue  :initform nil  :accessor queue)))


(defmethod enqueue ((q shared-queue) obj)
  (with-synchronization q
    (enqf (queue q) obj)
    (wq-notify (waitqueue q)))
  t)


(defmethod dequeue ((q shared-queue) &optional non-blocking)
  (with-synchronization q
    (if non-blocking
        (deqf (queue q))
        (loop
           (if (queue-empty-p (queue q))
               (wq-locked-wait (waitqueue q) (lock q))
               (return (deqf (queue q))))))))



;;;; Execution Queue


(defclass execution-queue (synchronized)
  ((max-processes :initarg :max-processes :initform 10   :accessor max-processes)
   (max-handler   :initarg :max-handler   :initform nil  :accessor max-handler)
   (max-idle      :initarg :max-idle      :initform nil  :accessor max-idle)
   (queue                                 :initform nil  :accessor eq-queue)
   (idle-count                            :initform 0    :accessor idle-count)
   (idle-list     :initarg :idle-list     :initform nil  :accessor idle-list)
   (process-list  :initarg :process-list  :initform nil  :accessor process-list)
   (process-count :initarg :process-count :initform 0    :accessor process-count)
   (executor      :initarg :executor  :initform 'eq-exec :accessor executor)
   (name-template :initarg :name-template :initform "eq" :accessor name-template)
   (name-count    :initarg :name-count    :initform 0    :accessor name-count)))


(defmacro with-execution-queue (q &body body)
  `(with-accessors ((max-processes max-processes)
                    (max-handler max-handler)
                    (max-idle max-idle)
                    (queue eq-queue) (idle-count idle-count)
                    (process-list process-list) (process-count process-count)
                    (executor executor) (name-template name-template)
                    (name-count name-count) (idle-list idle-list))
                   ,q
     ,@body))


(defmethod next-process-name ((q execution-queue))
  (with-execution-queue q
    (format nil "~A ~D" name-template (incf name-count))))


(defun eq-main (q obj)
  (with-execution-queue q
    (let ((process (current-process)))
      (loop
        (flet ((exit ()
                 (decf process-count)
                 (deletef process process-list)
                 (return))
               (idle ()
                 (incf idle-count)
                 (push process idle-list)
                 (loop
                    (synch-locked-wait q)
                    (unless (queue-empty-p queue)
                      (decf idle-count)
                      (deletef (current-process) idle-list)
                      (return)))))
          (handler-case
            (funcall executor obj)
            (serious-condition (condition)
                               (mess 4 "Thread fails: ~A thr ~A"
                                     condition (current-process))
                               (with-synchronization q (exit))))
          (with-synchronization q
            (when (and (queue-empty-p queue)
                       (or (null max-idle) (< idle-count max-idle)))
              (idle))
            (if (queue-empty-p queue) 
              (exit)
              (setq obj (deqf queue)))))))))



(defun eq-exec (obj)
  (etypecase obj
    (function (funcall obj))
    (cons (apply (car obj) (cdr obj)))))



(defmethod enqueue ((q execution-queue) obj)
  (with-execution-queue q
    (with-synchronization q
      (flet ((ok-make-more-processes ()
               (or (< process-count max-processes)
                   (if max-handler
                     (funcall max-handler q obj)))))
        (cond ((and (zerop idle-count) (ok-make-more-processes))
               (let ((p (start-process (next-process-name q) #'eq-main q obj)))
                 (push p process-list)
                 (incf process-count)
                 t))
              (t 
               (enqf queue obj)
               (synch-notify q)
               nil))))))


(defun garb-threads (q)
  (with-execution-queue q
    (with-synchronization q
      (setq process-list
            (remove-if (lambda (p)
                         (unless (process-running-p p)
                           (decf process-count)
                           t))
                       process-list)))))
