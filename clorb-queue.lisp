(in-package :clorb)

;; (enqf qplace object)
;; (deqf qplace)

(defmacro deqf (place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
                       (get-setf-expansion place env)
    (if (cdr store-vars) (error "Can't expand this"))
    (let ((queue (gensym))
          (result (gensym)))
      `(let* (,@(mapcar #'list vars vals)
              (,queue ,reader-form)
              (,result nil)
              ,@store-vars)
         (when ,queue
           (when (prog1 (eq ,queue (cdr ,queue))
                   (setq ,result (pop (cdr ,queue))))
             (setq ,(car store-vars) nil)
             ,writer-form))
         (values ,result (not (null ,queue)))))))


(define-modify-macro enqf (object) enq-func)
(defun enq-func (queue object)
  (cond ((null queue) 
         (let ((new (cons object nil)))
           (setf (cdr new) new)
           new))
        (t
         (let ((new (cons object (cdr queue))))
           (setf (cdr queue) new)
           new))))


(defun queue-memeber-p (queue item)
  (and queue
       (let ((pos queue))
         (loop (when (eql item (car queue))
                 (return t))
               (setq pos (cdr pos))
               (when (eql pos queue)
                 (return nil))))))


(defun queue-empty-p (queue)
  (null queue))
