(in-package :user)

(defconstant +trie-key-max+ 8)

(defun make-trie ()
  (cons nil nil))

(defun trie-get (key trie)
  (map nil
    (lambda (k)
      (let ((v (cdr trie)))
        (cond
         ((and (consp v) (eq k (car v)))
          (setq trie (cdr v)))
         ((vectorp v)
          (setq trie (aref v k)))
         (t
          (return-from trie-get nil)))))
    key)
  (car trie))

(defun trie-set (key trie val)
  (map nil
    (lambda (k)
      (let ((v (cdr trie)))
        (cond
         ((consp v)
          (if (eq k (car v))
              (setq trie (cdr v))
            (let ((a (make-array +trie-key-max+)))
              (setf (cdr trie) a)
              (setf (aref a (car v)) (cdr v))
              (setf (aref a k) (setf trie (cons nil nil))))))
         ((vectorp v)
          (setq trie (aref v k))
          (unless trie
            (setf (aref v k) (setf trie (cons nil nil)))))
         ((null v)
          (setf (cdr trie) (list k nil))
          (setf trie (cddr trie)))
         (t (error "SHOULDN'T")))))
    key)
  (setf (car trie) val))

(defun trie-remove (key trie)
  (map nil
    (lambda (k)
      (let ((v (cdr trie)))
        (cond
         ((and (consp v) (eq k (car v)))
          (setq trie (cdr v)))
         ((vectorp v)
          (setq trie (aref v k)))
         (t
          (return-from trie-remove nil)))))
    key)
  (when trie
    (prog1 (car trie)
      (setf (car trie) nil))))


(defun maptrie2 (function trie)
  (declare (type (function (sequence t)) function))
  (let ((key (make-array 20 :adjustable t :fill-pointer 0)))
    (labels ((rec (trie)
               (when (car trie)
                 (funcall function key (car trie)))
               (let ((v (cdr trie)))
                 (cond ((consp v)
                        (vector-push-extend (car v) key)
                        (rec (cdr v))
                        (decf (fill-pointer key)))
                       ((vectorp v)
                        (loop with p = (vector-push-extend 0 key)
                            for i below (length v)
                            for u = (aref v i)
                            when u
                            do (setf (aref key p) i)
                               (rec u))
                        (decf (fill-pointer key)))))))
      (rec trie))))



(defparameter a-trie
    '(nil 1 nil 0 nil . #((nil 0 hello))))

(assert (eq (trie-get '#(1 0 0 0) a-trie) 'HELLO)) 
(trie-set '#(2 0 0 0) a-trie 'new-val)
(assert (eq (trie-get '#(1 0 0 0) a-trie) 'HELLO)) 
(assert (eq (trie-get '#(2 0 0 0) a-trie) 'NEW-VAL))

(trie-set '#(2 0 0 4) a-trie 'four)
(assert (let ((v nil))
          (maptrie2 (lambda (key val)
                      (push (cons (coerce key 'list) val) v))
                    a-trie)
          (equal v '(((2 0 0 4) . four) 
                     ((2 0 0 0) . new-val)
                     ((1 0 0 0) . hello)))))

(assert (trie-remove '#(1 0 0 0) a-trie))
(assert (null (trie-get '#(1 0 0 0) a-trie)))
(assert (null (trie-remove '#(1 0 0 0) a-trie)))
(assert (eq (trie-get '#(2 0 0 0) a-trie) 'NEW-VAL))

#||
||#
