
(defgeneric getdict (key dict &optional default))
(defgeneric (setf getdict) (newval key dict)) 
(defgeneric remdict (key dict))
(defgeneric mapdict (function dict))

;; Assoc  (cons)
;;  for setf to work needs a dummy element first

(defmethod getdict (key (alist cons) &optional default)
  (let ((pair (assoc key alist)))
    (if pair 
        (values (cdr pair) t)
      (values default nil))))

(defmethod (setf getdict) (newval key (alist cons))
  (let ((pair (assoc key alist)))
    (cond ((null pair)
           (setq pair (cons key newval))
           (push pair (cdr alist))
           newval)
          (t
           (setf (cdr pair) newval)))))

(defmethod remdict (key (dict cons))
  (loop for head on dict
      when (and (consp (cadr head))
                (eql key (caadr head)))
      return (prog1 (cadr head)
               (setf (cdr head) (cddr head)))))

(defmethod mapdict (function (dict cons))
  (loop for pair in dict
      when (consp pair)
      do (funcall function (car pair) (cdr pair))))


;;;; Hash table dictionaries

(defmethod getdict (key (htable hash-table) &optional default)
  (gethash key htable default))

(defmethod (setf getdict) (newval key (htable hash-table))
  (setf (gethash key htable) newval))

(defmethod remdict (key (htable hash-table))
  (remhash  key htable))

(defmethod mapdict (function (htable hash-table))
  (maphash function htable))


(defmethod keys ((htable hash-table))
  (loop for k being the hash-keys of htable collect k))
