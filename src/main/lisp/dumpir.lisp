(in-package :clorb)

(defun dump-ir (&optional (wr *standard-output*))
  (with-standard-io-syntax
      (let ((*package* (find-package :clorb))
            (counter 0))
        (loop for hash-table in (list *repository* *typecode-repository*)
              do (let ((alist '()))
                   (maphash (lambda (k v)
                              (push (cons k v) alist)
                              (incf counter))
                            hash-table)
                   (write alist :circle t :stream wr)))
        counter)))

(defun save-ir ()
  (with-open-file (wr "ir.dump" :direction :output :if-exists :supersede)
    (values   
     (dump-ir wr)
     (file-position wr))))

#+Allegro
(defun save-ir-fasl ()
  (excl:fasl-write (list *repository* *typecode-repository*)
                   "ird.fasl"
                   t t))

(defun load-ir ()
  (with-open-file (rd "ir.dump" :direction :input)
    (with-standard-io-syntax
        (let ((*package* (find-package :clorb))
              (counter 0))
          (loop for hash-table in (list *repository* *typecode-repository*)
                do (mapc (lambda (pair)
                           (setf (gethash (car pair) hash-table) (cdr pair))
                           (incf counter))
                         (read rd)))
          counter))))
