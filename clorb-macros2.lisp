;;;; clorb-macros2.lisp
;; Macros for defining CORBA types, implementing the Lisp mapping.
;; define-struct
;; define-union
;; define-user-exception
;;

(in-package :clorb)


;;;; Struct Macrology

(defmacro define-corba-struct (name &key id members)
  (loop
      for member in members
      for slot = (car member)
      for field = (lispy-name (symbol-name slot))
      collect field into names
      collect slot into slots
      collect (list* slot :initarg field :initform (second member)
                     (cddr member))
      into slot-defs
      collect `(defmethod struct-get ((s ,name) (field (eql ,field)))
                 (slot-value s ',slot))
      into getters1
      collect `(define-method ,slot ((s ,name)) (slot-value s ',slot))
      into getters2
      collect `(define-method (setf ,slot) (val (s ,name))
                 (setf (slot-value s ',slot) val))
      into setters
      finally
        (return
          `(progn
             (defclass ,name (CORBA:struct) ,slot-defs)
             (defun ,name (&rest initargs)
               ,(format nil "Construct CORBA struct ~A.~%Slots: ~S" name names)
               (apply #'make-instance ',name initargs))
             (defmethod type-id ((s ,name)) ,id)
             ,@getters1 ,@getters2 ,@setters
             (defmethod fields ((s ,name))
               (loop for f in ',names
                   for n in ',slots
                   when (slot-boundp s n)
                   collect (cons f (slot-value s n))))
             (add-struct-class ',name ,id)))))

(defmacro define-struct (symbol &key id (name "") members)
  "  members = (name type slot-name)
"
  `(progn
     (define-corba-struct ,symbol :id ,id 
       :members ,(loop for (nil nil slot-name) in members
                       collect (list slot-name nil)))
     (set-symbol-ifr-id ',symbol ,id)
     (set-symbol-typecode 
      ',symbol
      (lambda ()
        (create-struct-tc ,id ,name
                          (list ,@(loop for (name type nil) in members 
                                        collect `(list ,name ,type))))))))


;;;; Union macrology

(defmacro define-union (symbol &key id (name "") discriminator-type members)
  "members = (label type &key creator name default)*
     where labels = () is default member"
  (let ((used-names '())
        (code '())
        (tc-members '()) )
    (dolist (m members)
      (destructuring-bind (label type &key creator (name "") default
                                 (accessor (string-upcase name))) m
        (push `(list ,(if default ''default label) ,name ,type) 
              tc-members)
        (unless (member name used-names :test #'equal)
          (push name used-names)
          (push `(progn 
                   (define-method ,accessor ((obj ,symbol))
                     (union-value obj))
                   (define-method (setf ,accessor) (value (obj ,symbol))
                     (setf (union-discriminator obj) ,label)
                     (setf (union-value obj) value))
                   ,(if creator
                      `(defun ,creator (value)
                         (,symbol :union-value value :union-discriminator ,label))))
                code)
          (when default
            (push `(progn 
                     (define-method default ((obj ,symbol)) (union-value obj))
                     (define-method (setf default) (value (obj ,symbol))
                       (setf (union-discriminator obj) ,label)
                       (setf (union-value obj) value))) code)))))
    
    `(progn
       (defclass ,symbol (corba:union) ())
       (setf (gethash ,id *union-registry*) ',symbol)
       (defun ,symbol (&key union-value union-discriminator)
         (make-instance ',symbol 
           :value union-value
           :discriminator union-discriminator))
       (set-symbol-ifr-id ',symbol ,id)
       (set-symbol-typecode ',symbol
                            (lambda ()
                              (create-union-tc ,id ,name
                                               ,discriminator-type
                                               (list ,@(nreverse tc-members)))))
       ,@code)))


#|
(define-union omg.org/root::filter :name "filter" :id "idl:filter.1.0"
  :discriminator-type corba:tc_long
  :members ((0 corba:tc_string :name "foo" :creator filter/foo))
)
|#


;;;; Exceptions 

(defmacro define-user-exception (symbol &key (name "") (id "") slots members)
  "Syntax: scoped-symbol
id: repo-id
name: repo-name
Slots: deprecated
Members: (name typecode)*"
  (assert (null slots))
  (loop
    for member in members
    for slot-name = (string (car member))
    for initarg = (lispy-name slot-name)
    for slot = (intern (symbol-name initarg))  ; FIXME: or make-symbol ??
    collect (list slot :initarg initarg)
    into slot-defs
    collect `(define-method ,initarg ((s ,symbol)) (slot-value s ',slot)) ; FIXME: not quite ANSI
    into getters
    collect `(list ,slot-name ,(second member)) 
    into tc-members
    finally
    (return
     `(progn
        (define-condition ,symbol (CORBA:UserException)
                          (,@slots ,@slot-defs))
        ,@getters
        (setf (gethash ,id *user-exception-classes*) ',symbol)
        (defun ,symbol (&rest initargs)
          (apply #'make-condition ',symbol initargs))
        (set-symbol-ifr-id ',symbol ,id)
        (set-symbol-typecode ',symbol 
                             (lambda () (make-typecode :tk_except ,id ,name (list ,@tc-members))))
        (defmethod exception-name ((exc ,symbol)) ',symbol)
        (defmethod userexception-values ((ex ,symbol))
          (list ,@(mapcar (lambda (slot-spec) `(slot-value ex ',(car slot-spec)))
                          slot-defs)))))))

