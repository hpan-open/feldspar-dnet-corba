;;;; clorb-macros2.lisp
;; Macros for defining CORBA types, implementing the Lisp mapping.
;; define-struct
;; define-union
;; define-user-exception
;;

(in-package :clorb)



;;;; Alias 

#|
(DEFINE-ALIAS OMG.ORG/CORBA:REPOSITORYID
 :ID "IDL:omg.org/CORBA/RepositoryId:1.0"
 :NAME "RepositoryId"
 :TYPE OMG.ORG/CORBA:STRING
 :TYPECODE OMG.ORG/CORBA:TC_STRING)
|#

(defmacro DEFINE-ALIAS (symbol &key id name type typecode)
  `(progn (deftype ,symbol () ',type)
          (set-symbol-id/typecode ',symbol ,id 
                                  (make-tc-alias ,id ,name ,typecode))))

;;;; Enum

(defmacro define-enum (symbol &key id name members)
  (let ((keys (mapcar #'lispy-name members)))
    `(progn (deftype ,symbol () '(member ,@keys))
            (set-symbol-id/typecode ',symbol ,id
                                    (make-typecode :tk_enum ,id ,name
                                                   ',(coerce members 'vector))))))

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
             (defun ,name (&key ,@slots)
               ,(format nil "Construct CORBA struct ~A.~%Slots: ~S" name names)
               (make-instance ',name 
                 ,@(loop for key in names as val in slots
                         collect key collect val )))
             (defmethod type-id ((s ,name)) ,id)
             ,@getters1 ,@getters2 ,@setters
             (defmethod fields ((s ,name))
               (loop for f in ',names
                   for n in ',slots
                   when (slot-boundp s n)
                   collect (cons f (slot-value s n))))))))

(defmacro define-struct (symbol &key id (name "") members
                                 read write)
  "Define a CORBA structure with class, constructor, typecode etc.
  members = ((name type slot-name)*)
  read = ((buffer) unmarshallingcode*)
  write = ((obj buffer) marshallingcode*)"
  `(progn
     (define-corba-struct ,symbol :id ,id 
       :members ,(loop for (nil nil slot-name) in members
                       collect (list slot-name nil)))
     (set-symbol-ifr-id ',symbol ,id)
     (set-symbol-typecode 
      ',symbol
      (create-struct-tc ,id ,name
                          (list ,@(loop for (name type nil) in members 
                                        collect `(list ,name ,type)))))
     ,(if read
        (destructuring-bind ((buffer) &rest forms) read
          `(defmethod struct-read ((type (eql ',symbol)) ,buffer)
             ,@forms)))
     ,(if write
        (destructuring-bind ((obj buffer) &rest forms) write
          `(defmethod struct-write (,obj (symbol (eql ',symbol)) ,buffer)
             ,@forms)))))


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
       (defun ,symbol (&key union-value union-discriminator)
         (make-instance ',symbol 
           :value union-value
           :discriminator union-discriminator))
       (set-symbol-id/typecode ',symbol ,id
                               (create-union-tc ,id ,name
                                                ,discriminator-type
                                                (list ,@(nreverse tc-members))))
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
    for slot = (feature slot-name)
    collect (list slot :initarg initarg) into slot-defs
    collect `(define-method ,slot ((s ,symbol)) (slot-value s ',slot)) ; FIXME: not quite ANSI
    into getters
    collect `(list ,slot-name ,(second member)) into tc-members
    collect slot into args
    append (list initarg slot) into initargs
    finally
    (return
     `(progn
        (define-condition ,symbol (CORBA:UserException)
                          (,@slots ,@slot-defs))
        ,@getters
        (defun ,symbol (&key ,@args)
          (make-condition ',symbol ,@initargs))
        (set-symbol-id/typecode ',symbol ,id
                                (make-typecode :tk_except ,id ,name (list ,@tc-members)))
        (defmethod exception-name ((exc ,symbol)) ',symbol)
        (defmethod userexception-values ((ex ,symbol))
          (list ,@(mapcar (lambda (slot-spec) `(slot-value ex ',(car slot-spec)))
                          slot-defs)))))))


;;;; Stub generation

(defmacro static-call ((op obj) &key output input exceptions)
  (let ((req '#:REQ) 
        (status '#:status)
        (output-buf (caar output))
        (input-buf (caar input)))
    `(loop
       (let (,req)
         (let (,output-buf)
           ,@(if (and output-buf (null (cdr output)))
               `((declare (ignorable ,output-buf))))
           (multiple-value-setq (,req ,output-buf) (start-request ,op ,obj ,(null input)))
           ,@(cdr output))
         (multiple-value-bind (,status ,input-buf) (invoke-request ,req)
           (case ,status
             (:no_exception
              (return (values ,@(cdr input))))
             (:user_exception
              (process-exception ,input-buf ',exceptions))))))))

;;;; Interface

(defmacro define-interface (symbol super &key (id "") proxy (name ""))
  `(progn
     (set-symbol-id/typecode ',symbol ,id 
                             (make-typecode :tk_objref ,id ,name))
     (defclass ,symbol ,super ())
     ,@(if proxy
         `((defclass ,(car proxy) ,(cdr proxy) ())
           (register-proxy-class ,id ',(car proxy))))
     (defmethod object-id ((obj ,symbol))
       ,id)
     (defmethod object-is-a or ((obj ,symbol) interface-id)
       (string= interface-id ,id))))

