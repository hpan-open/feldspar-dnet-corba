;;;; Exceptions

(in-package :clorb)


(define-condition corba:exception (serious-condition)
  ((id :initform "IDL:omg.org/CORBA/Exception:1.0"
       :initarg :id
       :reader exception-id)))

(deftype CORBA::completion_status ()
  '(member :COMPLETED_YES :COMPLETED_NO :COMPLETED_MAYBE))

;; Map from id to class
(defvar *system-execption-classes* 
  (make-hash-table :test #'equal))


(define-condition corba:systemexception (error corba:exception)
  ((minor :initform 0
	  :initarg :minor
	  :reader system-exception-minor)
   (completed :initform :completed_maybe
	      :initarg :completed
              :type CORBA::completion_status
	      :reader system-exception-completed))
  (:report report-systemexception)
  #-clisp
  (:default-initargs
      :id "IDL:omg.org/CORBA/SystemException:1.0"))





(macrolet
    ((define-system-exceptions (&rest excnames)
         `(progn
            ,@(loop for name in excnames collect
                    (let* ((namestr (string name))
                           (id (format nil "IDL:omg.org/CORBA/~A:1.0" namestr))
                           (sym (intern namestr :CORBA)))
                      `(progn
                         (define-condition ,sym (corba:systemexception)
                                           (#+clisp
                                            (id :initform ,id))
                           #-clisp
                           (:default-initargs
                             :id ,id))
                         (setf (gethash ,id *system-execption-classes*) ',sym) ))))))
  (define-system-exceptions
      UNKNOWN BAD_PARAM NO_MEMORY IMP_LIMIT
      COMM_FAILURE INV_OBJREF NO_PERMISSION INTERNAL MARSHAL
      INITIALIZE NO_IMPLEMENT BAD_TYPECODE BAD_OPERATION
      NO_RESOURCES NO_RESPONSE PERSIST_STORE BAD_INV_ORDER
      TRANSIENT FREE_MEM INV_IDENT INV_FLAG INTF_REPOS
      BAD_CONTEXT OBJ_ADAPTER DATA_CONVERSION OBJECT_NOT_EXIST
      TRANSACTION_REQUIRED TRANSACTION_ROLLEDBACK INVALID_TRANSACTION ))



;;;; User Exceptions
;;;
;;; exception-id exc        --> ifr-id
;;; exception-typecode exc  --> typecode
;;; id-exception-class id   --> exception-class
;;;

(define-condition corba:userexception (corba:exception)
                  ())

(define-condition unknown-user-exception (corba:userexception)
                  ((id :initarg :id :reader exception-id)
                   (values :initarg :values :reader exception-values)))


(defgeneric exception-typecode (exception)
  (:documentation "The typecode for the exception"))

(defmethod exception-id ((exc corba:userexception))
  (op:id (exception-typecode exc)))

(defvar *user-exception-classes*
  (make-hash-table :test #'equal))

(defun id-exception-class (id)
  (gethash id *user-exception-classes*))

(defmacro define-user-exception (symbol &key (name "") (id "") slots members)
  "Syntax: scoped-symbol
id: repo-id
name: repo-name
Slots: deprecated
Members: (name typecode)*"
  (loop
    for member in members
    for slot-name = (string (car member))
    for initarg = (lispy-name slot-name)
    for slot = (intern (symbol-name initarg))  ; FIXME: or make-symbol ??
    collect (list slot :initarg initarg)
    into slot-defs
    collect `(define-method ,slot ((s ,symbol)) (slot-value s ',slot)) ; FIXME: not quite ANSI
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
        (defmethod exception-typecode ((exc ,symbol))
          (load-time-value (make-typecode :tk_except ,id ,name (list ,@tc-members))))
        (defmethod userexception-values ((ex ,symbol))
          (list ,@(mapcar (lambda (slot-spec) `(slot-value ex ',(car slot-spec)))
                          slots))))))) 






(defun report-systemexception (exc stream)
  (format stream
          "Exception ~A~_ minor ~A~_ completed ~A"
	  (exception-id exc)
          (system-exception-minor exc)
          (system-exception-completed exc)))

(defun report-userexception (exc stream)
  (format stream
          "User Exception (~A) ~_~S"
          (exception-id exc) (userexception-values exc)))
