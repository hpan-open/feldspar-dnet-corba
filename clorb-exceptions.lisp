;;;; Exceptions

(in-package :clorb)


(define-condition corba:exception (serious-condition)
  ((id :initform "IDL:omg.org/CORBA/Exception:1.0"
       :initarg :id
       :reader exception-id)))

(deftype CORBA::completion_status ()
  '(member :COMPLETED_YES :COMPLETED_NO :COMPLETED_MAYBE))

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


(define-condition corba:userexception (corba:exception)
  ((values :initarg :values :reader userexception-values :initform nil)
   (types  :initarg :types  ))
  (:report report-userexception)
  #-clisp
  (:default-initargs
      :id "IDL:omg.org/CORBA/UserException:1.0"))


(macrolet
    ((define-system-exceptions (&rest excnames)
         `(progn
            ,@(loop for name in excnames collect
                    (let* ((namestr (string name))
                           (id (format nil "IDL:omg.org/CORBA/~A:1.0" namestr))
                           (sym (intern namestr :CORBA)))
                      `(define-condition ,sym (corba:systemexception)
                         (#+clisp
                          (id :initform ,id))
                         #-clisp
                         (:default-initargs
                             :id ,id)))))))
  (define-system-exceptions
      UNKNOWN BAD_PARAM NO_MEMORY IMP_LIMIT
      COMM_FAILURE INV_OBJREF NO_PERMISSION INTERNAL MARSHAL
      INITIALIZE NO_IMPLEMENT BAD_TYPECODE BAD_OPERATION
      NO_RESOURCES NO_RESPONSE PERSIST_STORE BAD_INV_ORDER
      TRANSIENT FREE_MEM INV_IDENT INV_FLAG INTF_REPOS
      BAD_CONTEXT OBJ_ADAPTER DATA_CONVERSION OBJECT_NOT_EXIST
      TRANSACTION_REQUIRED TRANSACTION_ROLLEDBACK INVALID_TRANSACTION ))


(defmacro define-user-exception (name &key id slots)
  `(progn
     (define-condition ,name (CORBA:UserException)
                       (,@slots
                        #+clisp (id :initform ,id))
       #-clisp
       (:default-initargs
         :id ,id ))
     (defmethod userexception-values ((ex ,name))
       (list ,@(mapcar (lambda (slot-spec) `(slot-value ex ',(car slot-spec)))
                       slots)))) )

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
