(in-package :clorb)

  
(defun symbol-ifr-parent-id (symbol)
  (let ((parent (get symbol 'ifr-parent)))
    (if parent (symbol-ifr-id parent) "")))

(defmethod generate-ifr-description ((tc except-typecode) symbol)
  (CORBA:EXCEPTIONDESCRIPTION
   :name (op:name tc)
   :id (op:id tc)
   :defined_in (symbol-ifr-parent-id symbol)
   :type tc))

(defmethod generate-ifr-description ((tc null) symbol)
  ;; For Contained that hasn't got a TypeCode: 
  ;;  InterfaceDef, ModuleDef, OperationDef, AttributeDef
  (multiple-value-bind (indicator value)
                       (get-properties (symbol-plist symbol) '(ifr-result ifr-type))
    (case indicator
      (ifr-result                       ; it's an operation
       (CORBA:OperationDescription
        :id (symbol-ifr-id symbol)
        :name (get symbol 'ifr-name)
        :version (get symbol 'ifr-version "1.0")
        :defined_in (symbol-ifr-parent-id symbol)
        :result value
        :mode (get symbol 'ifr-mode)
        :parameters (loop for (param-name param-mode param-type) in (get symbol 'ifr-params)
                          collect (corba:parameterdescription
                                   :name param-name
                                   :type param-type
                                   :mode param-mode))
        :exceptions (mapcar #'ifr-description (get symbol 'ifr-exceptions))))
      (ifr-type                         ; it's an attribute
       (CORBA:AttributeDescription
        :id (symbol-ifr-id symbol)
        :name (get symbol 'ifr-name)
        :version (get symbol 'ifr-version "1.0")
        :defined_in (symbol-ifr-parent-id symbol)
        :mode (get symbol 'ifr-mode)
        :type value))
      ((nil) 
       (error "Can't generate a description for ~S" symbol)))))
  



(defclass ifr-info-target (code-target)
  ())

(defmethod target-code-contained ((c CORBA:InterfaceDef) (op CORBA:OperationDef)
                                   (target ifr-info-target))
  (let ((desc (op:value (omg.org/features:describe op))))
    (assert (typep desc 'corba:operationdescription))
    (make-progn* 
     (call-next-method)
     `(define-operation ,(scoped-target-symbol target op)
        :id ,(op:id desc)
        :name ,(op:name desc)
        :defined_in ,(scoped-target-symbol target (op:defined_in op))
        :version ,(op:version desc)
        :result ,(target-typecode (omg.org/features:result_def op) target)
        :mode ,(op:mode desc)
        :contexts ,(op:contexts desc)
        :parameters ,(map 'list (lambda (param)
                                  (list (op:name param) 
                                        (op:mode param)
                                        (target-typecode (op:type_def param) target)))
                          (op:parameters desc))
        :exceptions ,(map 'list #'scoped-target-symbol (repeated target) (op:exceptions op))))))


(defmethod target-code-contained ((c CORBA:InterfaceDef) (attr CORBA:AttributeDef) 
                                   (target ifr-info-target))
  (let ((desc (op:value (omg.org/features:describe attr))))
    (make-progn*
     (call-next-method)
     `(define-attribute ,(scoped-target-symbol target attr)
        :id ,(op:id desc)
        :name ,(op:name desc)
        :version ,(op:version desc)
        :defined_in ,(scoped-target-symbol target (op:defined_in attr))
        :mode ,(op:mode desc)
        :type ,(target-typecode (op:type_def attr) target)))))



;;(pprint (target-code (lookup-name "::CosNaming::NamingContext::resolve")
;;                     (make-instance 'ifr-info-target)))


(defclass new-stub-target (ifr-info-target static-stub-target)
  ())

(defclass new-all-target (ifr-info-target servant-target static-stub-target)
  ())


#|

(CORBA:IDL "clorb:idl;interface_repository.idl" 
           :output "clorb:src;y-ifr-base.lisp" 
           :target 'new-stub-target
           :package-decl nil
           :eval nil
           :exclude '("::CORBA::TypeCode")
           :skeleton nil)

(CORBA:IDL "clorb:idl;CosNaming.idl" 
           :output "clorb:src;y-cosnaming.lisp" 
           :target 'new-stub-target
           :package-decl t
           :eval nil
           :skeleton nil)


|#
