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
  


#|

(CORBA:IDL "clorb:idl;interface_repository.idl" 
           :output "clorb:src;y-ifr-base.lisp" 
           :package-decl nil
           :eval nil
           :exclude '("::CORBA::TypeCode")
           :skeleton nil)

(CORBA:IDL "clorb:idl;CosNaming.idl" 
           :output "clorb:src;y-cosnaming.lisp" 
           :package-decl t
           :eval nil
           :skeleton nil)


|#
