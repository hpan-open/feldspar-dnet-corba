(in-package :clorb)


;;;; Value


(define-typecode value_box-typecode
  :kind :tk_value_box
  :cdr-syntax (complex :tk_string :tk_string :tk_typecode)
  :params (id name content_type))


(define-typecode value-typecode
  :kind :tk_value
  :cdr-syntax (complex :tk_string :tk_string :tk_short :tk_typecode 
                       (sequence (:tk_string :tk_typecode :tk_short)))
  :params (id name type_modifier concrete_base_type :members)
  :member-params (member_name member_type member_visibility)
  :constant (corba::TC_ValueBase "IDL:omg.org/CORBA/ValueBase:1.0" "ValueBase"
                                 CORBA::VM_NONE nil ()))


#|

(DEFINE-VALUE VTEST:TREE
  :ID "IDL:adorb.org/vtest/Tree:1.0"
  :NAME "Tree"
  :BASE_VALUE VTEST:NODE
  :IS_ABSTRACT NIL
  :IS_CUSTOM NIL
  :IS_TRUNCATABLE NIL
  :SUPPORTED_INTERFACES NIL
  :ABSTRACT_BASE_VALUES NIL
  :MEMBERS (("box" (SYMBOL-TYPECODE 'VTEST:VBOX) 1) 
            ("up" (SYMBOL-TYPECODE 'VTEST:NODE) 0)
            ("left" (SYMBOL-TYPECODE 'VTEST:NODE) 0) 
            ("right" (SYMBOL-TYPECODE 'VTEST:NODE) 0)))


|#


(defmacro DEFINE-VALUE (symbol &key id name base_value 
                               is_abstract is_custom is_truncatable
                               supported_interfaces abstract_base_values members)
  (let ((value-bases (append (if base_value (list base_value))
                             abstract_base_values)))

  `(progn
     (defclass ,symbol 
       (,@(or value-bases (list 'CORBA::ValueBase))
        ,@supported_interfaces)
       (,@(loop for (name) in members
                collect `(,(feature name) :initarg ,(key name)))))
     (defmethod shared-initialize ((value ,symbol) slot-names &key factory &allow-other-keys)
       (declare (ignore slot-names))
       (if factory (error 'CORBA:BAD_PARAM))
       (call-next-method))
     (defmethod object-id ((value ,symbol))
       ,id)
     ,@(loop for (name nil nil) in members nconc 
             (list `(define-method ,(feature name) ((value ,symbol))
                      (slot-value value ',(feature name)))
                   `(define-method (setf ,(feature name)) (new (value ,symbol))
                      (setf (slot-value value ',(feature name)) new))))
     (set-symbol-id/typecode 
      ',symbol
      ,id                      
      (create-value-tc ,id ,name 
                       ,(cond (is_abstract omg.org/corba:vm_abstract)
                              (is_truncatable omg.org/corba:vm_truncatable)
                              (is_custom omg.org/corba:vm_custom)
                              (t omg.org/corba:vm_none ))
                       ,(if base_value `(symbol-typecode ',base_value))
                       (list ,@(loop for (name tc access) in members
                                     collect `(list ,name ,tc ,access))))))))


(defun value-tag (&rest flags)
  (+ #x7fffff00
     (loop for x in flags sum (ecase x
                                (:codebase-url 1)
                                (:repoid 2)
                                (:repoids 6)
                                (:chunking 8)))))

(defmethod marshal (value (tc-formal value-typecode) buffer)
  (cond (value
         (let* ((id (object-id value))
                (exact-type (equal id (op:id tc-formal)))
                (value-class (ifr-id-symbol id))
                tc)
           (assert value-class)
           (setq tc (if exact-type tc-formal
                        (symbol-typecode value-class)))
           (marshal-long (if exact-type (value-tag) (value-tag :repoid)) 
                         buffer)
           (unless exact-type
             (marshal-string id buffer))
           (marshal-state value tc buffer)))
        (t
         (marshal-long 0 buffer))))


(defun marshal-state (value tc buffer)
  (when tc
    (marshal-state value (op:concrete_base_type tc) buffer)
    (loop for i from 0 below (op:member_count tc)
          do (marshal (slot-value value (feature (op:member_name tc i)))
                      (op:member_type tc i)
                      buffer))))

(defmethod unmarshal ((tc-formal value-typecode) buffer)
  (let ((valuetag (unmarshal-long buffer)))
    (cond ((zerop valuetag) nil)
          (t
           (let (tc value-class)
             (case (logand valuetag #x06)
               (0                       ; no type info, has to match formal parameter
                (setq tc tc-formal 
                      value-class (ifr-id-symbol (op:id tc))))
               (2  
                (let ((repoid (unmarshal-string buffer)))
                  (setq value-class (ifr-id-symbol repoid))
                  ;; check class is known
                  (or value-class (error 'CORBA:MARSHAL))
                  ;;(or (op:_is_a (op:id tc-formal) value-class))
                  (setq tc (symbol-typecode value-class))))
               (otherwise (error 'omg.org/corba:no_implement)))
             (let ((initargs nil)) 
               (let ((tc-stack nil))
                 (do ((base-tc tc (op:concrete_base_type base-tc)))
                     ((null base-tc))
                   (push base-tc tc-stack))
                 (dolist (tc tc-stack)
                   (dotimes (i (op:member_count tc))
                     (push (key (op:member_name tc i)) initargs)
                     (push (unmarshal (op:member_type tc i) buffer) initargs))))
               (apply #'make-instance value-class
                      :create-for-unmarshal t (nreverse initargs))))))))

