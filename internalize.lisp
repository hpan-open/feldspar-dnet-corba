(in-package :clorb)

(defclass irepository (repository)
  ())



(defmethod internalize ((r irepository) obj &optional (content t))
  (labels ((interner (o)
             (typecase o
               (string o)
               (sequence
                (map 'list #'interner o))
               (CORBA:Struct
                (map-struct #'interner o))
               (CORBA:Typecode o)
               (standard-object
                (internalize r o nil))
               (t o)))
           (module-interner (o)
             (internalize r o)))
    (let ((kind (op:def_kind obj)))
      (case kind
        ((:dk_Attribute :dk_Constant :dk_Exception :dk_Interface
          :dk_Module :dk_Operation :dk_Typedef
          :dk_Alias :dk_Struct :dk_Union :dk_Enum)
         ;; Objects with id
         (let* ((id (op:id obj))
                (old (gethash id (idmap r)) ))
           (if old
               (progn (when (and (eq kind :dk_Module) content)
                        (set-from old obj #'module-interner))
                      old)
             (let ((new (make-instance (get-ir-class kind)
                          :id id 
                          :name (op:name obj)
                          :version (op:version obj)
                          :absolute_name (op:absolute_name obj))))
               (setf (gethash id (idmap r)) new)
               (setf (slot-value new 'defined_in) 
                 (interner (op:defined_in obj))) 
               (loop for n in '(type_def mode value members)
                   when (slot-exists-p new n)
                   do (setf (slot-value new n) 
                        (interner (funcall (intern (symbol-name n) :op) obj))))
               (set-from new obj (if (eq kind :dk_Module)
                                     #'module-interner
                                   #'interner))
               new))))
        ((:dk_Primitive)
         (op:get_primitive r (op:kind obj)))
        ((:dk_Repository)
         (when content
           (map nil
             (lambda (c)
               (addto r (interner c)))
             (op:contents obj :dk_All t)))         
         r)
        ((:dk_String :dk_Sequence :dk_Array
          :dk_Wstring :dk_Fixed)
         (let ((new (make-instance (get-ir-class kind))))
           (set-from new obj #'interner)
           new))))))


(defgeneric set-from (dst src &optional filter))

(defmethod set-from ((obj irobject) src &optional filter)
  (declare (ignore src filter)))

(defmethod set-from ((obj container) src &optional filter)
  (setf (contents obj)
    (map 'list (or filter #'identity)
         (op:contents src :dk_All t))))

(defmethod set-from :after ((obj interface-def) src &optional filter)
  (setf (op:base_interfaces obj)
    (map 'list (or filter #'identity)
         (op:base_interfaces src))))

(defmethod set-from ((obj operation-def) src &optional (filter #'identity))
  (setf (op:result_def obj) (funcall filter (op:result_def src))
        (op:params obj) (funcall filter (op:params src))
        (op:contexts obj) (funcall filter (op:contexts src))
        (op:exceptions obj) (funcall filter (op:exceptions src))))

(defmethod set-from :after ((obj alias-def) src &optional (filter #'identity))
  (setf (op:original_type_def obj) 
    (funcall filter (op:original_type_def src))))

(defmethod set-from :after ((obj idltype) src &optional (filter #'identity))
  (loop for n in '(bound length kind digits scale element_type_def)
      when (slot-exists-p obj n)
      do (setf (slot-value obj n) 
           (funcall filter (funcall (intern (symbol-name n) :op) src)))))

(defmethod set-from :after ((obj struct-def) src &optional (filter #'identity))
  (setf (op:members obj) 
    (funcall filter (op:members src))))

(defmethod set-from :after ((obj union-def) src &optional (filter #'identity))
  (setf (op:discriminator_type_def obj) 
    (funcall filter (op:discriminator_type_def src))))
