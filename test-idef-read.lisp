(in-package :clorb)

(define-test-suite "IDEF Read Test" ()
  
  (define-test "Read Module" 
      (let ((r (make-instance 'repository)))
        (idef-read '((define-module "Foo" ())) r)
        (let ((m (op:lookup r "Foo")))
          (ensure m "lookup name")
          (ensure-equalp (op:def_kind m) :dk_module))))
  
  (define-test "Read Interface 1" 
      (let ((r (make-instance 'repository)))
        (idef-read '((define-interface "Foo" ())) r)
        (let ((m (op:lookup r "Foo")))
          (ensure m "lookup name")
          (ensure-equalp (op:def_kind m) :dk_interface)
          (ensure-equalp (op:id m) "IDL:Foo:1.0"))))
  
  (define-test "Read Interface 2" 
      (let ((r (make-instance 'repository)))
        (idef-read '((define-module "Foo" ()
                       (define-interface "Bar" ()))) 
                   r)
        (let* ((m (op:lookup r "Foo"))
               (i (op:lookup m "Bar")))
          (ensure i "lookup name")
          (ensure-equalp (op:def_kind i) :dk_interface)
          (ensure-equalp (op:id i) "IDL:Foo/Bar:1.0")
          (ensure-equalp (op:kind (op:type i)) :tk_objref))))

  (define-test "Read Operation" 
      (let ((r (make-instance 'repository)))
        (idef-read '((define-module "Foo" ()
                       (define-interface "Bar" ()
                         (define-operation "greet" ((:in level long))
                           :result-type string))))
                   r)
        (let* ((m (op:lookup r "Foo"))
               (i (op:lookup m "Bar"))
               (o (op:lookup i "greet")))
          (ensure o "lookup name")
          (ensure-equalp (op:def_kind o) :dk_Operation)
          (ensure-equalp (op:kind (op:result o)) :tk_string))))

  (define-test "Read Type" 
      (let ((r (make-instance 'repository)))
        (idef-read '((define-type "Foo" short))
                   r)
        (let* ((o (op:lookup r "Foo")))
          (ensure o "lookup name")
          (ensure-equalp (op:def_kind o) :dk_Alias)
          (ensure-equalp (op:def_kind (op:original_type_def o)) :dk_Primitive)
          (ensure-equalp (op:kind (op:original_type_def o)) :pk_short))))
  
  (define-test "Read Enum"
      (let ((r (make-instance 'repository)))
        (idef-read '((define-enum "F"
                         ("NORMAL" "DIRECTORY" "SYMLINK")))
                   r)
        (let* ((o (op:lookup r "F")))
          (ensure o "lookup name")
          (ensure-equalp (op:def_kind o) :dk_Enum)
          (ensure-equalp (op:members o) '("NORMAL" "DIRECTORY" "SYMLINK")))))
  
  (define-test "Read Struct"
      (let ((r (make-instance 'repository)))
        (idef-read '((define-struct "S"
                         (("a" long)
                          ("b" string))))
                   r)
        (let* ((o (op:lookup r "S")))
          (ensure o "lookup name")
          (ensure-equalp (op:def_kind o) :dk_Struct)
          (ensure-equalp (length (op:members o)) 2  ))))

  (define-test "Read Attribute"
      (let ((r (make-instance 'repository)))
        (idef-read '((define-interface "I" ()
                       (define-attribute "a" string)))
                   r)
        (let* ((o (op:lookup (op:lookup r "I") "a") ))
          (ensure o "lookup name")
          (ensure-equalp (op:def_kind o) :dk_Attribute)
          (ensure-equalp (op:mode o) :attr_normal  )
          (ensure-equalp (op:kind (op:type_def o)) :pk_string))))


  (let ((rep (make-instance 'repository)))
    (define-test "Complex example"
        (idef-read 
         '((define-module "FileSys" ()
             (define-enum "FileType"
                 ("NORMAL" "DIRECTORY" "SYMLINK"))
             (define-struct "Stat"
                 (("mtime" long)
                  ("ctime" long)
                  ("inode" long)
                  ("type" "FileType")))
             (define-interface "FileDescription" ()
               (define-attribute "name" string :readonly t)
               (define-type "Buffer" string)
               (define-operation "read" ((:in "size" long) 
                                         (:out "buf" "Buffer"))
                 :result-type void
                 :exceptions nil)
               (define-operation "destroy" ()
                 :result-type void))
             (define-interface "FileSystem" ()
               (define-operation "open" ((:in file_name string)
                                         (:in flags long))
                 :result-type "::FileSys::FileDescription"))))
         rep))))
