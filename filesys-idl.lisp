(in-package :clorb)
(idef-definitions
 (define-module "FileSys" ()
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
