;;; code moved to download-ir.lisp

(let ((*default-pathname-defaults* 
       (or *load-pathname*
           #+ccl (front-window))))
  (load "clorb-pkgdcl")
  (load "clorb-files"))

(clorb:reload)
