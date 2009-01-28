;;;; loadup.lisp --- Load the CLORB system


(load (merge-pathnames "clorb-files.lisp" *load-pathname*))
(net.cddr.clorb.system:reload)


(packer:declare-package
 :package "NET.CDDR.CLORB"
 :modules '((nil)))


(provide :clorb)
