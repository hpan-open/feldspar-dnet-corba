(in-package :cl-user)

;;(require :db-sockets)
(require :asdf)
(require :sb-bsd-sockets)

(load "clorb-pkgdcl")
(load "clorb-files")

(clorb:reload)

;;(clorb::load-ir)

(load "examples/hello/auto")


;; Local variables:
;; inferior-lisp-program: "sbcl"
;; End:
