
(require :db-sockets)
(load "clorb-pkgdcl")
(load "clorb-files")

(clorb:reload)

(clorb::load-ir)

(import '(cl-user::setup-pns
          cl-user::hello-client
          cl-user::setup-hello
          cl-user::run-hello)
        :clorb)

(defun hh ()
  (cl-user::setup-hello :file "hello.ior")
  (cl-user::hello-client :file "hello.ior"))

(defun hhn ()
  (cl-user::setup-hello :name "hello")
  (cl-user::hello-client :name "hello"))


;; Local variables:
;; inferior-lisp-program: "sbcl"
;; End:
