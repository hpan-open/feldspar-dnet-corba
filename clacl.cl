(load "clorb-sysdcl")
(excl:load-system :clorb :compile t)
(setq clorb::*host* "pc-lennarts.infotek.no")

(setf clorb::*running-orb* t)
(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(clorb::load-ir)


(defun describe-repo (r)
  (let ((l (coerce (op:contents r :dk_all t) 'list))
        (k (op:def_kind r)))
    (pprint-logical-block (*standard-output* l :prefix "> ")
      (loop for x in l
          for f = nil then t
          do (when f (pprint-newline (if (eq k :dk_Interface)
                                         :fill :mandatory)))
             (format *standard-output*
                     "~A ~S  "
                      (op:name x) (op:def_kind x))
             (when (typep x 'clorb::container)
               (pprint-newline :mandatory)
               (describe-repo x))))))

(setf (tpl:alias "rep") (lambda ()
                          (describe-repo clorb::*idef-repository*)))
