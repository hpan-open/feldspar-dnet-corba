;;; package: Lisp Unit Test (A)

(in-package :cl-user)

(defpackage "NET.CDDR.LUNA"
  (:use "COMMON-LISP")
  (:export
   "EVAL-TO-PATTERN"
   "DEFINE-TEST" "DEFINE-TEST-SUITE" "ENSURE" "ENSURE-EQL" "ENSURE-EQUALP" "ENSURE-PATTERN"
   "ENSURE-PATTERN*" "ENSURE-TYPEP" "EVAL-TO" "FAIL-MATCH" "MATCH" "MATCH-FAIL" "MATCH-FAIL-MESSAGE"
   "OR-PATTERN" "PATTERN" "PATTERN-ARGS" "PRINT-RESULT" "SEQUENCE-PATTERN" "SEXP-PATTERN" "TEST-RESULT"
   "VARIABLES" "WITH-SUB-TEST" "&ANY" "BOOLEAN-MATCH" "*TEST-SUITE-RESULT*"))


#|
(packer:require-package :net.cddr.utils)
(net.cddr.utils:gen-package-decl :net.cddr.luna)
|#