(in-package :clorb)

(define-test "Test ORB Pseudo object"

  (define-test "Can't marshal local objects"
    (let ((orb (CORBA:ORB_init)))
      (ensure-exception
       (op:object_to_string orb orb)
       corba:marshal 'op:minor 4)
      (ensure-exception
       (op:_create_request orb nil "foo" nil nil 0)
       CORBA:NO_IMPLEMENT 'op:minor 4)))



  #|end|#)
  