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


  (define-test "Initializing initial references"
    (let ((orb (CORBA:ORB_init)))
      (setf (orb-initial-references orb)
            (delete-if (lambda (entry)
                         (string-starts-with (car entry) "x-"))
                       (orb-initial-references orb)))
      (CORBA:ORB_init (list "-ORBInitRef" "x-a=corbaloc::x/x-a"
                            "-ORBInitRef x-b=corbaloc::x/x-b"
                            "-ORBInitRefx-c=corbaloc::x/x-c" ))
      (loop with names = (op:list_initial_references orb)
            for n in '("x-a" "x-b")
            do (ensure (member n names :test #'equal)
                       "Name '~A' should be in initial references" n)
            (let ((obj (op:resolve_initial_references orb n)))
              (ensure-equalp (oid-to-string 
                              (iiop-profile-key (first (object-profiles obj))))
                             n)))))

      
  #|end|#)
  