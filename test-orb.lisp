(in-package :clorb)

(defclass test-orb (clorb-orb)
  (resolve-namecontext resolve-name))

(defmethod orb-resolve ((orb test-orb) namecontext namestr)
  (setf (slot-value orb 'resolve-namecontext) namecontext)
  (setf (slot-value orb 'resolve-name) namestr))


(define-test "Test ORB Pseudo object"
  
  (define-test "Can't marshal local objects"
    (let ((orb (CORBA:ORB_init)))
      (ensure-exception
       (op:object_to_string orb orb)
       corba:marshal 'op:minor (std-minor 4))
      (ensure-exception
       (op:_create_request orb nil "foo" nil nil 0)
       CORBA:NO_IMPLEMENT 'op:minor (std-minor 4))))
  
  
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


  (define-test "Initializing port and host"
    (let ((orb (CORBA:ORB_init)))
      (let ((host (orb-host orb))
            (port (orb-port orb)))
        (unwind-protect
          (progn (CORBA:ORB_init
                  '("-ORBPort 98" "-ORBHostname lagostz")
                  "")
                 (ensure-pattern* orb
                                  'orb-host "lagostz"
                                  'orb-port 98))
          (setf (orb-host orb) host
                (orb-port orb) port)))))
  
  
  (define-test "Resolve corbaname URL"
    (let ((orb (make-instance 'test-orb)))
      (op:string_to_object orb "corbaname::example.com#a/str%20ing/path")
      (ensure-equalp (slot-value orb 'resolve-name) "a/str ing/path")
      (ensure-pattern* (slot-value orb 'resolve-namecontext)
                       'object-profiles (sequence-pattern
                                         (pattern 'iiop-profile-host "example.com"
                                                  'iiop-profile-port 2809
                                                  'iiop-profile-key (decode-objkey-vector "NameService"))))))
  
  
  #|end|#)
  