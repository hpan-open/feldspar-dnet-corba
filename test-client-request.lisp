(in-package :clorb)

(define-test-suite "Client Request"


  (define-test "one way static call"
    
    (let ((obj (make-instance 'CORBA:Proxy
                 :id "IDL:test:1.0"
                 :profiles (list (make-iiop-profile
                                  :version '(1 . 0)
                                  :host "localhost"
                                  :port 9999
                                  :key #()))))
          (conn (make-instance 'connection)))
      
      (setf (selected-profile obj) (first (object-profiles obj)))
      (setf (object-connection obj) conn)
      
      (static-call ("foo" obj)
                   :output ((buffer) (marshal-long 123 buffer))
                   :no-response t)
      
      )
    
    
    )

)