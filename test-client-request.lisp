(in-package :clorb)

(define-test-suite "Client Request"
  (variables
   (orb (CORBA:ORB_init)))

  (define-test "one way static call"
    (let ((obj (make-instance 'CORBA:Proxy
                 :id "IDL:test:1.0"
                 :the-orb orb
                 :profiles (list (make-iiop-profile
                                  :version '(1 . 0)
                                  :host "localhost"
                                  :port 9999
                                  :key #()))))
          (conn (make-instance 'connection
                  :orb (CORBA:ORB_init)
                  :io-descriptor (make-io-descriptor
                                  :shortcut-p t
                                  :stream (make-instance 'shortcut-stream
                                            :output (make-instance 'octets-stream))))))
      
      (setf (selected-profile obj) (first (object-profiles obj)))
      (setf (object-connection obj) conn)
      (let ((*io-event-queue* nil))
        (static-call ("foo" obj)
                     :output ((buffer) (marshal-long 123 buffer))
                     :no-response t))
      (ensure 
       (> (io-descriptor-write-limit (connection-io-descriptor conn)) 12))))
  


#| end suite |# )
