(in-package :clorb) 

(defparameter *host* "127.0.0.1")

(defparameter *port* 4722)

(defparameter *name-service* "NameService"
  "Reference to the CORBA NameService.
This should be the name of a file where the name service IOR is stored
or the IOR.")

(defparameter *interface-repository* "InterfaceRepository"
  "Reference to the CORBA InterfaceRepository.
This should be the name of a file where the service IOR is stored
or the IOR.")

(defparameter *log-level* 2)

(defparameter *explicit-any* nil
  "Flag, if true, CORBA::Any will be unmarshaled to an ANY struct.
If false, the any is automaticaly translated to its value.")

(defparameter *principal* nil
  "Octet sequence used for the principal field in the GIOP message.
Used by ORBit for its cookie.")

(defvar *service-context* nil
  "Service context sent with every CORBA request.")
