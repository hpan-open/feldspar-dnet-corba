(in-package :clorb) 

(defparameter *host* nil
  "The host that should be used in IORs.
If nil, use default.")

(defvar *port* nil
  "The port to listen to.
If nil, let implementation choose a port.")

(defvar *clorb-pathname-defaults*
  (if (find-package "NET.CDDR.CLORB.SYSTEM")
      (symbol-value (intern "*SOURCE-PATHNAME-DEFAULTS*"
                            "NET.CDDR.CLORB.SYSTEM"))
      (make-pathname :name nil :type nil :version nil
                     :defaults (or #+MCL ccl:*loading-file-source-file*
                                   *compile-file-pathname*
                                   *load-pathname* ))))


(defvar *default-include-directories*
  (list (merge-pathnames (make-pathname :directory '(:relative "idl")
                                        :name nil :type nil)
                         *clorb-pathname-defaults*)))


(defvar *name-service* "file:///tmp/NameService"
  "Reference to the CORBA NameService.
Should be an URL that can be accepted by op:string_to_object.")

(defvar *interface-repository* "file:///tmp/InterfaceRepository"
  "Reference to the CORBA InterfaceRepository.
Should be an URL that can be accepted by op:string_to_object.")

(defparameter *log-level* 3)

(defparameter *explicit-any* t
  "Flag, if true, CORBA::Any will be unmarshaled to an ANY struct.
If false, the any is automaticaly translated to its value.")

(defparameter *principal* nil
  "Octet sequence used for the principal field in the GIOP message.
Used by ORBit for its cookie.")

(defvar *service-context* nil
  "Service context sent with every CORBA request.")
