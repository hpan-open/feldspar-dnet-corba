(in-package :clorb)

#|
Layout for Object Key

Transient ior:
1. Magic number for transient IOR <UShort>
2. POA-id <UShort>
3. Unique number for this server instance <ULong>
4. ObjectId

Persistent ior (1):
1. Magic number identifiying the IOR as a persistent IOR
2. POA-id registered with a locator service <UShort>
3. ObjectId

Persistent ior (2):
1. Magic number identifiying the IOR as a persistent IOR
2. POA level <UShort> (1 if child of root poa)
3. poa-name (one per level) as CDR string
4. ObjectId

POA-ids are numbers and are mapped by the ORB to a POA. There should
be no need to store the actual path to the POA in the object key.
|#

(defconstant +transient-ior-magic+   #16R4C43)
(defconstant +persistent-ior-magic1+ #16R6C63)
(defconstant +persistent-ior-magic2+ #16R6C43)

(defvar *instance-id* (random (expt 2 31) (make-random-state t)))

(defun decode-object-key-from-buffer (buffer)
  (let* ((magic (unmarshal-ushort buffer)))
    (cond
      ((eql magic +transient-ior-magic+)
       (let* ((poaid (unmarshal-ushort buffer))
              (uniq (unmarshal-ulong buffer)))
         (if (= uniq *instance-id*)
             (values :transient
                     poaid
                     (subseq (buffer-octets buffer)
                             (buffer-in-pos buffer)))
           (progn
             (mess 2 "Illegal unique id, IOR from other instance")
             nil))))
      ((eql magic +persistent-ior-magic1+)
       (values :persistent
               (unmarshal-ushort buffer)
               (subseq (buffer-octets buffer)
                       (buffer-in-pos buffer))))
      ((eql magic +persistent-ior-magic2+)
       (values :persistent
               (loop repeat (unmarshal-ushort buffer)
                   collect (unmarshal-string buffer))
               (subseq (buffer-octets buffer)
                       (buffer-in-pos buffer))))
      (t
       ;;(warn "illegal magic=~S" magic)
       ;; default poa for boot objects
       (values :transient 0 (buffer-octets buffer))))))

(defun decode-object-key (octets)
  (let ((buffer (make-buffer :octets octets)))
    (decode-object-key-from-buffer buffer)))


(defgeneric to-object-id (objid)
  (:documentation "Convert a lisp object to an object key.
An object key is a octet seqeunce. But for convenience some other lisp
types will be converterd by this GF."))

(defmethod to-object-id ((objid string))
  (map 'vector #'char-code objid))

(defmethod to-object-id ((objid vector))
  ;; Assume an octet vector
  objid)

(defmethod to-object-id ((objid integer))
  ;; FIXME: if this is actually useful it should handle
  ;;  more than ulong compatible integers?
  (let ((buf (get-work-buffer)))
    (marshal-ulong objid buf)
    (buffer-contents buf)))

(defun object-id-to-ingeger (objid)
  (let ((buf (make-buffer :octets objid)))
    (unmarshal-ulong buf)))

(defun make-object-key (type poaid oid
                        &key (uniq *instance-id*) poa-name)
  ;; If poa-name use persistance v2
  (declare (optimize debug))
  (let* ((buffer (get-work-buffer))
         (octets (buffer-octets buffer)))
    (setq oid (to-object-id oid))
    (ecase type
      (:transient
       (marshal-ushort +transient-ior-magic+ buffer)
       (marshal-ushort poaid buffer)
       (marshal-ulong  uniq buffer))
      (:persistent
       (cond
        (poa-name
         (marshal-ushort +persistent-ior-magic2+ buffer)
         (marshal-ushort (length poa-name) buffer)
         (dolist (n poa-name)
           (marshal-string n buffer)))
        (t
         (marshal-ushort +persistent-ior-magic1+ buffer)
         (marshal-ushort poaid buffer)))))
    (let* ((prefix-len (length octets))
           (new-len (+ prefix-len (length oid))))
      (when (> new-len (array-total-size octets))
        (adjust-array octets new-len))
      (setf (fill-pointer octets) new-len)
      (setf (subseq octets prefix-len) oid))
    (copy-seq octets)))


;;;; Lisp mapping convenice functions

(defun portableserver:oid-to-string (oid)
  (map 'string #'code-char oid))

(defun portableserver:string-to-oid (string)
  (to-object-id string))
