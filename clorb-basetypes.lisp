;;;; Basic CORBA types

(in-package :clorb)


;;;; Basic types

(deftype corba:boolean ()
  'boolean)

(deftype corba:char ()
  'character)

(deftype corba:wchar ()
  ;; FIXME! is this wide ?
  'character)

(deftype corba:octet ()
  '(unsigned-byte 8))

(deftype corba:string ()
  'string)

(deftype wstring ()
  ;; FIXME
  'string)

(deftype corba:short ()
  '(signed-byte 16))

(deftype corba:ushort ()
  '(unsigned-byte 16))

(deftype corba:long ()
  '(signed-byte 32))

(deftype corba:ulong ()
  '(unsigned-byte 32))

(deftype corba:longlong ()
  '(signed-byte 64))

(deftype corba:ulonglong ()
  '(unsigned-byte 64))

(deftype corba:float ()
  'real)

(deftype corba:double ()
  'real)

(deftype corba:longdouble ()
  'real)

(deftype corba:fixed ()
  'rational)
