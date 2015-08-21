(in-package :cl-zk)


(defgeneric encode-value (value stream)
  (:documentation
   "Encodes a value into a stream"))

(defclass connect-request ()
  ((protocol-version :accessor protocol-version :initarg :protocol-version)
   (last-zxid-seen :accessor last-zxid-seen :initarg :last-zxid-seen)
   (timeout :accessor timeout :initarg :timeout)
   (session-id :accessor session-id :initarg :session-id)
   (password :accessor password :initarg :password)))

(defun write-length (thing stream)
  (let ((len (length thing)))
    (write-int len stream)
    len))

(defmethod encode-value ((value vector) stream)
  (write-length value stream)
  (write-sequence value stream))

(defun as-bytes (s)
  (flexi-streams:string-to-octets s))

(defmethod encode-value ((value connect-request) stream)
  (with-slots (protocol-version last-zxid-seen timeout session-id password) value
    (write-int protocol-version stream)
    (write-bigint last-zxid-seen stream)
    (write-int timeout stream)
    (write-bigint session-id stream)
    (write-int (length password) stream)
    (encode-value (as-bytes password) stream)
    (write-int 0 stream)))
        (write-int 1 stream)
        (write-int 0 stream))))
