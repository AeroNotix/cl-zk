(in-package :cl-zk)


(defgeneric encode-value (value stream)
  (:documentation
   "Encodes a value into a stream"))

(defgeneric decode-value (value stream)
  (:documentation
   "Decodes a value from a stream"))

(defun write-length (thing stream)
  (let ((len (length thing)))
    (write-int len stream)
    len))

(defmethod encode-value ((value vector) stream)
  (write-length value stream)
  (write-sequence value stream))

(defun as-bytes (s)
  (flexi-streams:string-to-octets s))

(defmethod encode-value :around (value (conn zk-connection))
  (let* ((os (flexi-streams:make-in-memory-output-stream))
         (ims (flexi-streams:make-flexi-stream os))
         (cxn (conn conn)))
    (call-next-method value ims)
    (force-output ims)
    (let ((bv (flexi-streams:get-output-stream-sequence os)))
      (write-int (length bv) cxn)
      (write-sequence bv cxn))
    (force-output cxn)))

(defmethod encode-value :around (value (stream flexi-streams:flexi-output-stream))
  (let* ((os (flexi-streams:make-in-memory-output-stream))
         (ims (flexi-streams:make-flexi-stream os)))
    (call-next-method value ims)
    (force-output ims)
    (let ((bv (flexi-streams:get-output-stream-sequence os)))
      (write-int (length bv) stream)
      (write-sequence bv stream))
    (force-output stream)))
