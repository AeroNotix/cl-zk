(in-package :cl-zk)


(defclass zk-connection ()
  ((conn :accessor conn :initarg :conn)))

(defun make-connection (&key (host "localhost") (port 2181))
  (let* ((cxn (usocket:socket-stream
               (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
         (conn (make-instance 'zk-connection :conn cxn)))
    (connect conn)))

(defgeneric connect (connection &key)
  (:documentation "Makes an initialization request to Zookeeper."))

(defgeneric get-data (connection path &key)
  (:documentation "Gets data from the path."))

(defmethod connect ((conn zk-connection)
                    &key (protocol-version 0)
                      (last-zxid-seen 0)
                      (timeout 0)
                      (session-id 0)
                      (password ""))
  (let* ((os (flexi-streams:make-in-memory-output-stream))
         (ims (flexi-streams:make-flexi-stream os))
         (c (conn conn))
         (connect-request (make-instance 'connect-request
                                         :protocol-version protocol-version
                                         :last-zxid-seen last-zxid-seen
                                         :timeout timeout
                                         :session-id session-id
                                         :password password)))
    (encode-value connect-request ims)
    (force-output ims)
    (let ((bv (flexi-streams:get-output-stream-sequence os)))
      (write-int (length bv) c)
      (write-sequence bv c))
    (force-output c)
    (values)))
