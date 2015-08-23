(in-package :cl-zk)


(defclass zk-connection ()
  ((conn :accessor conn :initarg :conn)))

(defun make-connection (&key (host "localhost") (port 2181))
  (let* ((cxn (usocket:socket-stream
               (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
         (conn (make-instance 'zk-connection :conn cxn)))
    (connect conn)
    (let ((cr (read-connect-response conn)))
      (format t "~A" (password cr)))
    conn))

(defgeneric connect (connection &key)
  (:documentation "Makes an initialization request to Zookeeper."))

(defmethod connect ((conn zk-connection)
                    &key (protocol-version 0)
                      (last-zxid-seen 0)
                      (timeout 0)
                      (session-id 0)
                      (password "")
                      (read-only nil))
  (let* ((connect-request
          (make-instance 'connect-request
                         :protocol-version protocol-version
                         :last-zxid-seen last-zxid-seen
                         :timeout timeout
                         :session-id session-id
                         :password password
                         :read-only read-only))
         (ims (flexi-streams:make-in-memory-output-stream))
         (os (flexi-streams:make-flexi-stream ims)))
    (encode-value connect-request os)
    (format t "~A" (flexi-streams:get-output-stream-sequence ims))
    (values)))

(defun get-data (conn path)
  (let ((gdr (make-instance 'get-data-request :path path :watch nil))
        (c (conn conn)))
    (encode-value gdr conn)
    (let* ((size (* 1 (read-int c)))
           (buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf c :end size)
      buf)))

(defun get-children (conn path)
  (let ((gdr (make-instance 'get-children-request :path path :watch nil))
        (c (conn conn)))
    (encode-value gdr conn)
    (let* ((size (* 1 (read-int c)))
           (buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf c :end size)
      buf)))

(defun read-connect-response (conn)
  (let* ((c (conn conn))
         (cr (make-instance 'connect-response)))
    (decode-value cr c)))
