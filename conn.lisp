(in-package :cl-zk)


(defclass zk-connection ()
  ((conn :accessor conn :initarg :conn)
   (tcp- :accessor tcp-conn :initarg :tcp-conn)
   (read-loop-thread :accessor read-loop-thread :initarg :read-loop-thread)
   (stop-channel :accessor stop-channel :initarg :stop-channel)
   (query-channel :accessor query-channel :initarg :query-channel)))

(defun make-connection (&key (host "localhost") (port 2181))
  (let* ((tcp-conn (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
         (cxn (usocket:socket-stream tcp-conn))
         (conn (make-instance 'zk-connection :conn cxn :tcp-conn tcp-conn)))
    (connect conn)
    (read-connect-response conn)
    (multiple-value-bind (stop-channel query-channel read-loop-thread) (start-io-loop conn)
      (setf (stop-channel conn) stop-channel)
      (setf (query-channel conn) query-channel)
      (setf (read-loop-thread conn) read-loop-thread))
    conn))

(defgeneric connect (connection &key)
  (:documentation "Makes an initialization request to Zookeeper."))

(defmethod connect ((conn zk-connection)
                    &key (protocol-version 0)
                      (last-zxid-seen 0)
                      (timeout 10000)
                      (session-id 0)
                      (password (make-string 16 :initial-element (code-char 0)))
                      (read-only nil))
  (let* ((connect-request
          (make-instance 'connect-request
                         :protocol-version protocol-version
                         :last-zxid-seen last-zxid-seen
                         :timeout timeout
                         :session-id session-id
                         :password password
                         :read-only read-only))
         (os (flexi-streams:make-in-memory-output-stream))
         (ims (flexi-streams:make-flexi-stream os)))
    (encode-value connect-request ims)
    (encode-value connect-request conn)
    (format t "~A" (flexi-streams:get-output-stream-sequence os))
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

(defun timeout-channel (time)
  (let* ((c (make-instance 'chanl:bounded-channel :size 1))
         (timeout-fn (lambda ()
                       (sleep time)
                       (chanl:send c :timeout))))
    (bordeaux-threads:make-thread timeout-fn)
    c))

(defun close-connection (conn)
  (chanl:send (stop-channel conn) t)
  (values))

(defun make-zk-loop (conn stop-channel query-channel)
  (lambda ()
    (let ((exit? nil))
      (loop
         do
           (let ((tc (timeout-channel 10)))
             (force-output)
             (chanl:select
               ((chanl:recv stop-channel)
                (let ((c (conn conn)))
                  (usocket:socket-close (tcp-conn conn))
                  (setf exit? t)))
                ((chanl:recv tc)
                   (encode-value +ping-instance+ conn))))
               while (not exit?)))))))

(defun start-io-loop (conn)
  (let* ((stop-channel (make-instance 'chanl:channel))
         (query-channel (make-instance 'chanl:channel))
         (read-loop-fn (make-zk-loop conn stop-channel query-channel)))
    (values
     stop-channel
     query-channel
     (bordeaux-threads:make-thread read-loop-fn))))
