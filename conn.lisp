(in-package :cl-zk)

;; TODO: Have the Makefile make install the proper .so file
;; TODO: Document how to have this work properly
(cffi:define-foreign-library zookeeper
  (:unix
   (:or "libzookeeper.so"
        "libzookeeper_mt.so"
        "libzookeeper_st.so"
        "/home/xeno/dev/cl-zk/.zookeeper/src/c/.libs/libzookeeper_mt.so")))

(cffi:use-foreign-library zookeeper)

(cffi:defcvar "ZOO_EXPIRED_SESSION_STATE" :int)
(cffi:defcvar "ZOO_AUTH_FAILED_STATE" :int)
(cffi:defcvar "ZOO_CONNECTING_STATE" :int)
(cffi:defcvar "ZOO_ASSOCIATING_STATE" :int)
(cffi:defcvar "ZOO_CONNECTED_STATE" :int)
(cffi:defcvar "ZOO_READONLY_STATE" :int)
(cffi:defcvar "ZOO_NOTCONNECTED_STATE" :int)

(cffi:defcfun zookeeper-init :pointer
  (hostname :string)
  (watcher-fn :pointer)
  (recv-timeout :int)
  (clientid :pointer)
  (context :pointer)
  (flags :int))

(cffi:defcfun zookeeper-close :int
  (zhandle :pointer))

(cffi:defcfun zookeeper-init2 :pointer
  (hostname :string)
  (watcher-fn :pointer)
  (recv-timeout :int)
  (clientid :pointer)
  (context :pointer)
  (flags :int)
  (log-callback :pointer))

(cffi:defcfun zoo-get :int
  (zhandle :pointer)
  (path :string)
  (watch :int)
  (buffer :pointer)
  (buffer-len :int)
  (stat :pointer)) ;; TODO: This is a Stat thing? Implement

(cffi:defcfun zoo-exists :int
  (zhandle :pointer)
  (path :string)
  (watch :int)
  (stat :pointer))

(cffi:defcfun zoo-state :int
  (zhandle :pointer))

(defvar np (cffi:null-pointer))

(cffi:defcvar ("errno" *errno* :read-only t) :int)

(defun make-c-connection (host &key (timeout 10000) (clientid np))
  (let ((conn (zookeeper-init host np timeout clientid np 0)))
    (if (eq np conn)
        ;; TODO: make this a signal/restart
        (error (format nil "Error creating connection: ~d" *errno*))
        conn)))

(cffi:defcallback logcb :void ((message :string))
  (format t "~A~%" message))

(defun make-c-connection2 (host logcallback &key (timeout 10000) (clientid np))
  (zookeeper-init2 host np timeout clientid np 0 logcallback))
