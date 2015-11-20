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

(defvar np (cffi:null-pointer))

(cffi:defcvar ("errno" *errno* :read-only t) :int)
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

(cffi:defcfun zookeeper-init2 :pointer
  (hostname :string)
  (watcher-fn :pointer)
  (recv-timeout :int)
  (clientid :pointer)
  (context :pointer)
  (flags :int)
  (log-callback :pointer))

(cffi:defcfun zerror :string
  (c :int))

(cffi:defcfun zookeeper-close :int
  (zhandle :pointer))

(cffi:defcfun zoo-get :int
  (zhandle :pointer)
  (path :string)
  (watch :int)
  (buffer :pointer)
  (buffer-len :int)
  (stat :pointer)) ;; TODO: This is a Stat thing? Implement

(cffi:defcfun ("zoo_exists" %zoo-exists) :int
  (zhandle :pointer)
  (path :string)
  (watch :int)
  (stat :pointer))

(defun exists? (zhandle path watch stat)
  (let* ((exists? (%zoo-exists zhandle path watch stat))
         (exists-kw (cffi:foreign-enum-keyword 'zoo-errors exists?)))
    (ccase exists-kw
      (:zok T)
      (:znonode NIL)
      ;; This should signal an error since people would use this like:
      ;; (when (exists? ..)) and then the kw returned would be truthy
      (t exists-kw))))

(cffi:defcfun zoo-state :int
  (zhandle :pointer))

(defun make-c-connection (host &key (timeout 10000) (clientid np))
  (let ((conn (zookeeper-init host np timeout clientid np 0)))
    (if (eq np conn)
        ;; TODO: make this a signal/restart
        (error (format nil "Error creating connection: ~d" *errno*))
        conn)))

(cffi:defcallback logcb :void ((message :string))
  (format t "~A~%" message))

(defun make-c-connection2 (host logcallback &key (timeout 10000) (clientid np))
  ;; TODO: This blocks when a debug connection tries to do something with it.
  ;; TODO: Debug^^
  (zookeeper-init2 host np timeout clientid np 0 logcallback))
