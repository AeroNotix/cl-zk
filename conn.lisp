(in-package :cl-zk)

;; TODO: Have the Makefile make install the proper .so file
;; TODO: Document how to have this work properly
(cffi:define-foreign-library zookeeper
  (:unix
   (:or "libzookeeper.so"
        "libzookeeper_mt.so"
        "libzookeeper_st.so")))

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

(cffi:defcfun ("zoo_get" %zoo-get) :int
  (zhandle :pointer)
  (path :string)
  (watch :int)
  (buffer :pointer)
  (buffer-len :pointer)
  (stat :pointer)) ;; TODO: This is a Stat thing? Implement

(defun get-data (zhandle path watch stat)
  ;; TODO: How to handle dynamically sized get-data calls, without
  ;; resorting to just allocating a buffer the max size of data
  ;; allowed?
  (let ((buf-size 512))
    (cffi:with-foreign-object (buf :char buf-size)
      (with-pointer-to-int (i buf-size)
        (let* ((get-result (%zoo-get zhandle path watch buf i stat))
               (get-kw (cffi:foreign-enum-keyword 'zoo-errors get-result)))
          (ccase get-kw
            ;; TODO: handle other cases
            (:zok
             (cffi:foreign-string-to-lisp buf))))))))

(cffi:defcfun ("zoo_set" %zoo-set) :int
  (zhandle :pointer)
  (path :string)
  (buffer :pointer)
  (buffer-len :int)
  (version :int))

;; TODO: defgeneric this have it operate on strings and buffers and
;; streams and all that good stuff. Implement it with just strings
;; now.
(defun set-data (zhandle path data &key (version -1))
  (cffi:with-foreign-string (buf data)
    (%zoo-set zhandle path buf (+ (length data) 1) version)))

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
  ;; TODO: This blocks when a debugged connection tries to do something with it.
  ;; TODO: Debug^^
  (zookeeper-init2 host np timeout clientid np 0 logcallback))
