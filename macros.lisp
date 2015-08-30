(in-package :cl-zk)


(defun make-clos-field (name)
  (let ((keyword-name (values (alexandria:make-keyword name))))
    `(,name :accessor ,name :initarg ,keyword-name)))

(defun make-setf-block (fieldspec obj stream)
  (let ((name (first fieldspec)))
    `(setf (,name ,obj) ,(reader-for-type fieldspec stream))))

(defun reader-for-type (fieldspec stream)
  (let ((type (second fieldspec)))
    (ccase type
      (int `(read-int ,stream :signed? t))
      (bigint `(read-sized 64 ,stream))
      (byte-array
       `(let* ((size (read-int ,stream))
               (buf  (make-array size :element-type '(unsigned-byte 8))))
          (assert (= (read-sequence buf ,stream :end size) size))
          buf))
      (boolean
       `nil))))

(defun writer-for-type (fieldspec stream)
  (let ((name (first fieldspec))
        (type (second fieldspec))
        (buf-sym (gensym)))
    (ccase type
      (int `(write-int ,name ,stream))
      (bigint `(write-bigint ,name ,stream))
      (byte-array
       `(let ((,buf-sym (as-bytes ,name)))
          (write-length ,buf-sym ,stream)
          (write-sequence ,buf-sym ,stream)))
      (boolean
       `(if ,name
            (write-byte 1 ,stream)
            (write-byte 0 ,stream))))))

(defmacro define-message (name type-id superclasses fields)
  (let* ((xid-sym (gensym))
         (names (mapcar #'car fields))
         (name-slots (cons `(,xid-sym xid) (mapcar #'car fields)))
         (value-sym (gensym))
         (stream-sym (gensym)))
    `(progn
       (defclass ,name ,superclasses
         ((xid :accessor xid :initarg :xid :initform :unset)
          ,@(mapcar #'make-clos-field names)))
       (defmethod encode-value ((,value-sym ,name) ,stream-sym)
         (with-slots ,name-slots ,value-sym
           (unless (eq ,xid-sym :unset)
             (write-int ,xid-sym ,stream-sym))
           ,(if type-id
                `(write-int ,type-id ,stream-sym))
           ,@(mapcar (lambda (p)
                       (writer-for-type p stream-sym)) fields)))
       (defmethod decode-value ((,value-sym ,name) ,stream-sym)
         (read-int ,stream-sym) ;; The size of the packet coming
         ,@(mapcar (lambda (p)
                     (make-setf-block p value-sym stream-sym))  fields)
         ,value-sym))))
