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
        (type (second fieldspec)))
    (ccase type
      (int `(write-int ,name ,stream))
      (bigint `(write-bigint ,name ,stream))
      (byte-array
       `(encode-value (as-bytes ,name) ,stream))
      (boolean
       `(if ,name
            (write-int 1 ,stream)
            (write-int 0 ,stream))))))

(defmacro define-message (name type-id superclasses fields)
  (let ((names (mapcar #'car fields))
        (value-sym (gensym))
        (stream-sym (gensym)))
    `(progn
       (defclass ,name ,superclasses
         ,(mapcar #'make-clos-field names))
       (defmethod encode-value ((,value-sym ,name) ,stream-sym)
         (with-slots ,names ,value-sym
           (if ,type-id ;; Gensym this
               (write-int ,type-id ,stream-sym))
           ,@(mapcar (lambda (p)
                       (writer-for-type p stream-sym)) fields)))
       (defmethod decode-value ((,value-sym ,name) ,stream-sym)
         (read-int ,stream-sym) ;; The size of the packet coming
         ,@(mapcar (lambda (p)
                     (make-setf-block p value-sym stream-sym))  fields)
         ,value-sym))))
