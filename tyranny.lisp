(in-package :cl-zk)

(defun make-pointer-to-int (int-value)
  (cffi:foreign-alloc :int :initial-element int-value))


;; TODO: Seriously? Train, no sleep, no internet. Will fix.
(defmacro with-pointer-to-int (binding-form &body body)
  (let ((name (first binding-form))
        (value (second binding-form)))
    `(let ((,name (make-pointer-to-int ,value)))
       (unwind-protect
            (progn
              ,@body)
         (cffi:foreign-free ,name)))))
