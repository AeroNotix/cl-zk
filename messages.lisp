(in-package :cl-zk)


(define-message connect-request nil ()
  ((protocol-version int)
   (last-zxid-seen bigint)
   (timeout int)
   (session-id bigint)
   (password byte-array)
   (read-only boolean)))

(define-message connect-response nil ()
  ((protocol-version int)
   (timeout int)
   (session-id bigint)
   (password byte-array)))

(define-message get-data-request 4 ()
  ((path byte-array)
   (watch boolean)))

(define-message get-children-request 8 ()
  ((path byte-array)
   (watch boolean)))

(define-message ping 11 ()
  ())
