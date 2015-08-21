(defsystem :cl-zk
  :author "Aaron France"
  :version "0.0.1"
  :license "AL2"
  :description "Zookeeper client"
  :serial t
  :components ((:file "package"))
  :depends-on (:alexandria
               :flexi-streams
               :split-sequence
               :bordeaux-threads
               :uuid
               :usocket)
  :defsystem-depends-on (:fiveam)
  :in-order-to ((test-op (test-op :cl-zk-test))))

(defsystem :cl-zk-test
  :version "0.0.1"
  :description "Zookeeper client tests"
  :licence "BSD"
  :components ((:module "test"
                        :components
                        ((:file "tests"))))
  :depends-on (:cl-zk :fiveam :alexandria :flexi-streams :uuid)
  :perform (test-op :after (o s)
                    (fiveam:run! :cl-zk)))
