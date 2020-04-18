(asdf:defsystem :boots
  :description "A simple text UI framework, inspired by _why's Shoes."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :defsystem-depends-on (:cffi-grovel)
  :depends-on (

               :alexandria
               :cffi
               :mansion

               )

  :in-order-to ((asdf:test-op (asdf:test-op :boots/test)))

  :serial t
  :components
  ((:module "src" :serial t :components
    ((:file "package")
     (:file "base")
     (:file "attributes")
     (:file "events")
     (:module "terminals" :serial t :components
      ((:file "protocol")
       (:cffi-grovel-file "ansi-grovel")
       (:file "ansi")
       (:file "static")))
     (:file "widgets")
     (:file "sizing")
     (:file "drawing")
     (:file "api")))))

(asdf:defsystem :boots/test
  :description "Test suite for Boots."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:boots :1am)

  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "boots/test:run-tests"))))
