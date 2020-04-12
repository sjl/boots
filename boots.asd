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

  :serial t
  :components
  ((:module "src" :serial t :components
    ((:file "package")
     (:file "utils")
     (:file "attributes")
     (:file "events")
     (:module "terminals" :serial t :components
      ((:file "protocol")
       (:cffi-grovel-file "ansi-grovel")
       (:file "ansi")))
     (:file "widgets")
     (:file "sizing")
     (:file "drawing")
     (:file "api")))))
