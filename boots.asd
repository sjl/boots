(asdf:defsystem :boots
  :description "A simple text UI framework, inspired by _why's Shoes."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (

               :cl-charms
               :iterate
               :losh

               )

  :serial t
  :components
  ((:module "vendor" :serial t
    :components ((:file "quickutils-package")
                 (:file "quickutils")))
   (:file "package")
   (:module "src" :serial t
    :components
    ((:file "main")))))
