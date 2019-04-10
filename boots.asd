(asdf:defsystem :boots
  :description "A simple text UI framework, inspired by _why's Shoes."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (

               :iterate
               :losh

               )

  :serial t
  :components
  ((:module "src" :serial t :components
    ((:file "package")
     (:file "main")))))
