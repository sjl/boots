(asdf:defsystem :boots
  :description "A simple text UI framework, inspired by _why's Shoes."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :defsystem-depends-on (:cffi-grovel)
  :depends-on (

               :alexandria
               :iterate
               :losh
               :cffi

               )

  :serial t
  :components
  ((:module "src" :serial t :components
    ((:file "package")
     (:file "widgets")
     (:file "sizing")
     (:file "blitting")
     (:file "drawing")
     (:module "backends" :serial nil
      :components
      ((:module "terminal" :serial t
        :components ((:cffi-grovel-file "grovel")
                     (:file "main")))))

     (:file "api")))))
