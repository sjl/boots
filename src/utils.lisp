(in-package :boots/utils)

(defmacro defun-inline (name args &body body)
  "Like `defun`, but declaims `name` to be `inline`."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)
     ',name))

(defmacro _ (expr &rest forms)
  "Thread the given forms, with `_` as a placeholder."
  `(let* ((_ ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(_ (,form _))
                        `(_ ,form)))
                    forms))
     _))

(defmacro defclass* (name-and-options direct-superclasses slots &rest options)
  "`defclass` without the tedium.

  This is like `defclass`, but the `:initarg` and `:accessor` slot options will
  automatically be filled in with sane values if they aren't given.

  `name-and-options` can be a symbol or a list, which will be destructured
  against `(name &key conc-name)`.

  "
  (destructuring-bind (name &key conc-name)
      (alexandria:ensure-list name-and-options)
    (flet ((build-slot-definition (slot-spec)
             (destructuring-bind
                 (name &rest slot-options
                       &key
                       (accessor (if conc-name
                                   (alexandria:symbolicate conc-name name)
                                   name)
                                 accessor?)
                       ;; work around https://bugs.launchpad.net/sbcl/+bug/1870004
                       (initarg (values (alexandria:make-keyword name)) initarg?)
                       &allow-other-keys)
                 (alexandria:ensure-list slot-spec)
               `(,name
                  ,@(unless accessor? `(:accessor ,accessor))
                  ,@(unless initarg? `(:initarg ,initarg))
                  ,@slot-options))))
      `(defclass ,name ,direct-superclasses
         ,(mapcar #'build-slot-definition slots)
         ,@options))))

