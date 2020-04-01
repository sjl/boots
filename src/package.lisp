(defpackage :boots/utils
  (:use :cl)
  (:export :_ :defun-inline :defclass*))

(defpackage :boots/attributes
  (:use :cl :boots/utils)
  (:export
    :attribute
    :attr :rgb :rgb*
    :with-fg :with-bg
    :boldp :italicp :underlinep))

(defpackage :boots/events
  (:use :cl :boots/utils)
  (:export
    :event))

(defpackage :boots/widgets
  (:use :cl :boots/utils)
  (:export
    :event))

(defpackage :boots/terminals
  (:use :cl :boots/utils)
  (:export :terminal :blit :draw :read-event :read-event-no-hang))

(defpackage :boots/terminals/ansi
  (:use :cl
    :boots/utils
    :boots/terminals
    :boots/attributes)
  (:export))

(defpackage :boots
  (:use :cl :boots/utils)
  (:export))
