(ql:quickload :boots)

(defpackage :boots/examples/layout
  (:use :cl)
  (:export :run))

(in-package :boots/examples/layout)

(defparameter *example*
  (boots:stack ()
    (boots:pile (:height 10)
      (boots:make-canvas :border t :width 8 :height 4 :margin-top 1 :margin-left 1)
      (boots:make-canvas :border t :width 8 :height 4 :margin-top 2 :margin-left 4)
      (boots:make-canvas :border t :width 8 :height 4 :margin-top 3 :margin-left 7))
    (boots:shelf ()
      (boots:make-canvas :border-right t)
      (boots:pile ()
        (boots:stack (:border t :margin t :margin-top 0.15)
          (boots:make-canvas :height 1 :border-bottom t)
          (boots:make-canvas)
          (boots:make-canvas :height 1 :border-top t))
        (boots:stack ()
          (boots:make-canvas)
          (boots:make-canvas)))
      (boots:make-canvas :border-left t))))

(defun run ()
  (boots/terminals/ansi:with-ansi-terminal (terminal)
    (boots:with-screen (boots:*screen* terminal :root *example*)
      (boots:redraw)
      (boots:read-event))))
