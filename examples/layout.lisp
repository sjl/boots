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

;;; The resulting layout will look something like this (depending on the
;;; terminal size):
;;;
;;; ┌────────┐
;;; │CCCCCCCC│──┐
;;; │CCCCCCCC│UU│──┐
;;; │CCCCCCCC│UU│SS│
;;; │CCCCCCCC│UU│SS│
;;; └────────┘UU│SS│
;;;    └────────┘SS│
;;;       └────────┘
;;;
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJJJJJJJJJJJJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJJJJJJJJJJJJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJJJJJJJJJJJJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ┌───────┐JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│fffffff│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│───────│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│HHHHHHH│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│HHHHHHH│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│HHHHHHH│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│HHHHHHH│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│HHHHHHH│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│HHHHHHH│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│JJJJJJJ│───────│JJJJJJJ│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLL│RRRRRRR│LLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLL└───────┘LLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee
;;;FFFFFFFFFFFFFFFFFFFFFFF│LLLLLLLLLLLLLLLLLLLLLLL│eeeeeeeeeeeeeeeeeeeeeee

(defun run ()
  (boots/terminals/ansi:with-ansi-terminal (terminal)
    (boots:with-screen (boots:*screen* terminal :root *example*)
      (boots:redraw)
      (boots:read-event))))
