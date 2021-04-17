(ql:quickload :boots)

(defpackage :boots/examples/event-viewer
  (:use :cl)
  (:export :run))

(in-package :boots/examples/event-viewer)

(defparameter *events* (make-list 10 :initial-element nil))

(defparameter *ui* (boots:canvas (:width 50 :height 11 :border t :margin t) (pad)
                     (boots:draw pad 0 0 "Press Q to quit.")
                     (loop :for row :from 1
                           :for (event . modifiers) in *events*
                           :for s = (format nil "~S ~A" event (boots%::print-modifiers modifiers nil))
                           :do (boots:draw pad 0 row s))))

(defun run ()
  (boots/terminals/ansi:with-ansi-terminal (terminal)
    (boots:with-screen (boots:*screen* terminal :root *ui*)
      (fill *events* (cons nil nil))
      (loop
        (setf *events* (subseq *events* 0 10))
        (boots:redraw)
        (multiple-value-bind (e m) (boots:read-event)
          (boots:event-case (values e m)
            (#\Q (return))
            (#\newline (push (cons :return 0) *events*))
            (#\escape (push (cons :escape 0) *events*))
            (t (push (cons e m) *events*))))))))

