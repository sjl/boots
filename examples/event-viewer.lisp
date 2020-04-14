(ql:quickload :boots)

(defpackage :boots/examples/event-viewer
  (:use :cl)
  (:export :run))

(in-package :boots/examples/event-viewer)

(defparameter *events* (make-list 10 :initial-element nil))

(defparameter *ui* (boots:canvas (:width 30 :height 11 :border t :margin t) (pad)
                     (boots:draw pad 0 0 "Press Q to quit.")
                     (boots:draw pad 0 1 (format nil "~{~S~%~}" *events*))))

(defun run ()
  (boots/terminals/ansi:with-ansi-terminal (terminal)
    (boots:with-screen (boots:*screen* terminal :root *ui*)
      (loop
        (boots:redraw)
        (let ((event (boots:read-event)))
          (case event
            (#\Q (return))
            (t (progn (push event *events*)
                      (setf *events* (subseq *events* 0 10))))))))))
