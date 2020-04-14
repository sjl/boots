(ql:quickload '(:boots :bobbin))

(defpackage :boots/examples/colors
  (:use :cl)
  (:export :run))

(in-package :boots/examples/colors)


(defparameter *help*
  (boots:canvas (:width 50 :height 3 :padding 1 :margin-left t :margin-right t) (pad)
    (boots:draw pad 0 0
                (format nil "space to switch colors~%c to switch between truecolor & 256color~%Q to quit"))))

(defun color (state y x)
  (boots:attr :bg (ecase state
                    (0 (boots:rgb 0 x y))
                    (1 (boots:rgb x 0 y))
                    (2 (boots:rgb x y 0))
                    (3 (boots:rgb x x y))
                    (4 (boots:rgb x y x))
                    (5 (boots:rgb y x x))
                    (6 (boots:rgb x x x))
                    (7 (boots:rgb y y y)))))

(defparameter *colors*
  (let ((colors (make-array (list 8 64 128))))
    (dotimes (s 8)
      (dotimes (y 64)
        (dotimes (x 128)
          (setf (aref colors s y x) (color s (* 4 y) (* 2 x))))))
    colors))

(defparameter *state* 0)

(defparameter *color-window*
  (boots:canvas (:width 128 :height 64 :margin t :border t) (pad)
    (let ((s *state*))
      (dotimes (y 64)
        (dotimes (x 128)
          (boots:draw pad x y #\space (aref *colors* s y x)))))))

(defun run ()
  (boots/terminals/ansi:with-ansi-terminal (terminal)
    (boots:with-screen (screen terminal :root (boots:stack () *help* *color-window*))
      (loop
        (boots:redraw)
        (case (boots:read-event)
          (#\Q (return))
          (#\space (setf *state* (mod (1+ *state*) 8)))
          (#\c (progn (setf (boots/terminals/ansi::truecolor terminal)
                            (not (boots/terminals/ansi::truecolor terminal)))
                      (boots:redraw :full t))))))))
