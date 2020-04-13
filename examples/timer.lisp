(ql:quickload :boots)

(defpackage :boots/examples/timer
  (:use :cl)
  (:export :run))

(in-package :boots/examples/timer)


(defparameter *started* nil)
(defparameter *total* nil)
(defparameter *states* '#1=(:ready :active :stopped . #1#))
(defparameter *state* (pop *states*))

(defconstant +bold+ (boots:attr :bold t))
(defconstant +ital-green+ (boots:attr :italic t :fg (boots:rgb 60 255 99)))
(defconstant +bold-red+ (boots:attr :bold t :fg (boots:rgb 255 60 99)))

(defun elapsed ()
  (/ (- (get-internal-real-time) *started*)
     internal-time-units-per-second))

(defun rendered-length (thing)
  (typecase thing
    (list (reduce #'+ thing :key #'rendered-length))
    (string (length thing))
    (character 1)
    (t 0)))

(defun draw-centered (pad y thing)
  (boots:draw pad (truncate (- (boots:width pad) (rendered-length thing)) 2)
              y
              thing))

(defparameter *w-timer*
  (boots:canvas (:height 2 :margin-top t :margin-bottom t) (pad)
    (draw-centered pad 0 (format nil "~,2F" (ecase *state*
                                              (:ready 0)
                                              (:active (elapsed))
                                              (:stopped *total*))))
    (draw-centered pad 1 (list "Press "
                               +bold+ "space" nil " to start/stop/reset, "
                               +ital-green+ "C" nil " to switch between truecolor/256color, "
                               +bold-red+ "q" nil " to quit."))))

(defparameter *w-bar*
  (boots:canvas (:height 3) (pad)
    (dotimes (v 256)
      (boots:draw pad v 0 #\Space (boots:attr :bg (boots:rgb v 0 0)))
      (boots:draw pad v 1 #\Space (boots:attr :bg (boots:rgb 0 v 0)))
      (boots:draw pad v 2 #\Space (boots:attr :bg (boots:rgb 0 0 v))))))

(defun handle-press ()
  (ecase *state*
    (:ready (setf *started* (get-internal-real-time)))
    (:active (setf *total* (elapsed)))
    (:stopped (setf *started* nil *total* nil)))
  (setf *state* (pop *states*)))

(defun run ()
  (boots/terminals/ansi:with-ansi-terminal (terminal)
    (boots:with-screen (boots:*screen* terminal :root (boots:stack ()
                                                        *w-bar*
                                                        *w-timer*))
      (loop
        (boots:redraw)
        (case (boots:read-event-no-hang)
          (#\q (return))
          (#\space (handle-press))
          (#\C (setf (boots/terminals/ansi::truecolor terminal)
                     (not (boots/terminals/ansi::truecolor terminal))))
          ((nil) (sleep 1/60)))))))

;; (run)
