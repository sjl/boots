(ql:quickload :boots)

(defpackage :boots/examples/timer
  (:use :cl)
  (:export :run))

(in-package :boots/examples/timer)


(defparameter *started* nil)
(defparameter *total* nil)
(defparameter *states* '#1=(:ready :active :stopped . #1#))
(defparameter *state* (pop *states*))

(defun elapsed ()
  (/ (- (get-internal-real-time) *started*)
     internal-time-units-per-second))

(defparameter *w-timer*
  (boots:canvas (:height 2 :margin-top t :margin-bottom t) (pad)
    (boots:draw pad 0 1 (format nil "~A ~Sx~S" (symbol-name *state*)
                                (boots:width pad)
                                (boots:height pad)))
    (let ((s (ecase *state*
               (:ready "Press space to start/stop/rest, q to quit.")
               (:active (format nil "~,2F" (elapsed)))
               (:stopped (format nil "~,2F" *total*)))))
      (boots:draw pad (truncate (- (boots:width pad) (length s)) 2) 0 s))))

(defun handle-press ()
  (ecase *state*
    (:ready (setf *started* (get-internal-real-time)))
    (:active (setf *total* (elapsed)))
    (:stopped (setf *started* nil *total* nil)))
  (setf *state* (pop *states*)))

(defun run ()
  (boots:with-boots (boots/terminals/ansi:make-ansi-terminal)
    (setf (boots:root boots:*screen*) *w-timer*)
    (loop
      (boots:redraw)
      (case (boots:read-event-no-hang)
        (#\q (return))
        (#\space (handle-press))
        ((nil) (sleep 1/60))))))

(run)
