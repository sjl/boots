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
(defconstant +bold-red+ (boots:attr :bold t :fg (boots:rgb 255 60 99)))

(defun elapsed ()
  (/ (- (get-internal-real-time) *started*)
     internal-time-units-per-second))

(defun elapsed-string ()
  (let ((sec (elapsed)))
    (if (< sec 60)
      (format nil "~5,2,,,'0F"
              (mod sec 60))
      (format nil "~2,'0D:~5,2,,,'0F"
              (truncate sec 60)
              (mod sec 60)))))

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
                                              (:active (elapsed-string))
                                              (:stopped *total*))))
    (draw-centered pad 1 (list "Press "
                               +bold+ "space" nil " to start/stop/reset, "
                               +bold-red+ "q" nil " to quit."))))

(defun handle-press ()
  (ecase *state*
    (:ready (setf *started* (get-internal-real-time)))
    (:active (setf *total* (elapsed)))
    (:stopped (setf *started* nil *total* nil)))
  (setf *state* (pop *states*)))

(defun run ()
  (boots/terminals/ansi:with-ansi-terminal (terminal)
    (boots:with-screen (boots:*screen* terminal :root *w-timer*)
      (loop
        (boots:redraw)
        (case (boots:read-event-no-hang)
          (#\q (return))
          (#\space (handle-press))
          ((nil) (sleep 1/60)))))))

;; (run)
