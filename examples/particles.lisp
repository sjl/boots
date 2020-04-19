(ql:quickload :boots)

(defpackage :boots/examples/particles
  (:use :cl)
  (:export :run))

(in-package :boots/examples/particles)

;;;; Utilities ----------------------------------------------------------------
(deftype vec2 () '(complex single-float))
(defconstant tau (coerce (* 2 pi) 'single-float))
(defparameter *gravity* #c(0.0 30.0))
(defparameter *cursor-attr* (boots:attr :bg (boots:rgb #x0A #x9D #xFF)))

(declaim (type vec2 *gravity*)
         (inline x y))

(defun x (loc)
  (realpart loc))

(defun y (loc)
  (imagpart loc))

(defun random-color ()
  (boots:rgb (random 256) (random 256) (random 256)))

(defun random-range (lo hi)
  (+ lo (random (- hi lo))))

(defun random-direction ()
  (let ((a (random tau)))
    (complex (cos a) (sin a))))


;;;; Particles -----------------------------------------------------------------
(defvar *particles* nil)

(defstruct (particle (:conc-name nil)
                     (:constructor make-particle%))
  (loc #c(0.0 0.0) :type vec2)
  (glyph #\* :type character)
  (attr (boots:default) :type boots:attribute)
  (velocity #c(0.0 0.0) :type vec2)
  (lifetime 0.0 :type single-float))

(defun draw-particle (pad p)
  (declare (optimize speed)
           (type particle p))
  (boots:draw pad (round (x (loc p))) (round (y (loc p))) (glyph p) (attr p)))

(defun make-particle (loc)
  (push (make-particle%
          :loc (coerce loc '(complex single-float))
          :glyph (alexandria:random-elt "*`,.")
          :attr (boots:attr :fg (random-color))
          :lifetime (random-range 2.0 6.0)
          :velocity (* (random-direction)
                       (random-range 10.0 40.0)))
        *particles*))

(declaim (inline tick-particle))
(defun tick-particle (p elapsed)
  (declare (optimize speed)
           (type particle p)
           (type single-float elapsed))
  (decf (lifetime p) elapsed)
  (incf (loc p) (* (velocity p) elapsed))
  (incf (velocity p) (* *gravity* elapsed)))


;;;; Rendering ----------------------------------------------------------------
(defparameter *mspf* 0.0)
(defvar *cursor* #c(0 0))

(defun draw/main (pad)
  (setf *cursor* (complex (alexandria:clamp (x *cursor*) 0 (1- (boots:width pad)))
                          (alexandria:clamp (y *cursor*) 0 (1- (boots:height pad)))))
  (dolist (p *particles*)
    (draw-particle pad p))
  (boots:draw pad (x *cursor*) (y *cursor*) #\space *cursor-attr*))

(defparameter *help-attr*
  (boots:attr :bold t
              :fg (boots:rgb 0 0 0)
              :bg (boots:rgb 255 255 255)))

(defun draw/help (pad)
  (boots:draw pad 0 0 "[wasd] or [hjklyubn] move, [space] add particles, [q]uit"
              *help-attr*)
  (boots:draw pad 0 1 (format nil "MSPF: ~,2F" *mspf*) *help-attr*))

(defparameter *ui*
  (boots:stack ()
    (boots:make-canvas :draw 'draw/main)
    (boots:make-canvas :height 2 :fill-attr *help-attr* :draw 'draw/help)))


;;;; Main Loop ----------------------------------------------------------------
(defun force-redraw ()
  (setf (boots:root boots:*screen*) *ui*)
  (boots:redraw :full t))

(defun add ()
  (dotimes (i (random-range 10 20))
    (make-particle *cursor*)))

(defun cull ()
  (setf *particles* (delete-if #'minusp *particles* :key #'lifetime)))

(defun tick (elapsed)
  (dolist (p *particles*)
    (tick-particle p elapsed)))

(defun handle-event (event)
  (case event
    (#\q :quit)
    (#\space (add))
    ((#\h #\a) (incf *cursor* #c(-1  0)))
    ((#\j #\s) (incf *cursor* #c( 0  1)))
    ((#\k #\w) (incf *cursor* #c( 0 -1)))
    ((#\l #\d) (incf *cursor* #c( 1  0)))
    (#\y (incf *cursor* #c(-1 -1)))
    (#\u (incf *cursor* #c( 1 -1)))
    (#\b (incf *cursor* #c(-1  1)))
    (#\n (incf *cursor* #c( 1  1)))))

(defun main-loop ()
  (loop
    :with elapsed
    :with prev = (get-internal-real-time)
    :for now = (get-internal-real-time)
    :do (progn
          (setf elapsed (coerce (/ (- now prev) internal-time-units-per-second)
                                'single-float)
                prev now
                *mspf* (* 1000.0 elapsed))
          (tick elapsed)
          (cull)
          (case (handle-event (boots:read-event-no-hang))
            (:quit (return)))
          (boots:redraw)
          (sleep 1/60))))

(defun run ()
  (setf *random-state* (make-random-state t))
  (boots/terminals/ansi:with-ansi-terminal (terminal :truecolor t)
    (boots:with-screen (boots:*screen* terminal :root *ui*)
      (main-loop))))
