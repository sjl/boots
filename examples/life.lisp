(ql:quickload '(:boots :alexandria))

(defpackage :boots/examples/life
  (:use :cl)
  (:export :run))

(in-package :boots/examples/life)

(defparameter *world-width* 78)
(defparameter *world-height* 23)
(defparameter *world* (make-array (list *world-width* *world-height*) :initial-element nil))
(defparameter *buffer* (make-array (list *world-width* *world-height*) :initial-element nil))
(defparameter *cursor* #c(0 0))
(defparameter *running* nil)
(defparameter *frameskip* 1)
(defparameter *generation* 0)
(defparameter *cursor-attr* (boots:attr :bold t :bg (boots:rgb 255 100 130)))
(defparameter *help-text*
  (remove #\| (format nil "|hjklyubn: move~@
                           |  return: toggle cell~@
                           |   space: tick~@
                           |       p: play/pause~@
                           |       q: quit~@
                           |       >: increase frameskip~@
                           |       <: decrease frameskip")))

(defun x (pos) (realpart pos))
(defun y (pos) (imagpart pos))

(defmacro doworld ((pos) &body body)
  (alexandria:with-gensyms (x y)
    `(dotimes (,x *world-width*)
       (dotimes (,y *world-height*) 
         (let ((,pos (complex ,x ,y)))
           ,@body)))))

(defun wref (array pos)
  (when (and (< -1 (x pos) *world-width*)
             (< -1 (y pos) *world-height*))
    (aref array (x pos) (y pos))))

(defun (setf wref) (new-value array pos)
  (setf (aref array (x pos) (y pos)) new-value))

(defun count-neighbors (pos &aux (result 0))
  (loop :for x :from (1- (x pos)) :to (1+ (x pos))
        :do (loop :for y :from (1- (y pos)) :to (1+ (y pos))
                  :for p = (complex x y)
                  :when (and (/= p pos) (wref *world* p))
                  :do (incf result)))
  result)

(defun tick ()
  (loop
    :repeat *frameskip* :do
    (doworld (pos)
      (let ((neighbors (count-neighbors pos)))
        (setf (wref *buffer* pos)
              (if (wref *world* pos)
                (<= 2 neighbors 3)
                (= neighbors 3)))))
    (rotatef *world* *buffer*)
    (incf *generation*)))

(defun draw-world (pad)
  (doworld (pos)
    (boots:draw pad (x pos) (y pos)
                (if (wref *world* pos) #\O #\space)
                (if (= pos *cursor*) *cursor-attr* (boots:default)))))

(defun toggle ()
  (setf (wref *world* *cursor*) (not (wref *world* *cursor*))))

(defun clamp ()
  (setf *cursor*
        (complex (alexandria:clamp (realpart *cursor*) 0 (1- *world-width*))
                 (alexandria:clamp (imagpart *cursor*) 0 (1- *world-height*)))))

(defparameter *help-box*
  (boots:canvas (:height 7 :border t :width 30
                 :margin-top t :margin-left t) (pad)
    (boots:draw pad 0 0 *help-text*)))

(defparameter *status-bar*
  (boots:canvas (:height 1 :border t :border-bottom nil :width *world-width*
                 :margin-top t :margin-horizontal t) (pad)
    (boots:draw pad 0 0 (format nil "Frameskip ~D Generation ~D"
                                *frameskip* *generation*))))

(defparameter *world-display*
  (boots:make-canvas :width *world-width* :height *world-height*
                     :border t :border-top nil
                     :margin-horizontal t
                     :margin-bottom t
                     :draw #'draw-world))

(defun run ()
  (boots:with-light-borders
    (boots/terminals/ansi:with-ansi-terminal (terminal)
      (boots:with-screen (nil terminal :root (boots:pile ()
                                               *help-box*
                                               (boots:stack ()
                                                 *status-bar*
                                                 *world-display*)))
        (loop (clamp)
              (when *running* (tick))
              (boots:redraw)
              (case (boots:read-event-no-hang)
                (#\newline (toggle))
                (#\space (tick))
                (#\p (setf *running* (not *running*)))
                (#\> (incf *frameskip*))
                (#\< (unless (plusp (decf *frameskip*))
                       (setf *frameskip* 1)))
                (#\h (incf *cursor* #c(-1 0)))
                (#\j (incf *cursor* #c(0 1)))
                (#\k (incf *cursor* #c(0 -1)))
                (#\l (incf *cursor* #c(1 0)))
                (#\y (incf *cursor* #c(-1 -1)))
                (#\u (incf *cursor* #c(1 -1)))
                (#\b (incf *cursor* #c(-1 1)))
                (#\n (incf *cursor* #c(1 1)))
                (#\q (return-from run))
                ((nil) (sleep 1/30))))))))
