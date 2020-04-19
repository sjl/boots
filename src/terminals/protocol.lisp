(in-package :boots/terminals)

(defclass* terminal ()
  ((width :type size)
   (height :type size)))

(defgeneric prep (terminal full))
(defgeneric blit (terminal))

(defgeneric draw-region (terminal x y width height characters attributes))

(defgeneric read-event (terminal))
(defgeneric read-event-no-hang (terminal))

(defmethod read-event (terminal)
  (loop
    :for event = (read-event-no-hang terminal)
    :if event :do (return event)
    :else :do (sleep 1/60)))

