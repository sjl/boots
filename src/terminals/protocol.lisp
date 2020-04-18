(in-package :boots/terminals)

(defclass* terminal ()
  ((width :type size)
   (height :type size)))

(defgeneric prep (terminal full))
(defgeneric blit (terminal))

(defgeneric put (terminal x y character &optional attr))
(defgeneric paint (terminal x y width height character &optional attr))

(defgeneric read-event (terminal))
(defgeneric read-event-no-hang (terminal))

(defmethod read-event (terminal)
  (loop
    :for event = (read-event-no-hang terminal)
    :if event :do (return event)
    :else :do (sleep 1/60)))

