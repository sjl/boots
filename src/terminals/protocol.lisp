(in-package :boots/terminals)

(defclass* terminal ()
  ((width)
   (height)))

(defgeneric blit (terminal))

(defgeneric put (terminal x y character &optional attr))
(defgeneric paint (terminal x y width height character &optional attr))

(defgeneric start (terminal))
(defgeneric stop (terminal))

(defgeneric read-event (terminal))

(defmethod read-event (terminal)
  (loop
    :for event = (read-event-no-hang terminal)
    :if event :do (return event)
    :else :do (sleep 1/60)))

(defgeneric read-event-no-hang (terminal))

