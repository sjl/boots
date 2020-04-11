(in-package :boots/terminals)

(defclass* terminal ()
  ((width)
   (height)))

(defgeneric blit (terminal))

(defgeneric put (terminal x y character &optional attr))

(defgeneric read-event (terminal))

(defmethod read-event (terminal)
  (loop
    :for event = (read-event-no-hang terminal)
    :if event :do (return event)
    :else :do (sleep 1/60)))

(defgeneric read-event-no-hang (terminal))

