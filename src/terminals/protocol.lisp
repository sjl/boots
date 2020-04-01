(in-package :boots/terminals)

(defclass terminal () ())

(defgeneric blit (terminal))

(defgeneric draw (terminal x y thing &optional attr))

(defgeneric read-event (terminal))

(defgeneric read-event-no-hang (terminal))
