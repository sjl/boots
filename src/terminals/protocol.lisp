(in-package :boots/terminals)

(defclass* terminal ()
  ((width :type size)
   (height :type size)))

(defgeneric prep (terminal mode)
  (:documentation
    "Prepare `terminal` for drawing.

  `prep` will prepare `terminal` for drawing by (possibly) resizing it.
  `mode` must be one of `:full`, `:normal`, or `:minimal`.

  * `:full` means *always* resize and *always* draw.
  * `:default` means only resize if the underlying size actually changed, and *always* draw.
  * `:minimal` means only resize and only draw if the underlying size actually changed.

  "))

(defgeneric blit (terminal)
  (:documentation
   "Blit the contents of `terminal` to the underlying output.

  This should be called after `redraw`ing everything.

   "))

(defgeneric draw-region (terminal x y width height characters attributes))

(defgeneric read-event (terminal))
(defgeneric read-event-no-hang (terminal))

(defmethod read-event (terminal)
  (loop
    :for event = (read-event-no-hang terminal)
    :if event :do (return event)
    :else :do (sleep 1/60)))

