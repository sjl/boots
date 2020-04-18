(in-package :boots/terminals/static)

;;; A terminal that just stores its data, for testing.


(defclass* static-terminal (terminal)
  ((events :type list)
   ;; https://github.com/Clozure/ccl/issues/291
   (characters #-ccl :type #-ccl char-array)
   (attributes #-ccl :type #-ccl attr-array)))

(defun make-static-terminal (width height events)
  (make-instance 'static-terminal
    :width width
    :height height
    :events events
    :characters (make2d height width 'character #\space)
    :attributes (make2d height width 'attribute (default))))

(defmacro with-static-terminal ((symbol &key width height events) &body body)
  `(let ((,symbol (make-static-terminal ,width ,height ,events)))
     ,@body))

(defmethod prep ((terminal static-terminal) full)
  (fill2d (characters terminal) #\space)
  (fill2d (attributes terminal) (default)))

(defmethod blit ((terminal static-terminal))
  nil)

(defmethod put ((terminal static-terminal) x y character &optional attr)
  (setf (aref (characters terminal) y x) character
        (aref (attributes terminal) y x) (or attr (default)))
  nil)

(defmethod paint ((terminal static-terminal) x y width height character &optional attr)
  (loop :for x% :from x :below (+ x width) :do
        (loop :for y% :from y :below (+ y height) :do
              (setf (aref (characters terminal) y% x%) character
                    (aref (attributes terminal) y% x%) (or attr (default))))))

(defmethod read-event-no-hang ((terminal static-terminal))
  (pop (events terminal)))

(defun stringify (terminal)
  (with-output-to-string (s)
    (dotimes (y (height terminal))
      (unless (zerop y)
        (terpri s))
      (dotimes (x (width terminal))
        (write-char (aref (characters terminal) y x) s)))))
