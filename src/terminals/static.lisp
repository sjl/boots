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

(defmethod prep ((terminal static-terminal) mode)
  (fill2d (characters terminal) #\space)
  (fill2d (attributes terminal) (default))
  t)

(defmethod blit ((terminal static-terminal))
  nil)


(defmethod draw-region ((terminal static-terminal) x y width height characters attributes)
  (require-type characters char-array)
  (require-type attributes attr-array)
  (loop :for x% :from x :below (+ x width) :do
        (loop :for y% :from y :below (+ y height)
              :for c = (aref characters y% x%)
              :for a = (aref attributes y% x%)
              :unless (char= c #\nul)
              :do (setf (aref (characters terminal) y% x%) c
                        (aref (attributes terminal) y% x%) a))))


(defmethod read-event-no-hang ((terminal static-terminal))
  (pop (events terminal)))


(defun stringify (terminal)
  (with-output-to-string (s)
    (dotimes (y (height terminal))
      (unless (zerop y)
        (terpri s))
      (dotimes (x (width terminal))
        (write-char (aref (characters terminal) y x) s)))))
