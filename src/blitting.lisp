(in-package :boots)

(defmethod blit (screen)
  (terpri)
  (dotimes (y (height% screen))
    (dotimes (x (width% screen))
      (write-char (aref (text% screen) x y)))
    (terpri)))
