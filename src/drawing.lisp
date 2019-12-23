(in-package :boots)

(defparameter *s* nil)

(defun fill-with-random-chars (canvas)
  (let ((ch (random-elt "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (dotimes (y (height% canvas))
      (dotimes (x (width% canvas))
        (setf (aref (text% *s*)
                    (+ (content-x% canvas) x)
                    (+ (content-y% canvas) y))
              ch)))))


(defgeneric draw (widget))

(defmethod draw ((s screen))
  (fill-multidimensional-array (text% s) #\space)
  (let ((*s* s))
    (draw (root s))))

(defmethod draw ((c canvas))
  (funcall (draw% c) c))

(defmethod draw ((c container))
  (map nil #'draw (children c)))


(defun draw-border (widget)
  (let* ((bt (border-top% widget))
         (br (border-right% widget))
         (bb (border-bottom% widget))
         (bl (border-left% widget))
         (btl (and (plusp bt) (plusp bl)))
         (btr (and (plusp bt) (plusp br)))
         (bbl (and (plusp bb) (plusp bl)))
         (bbr (and (plusp bb) (plusp br)))
         (c (text% *s*))
         (wx (window-x% widget))
         (wy (window-y% widget))
         (w (+ (padding-left% widget)
               (width% widget)
               (padding-right% widget)))
         (h (+ (padding-top% widget)
               (height% widget)
               (padding-bottom% widget))))
    ;; +-----+
    ;; |     |
    ;; |     |
    ;; +-----+
    (when (plusp bt)
      (loop :with y = wy :for x :from wx :repeat (+ w bl br) :do (setf (aref c x y) #\-)))
    (when (plusp bb)
      (loop :with y = (+ wy h bt) :for x :from wx :repeat (+ w bl br) :do (setf (aref c x y) #\-)))
    (when (plusp bl)
      (loop :with x = wx :for y :from wy :repeat (+ h bt bb) :do (setf (aref c x y) #\|)))
    (when (plusp br)
      (loop :with x = (+ wx w bl) :for y :from wy :repeat (+ h bt bb) :do (setf (aref c x y) #\|)))
    (when btl (setf (aref c wx wy) #\+))
    (when btr (setf (aref c (+ wx bl w) wy) #\+))
    (when bbl (setf (aref c wx (+ wy bt h)) #\+))
    (when bbr (setf (aref c (+ wx bl w) (+ wy bt h)) #\+))))

(defmethod draw :before ((w widget))
  (draw-border w))


(defun-inline resolve-pos (canvas x y)
  (when (floatp x)
    (setf x (round (* x (width% canvas)))))
  (when (floatp y)
    (setf y (round (* y (height% canvas)))))
  (values (if (minusp x)
            (+ (width% canvas) x)
            x)
          (if (minusp y)
            (+ (height% canvas) y)
            y)))

(defun-inline in-bounds-p (x y w h)
  (and (<= 0 x) (< x w)
       (<= 0 y) (< y h)))

(defun-inline draw-char% (array x y cw ch cx cy character)
  (when (in-bounds-p x y cw ch)
    (setf (aref array (+ cx x) (+ cy y))
          character)))


;;;; API ----------------------------------------------------------------------
(defun draw-char (canvas x y character)
  (multiple-value-bind (x y) (resolve-pos canvas x y)
    (draw-char% (text% *s*) x y
                (width% canvas) (height% canvas)
                (content-x% canvas) (content-y% canvas)
                character)))

(defun draw-string (canvas x y string)
  (multiple-value-bind (x y) (resolve-pos canvas x y)
    (loop
      :with array = (text% *s*)
      :with y% = y
      :with cw = (width% canvas)
      :with ch = (height% canvas)
      :with cx = (content-x% canvas)
      :with cy = (content-y% canvas)
      :for char :across string
      :for x% :from x
      :do (if (char= #\newline char)
            (setf x% (1- x)
                  y% (1+ y%))
            (draw-char% array x% y% cw ch cx cy char)))))
