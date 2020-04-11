(in-package :boots%)

;;;; Notes --------------------------------------------------------------------

(defparameter *border-top-left-char* #\+)
(defparameter *border-top-right-char* #\+)
(defparameter *border-bottom-left-char* #\+)
(defparameter *border-bottom-right-char* #\+)
(defparameter *border-vertical-char* #\|)
(defparameter *border-horizontal-char* #\-)

(defun required (name)
  (error "~A is required" name))

(defstruct pad
  (terminal (required 'terminal) :type boots/terminals:terminal)
  (w)
  (h)
  (x)
  (y))

(defgeneric redraw (widget pad))

(defmethod redraw :before ((widget widget) pad)
  (setf (pad-w pad) (width% widget)
        (pad-h pad) (height% widget)
        (pad-x pad) (content-x% widget)
        (pad-y pad) (content-y% widget))
  (clear-content pad)
  (draw-frame widget pad))

(defmethod redraw ((container container) pad)
  (dolist (widget (children container))
    (redraw widget pad)))

(defmethod redraw ((container pile) pad)
  ;; We need to draw back-to-front, but want to use a stack as a friendly
  ;; interface, so… welp.
  (dolist (widget (reverse (children container)))
    (redraw widget pad)))

(defmethod redraw ((canvas canvas) pad)
  (funcall (drawing-function canvas) pad))

(defun redraw-screen (screen)
  (check-type screen screen)
  (ensure-screen-resized screen)
  ;; todo cache the pad in the screen
  (redraw (root screen) (make-pad :terminal (terminal screen)))
  (boots/terminals:blit (terminal screen)))

(defun draw-char (pad x y thing &optional attr)
  (boots/terminals:put (pad-terminal pad) x y thing (or attr (default))))

(defun draw-frame (widget pad)
  (let* ((x (window-x% widget))
         (y (window-y% widget))
         (w (width% widget))
         (h (height% widget))
         (bt (border-top% widget))
         (br (border-right% widget))
         (bb (border-bottom% widget))
         (bl (border-left% widget))
         (pt (padding-top% widget))
         (pr (padding-right% widget))
         (pb (padding-bottom% widget))
         (pl (padding-left% widget))
         (tw (+ bl pl w pr br))
         (th (+ bt pt h pb bb))
         (cv *border-vertical-char*)
         (ch *border-horizontal-char*)
         (ctl *border-top-left-char*)
         (ctr *border-top-right-char*)
         (cbl *border-bottom-left-char*)
         (cbr *border-bottom-right-char*))
    (when (plusp bl)
      (loop :with x% = x
            :with end = (+ y th -1)
            :for y% :from y :to end
            :do (draw-char pad x% y%
                           (cond ((and (= y% y) (plusp bt)) ctl)
                                 ((and (= y% end) (plusp bb)) cbl)
                                 (t cv))))
      (decf tw)
      (incf x))
    (when (plusp br)
      (loop :with x% = (+ x tw -1)
            :with end = (+ y th -1)
            :for y% :from y :to end
            :do (draw-char pad x% y%
                           (cond ((and (= y% y) (plusp bt)) ctr)
                                 ((and (= y% end) (plusp bb)) cbr)
                                 (t cv))))
      (decf tw))
    (when (plusp bt)
      (loop :with y% = y
            :for x% :from x :below (+ x tw)
            :do (draw-char pad x% y% ch))
      (decf th)
      (incf y))
    (when (plusp bb)
      (loop :with y% = (+ y th -1)
            :for x% :from x :below (+ x tw)
            :do (draw-char pad x% y% ch))
      (decf th))
    (when (plusp pl)
      (loop :for x% :from x :repeat pl :do
            (loop :for y% :from y :below (+ y th)
                  :do (draw-char pad x% y% #\Space)))
      (decf tw pl)
      (incf x pl))
    (when (plusp pr)
      (loop :for x% :downfrom (+ x tw -1) :repeat pr :do
            (loop :for y% :from y :below (+ y th)
                  :do (draw-char pad x% y% #\Space)))
      (decf tw pr))
    (when (plusp pt)
      (loop :for y% :from y :repeat pt :do
            (loop :for x% :from x :below (+ x tw)
                  :do (draw-char pad x% y% #\Space)))
      (decf th pt)
      (incf y pt))
    (when (plusp pb)
      (loop :for y% :downfrom (+ y th -1) :repeat pb :do
            (loop :for x% :from x :below (+ x tw)
                  :do (draw-char pad x% y% #\Space)))
      (decf th pb))))


(defun clear-content (pad)
  (loop :for y :from (pad-y pad) :repeat (pad-h pad) :do
        (loop :for x :from (pad-x pad) :repeat (pad-w pad) :do
              (draw-char pad x y #\space))))

(defgeneric draw (pad x y thing &optional attr))

(defmethod draw (pad x y (thing character) &optional attr)
  (assert (char/= #\newline thing) (thing) "Cannot draw a newline into a cell.")
  (when (and (in-range-p 0 x (pad-w pad))
             (in-range-p 0 y (pad-h pad)))
    (draw-char pad (+ x (pad-x pad)) (+ y (pad-y pad)) thing attr)))

(defmethod draw (pad x y (thing string) &optional attr)
  ;; todo optimize this by inlining the guts of (draw ... char ...)
  (loop :with x% = x
        :with y% = y
        :for char :across thing
        :do (if (eql #\newline char)
              (setf x% x y% (1+ y%))
                (progn
                  (draw pad x% y% char attr)
                  (incf x%)))))
