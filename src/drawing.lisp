(in-package :boots%)

(defstruct pad
  (terminal (error "terminal is required") :type boots/terminals:terminal)
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defgeneric redraw (widget pad))

(defmethod redraw :before ((widget widget) pad)
  (setf (pad-w pad) (width% widget)
        (pad-h pad) (height% widget)
        (pad-x pad) (content-x% widget)
        (pad-y pad) (content-y% widget))
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

(defun redraw-screen (screen full)
  (check-type screen screen)
  (boots/terminals:prep (terminal screen) full)
  (ensure-screen-resized screen)
  ;; todo cache the pad in the screen
  (redraw (root screen) (make-pad :terminal (terminal screen)))
  (boots/terminals:blit (terminal screen)))

(defun draw-frame (widget pad)
  (let* ((bt (border-top% widget))      (bt? (plusp bt))
         (br (border-right% widget))    (br? (plusp br))
         (bb (border-bottom% widget))   (bb? (plusp bb))
         (bl (border-left% widget))     (bl? (plusp bl))
         (pt (padding-top% widget))
         (pr (padding-right% widget))
         (pb (padding-bottom% widget))
         (pl (padding-left% widget))
         (tw (+ bl pl (width% widget)  pr br))
         (th (+ bt pt (height% widget) pb bb))
         (left (window-x% widget))
         (right (+ left tw -1))
         (top (window-y% widget))
         (bottom (+ top th -1))
         (term (pad-terminal pad)))
    (when bl? (boots/terminals:paint term left  top    1  th *border-vertical-char*))
    (when br? (boots/terminals:paint term right top    1  th *border-vertical-char*))
    (when bt? (boots/terminals:paint term left  top    tw 1  *border-horizontal-char*))
    (when bb? (boots/terminals:paint term left  bottom tw 1  *border-horizontal-char*))
    (when (and bt? bl?) (boots/terminals:put term left  top    *border-top-left-char*))
    (when (and bt? br?) (boots/terminals:put term right top    *border-top-right-char*))
    (when (and bb? bl?) (boots/terminals:put term left  bottom *border-bottom-left-char*))
    (when (and bb? br?) (boots/terminals:put term right bottom *border-bottom-right-char*))
    (incf top bl)
    (incf left bt)
    (decf tw (+ bl br))
    (decf th (+ bt bb))
    (boots/terminals:paint term left top tw th #\space)))


(defun draw-character% (pad x y character attr)
  (declare (optimize speed)
           (type pad pad)
           (type fixnum x y)
           (type character character))
  (when (and (in-range-p 0 x (pad-w pad))
             (in-range-p 0 y (pad-h pad)))
    (boots/terminals:put (pad-terminal pad)
                         (+ x (pad-x pad))
                         (+ y (pad-y pad))
                         character attr))
  (values (1+ x) y))

(defun draw-string% (pad x y string attr)
  (declare (optimize speed)
           (type pad pad)
           (type fixnum x y)
           (type string string))
  (loop :with x% fixnum = x
        :with y% fixnum = y
        :for char :across string
        :do (if (eql #\newline char)
              (setf x% x y% (1+ y%))
              (progn
                (draw-character% pad x% y% char attr)
                (incf x%)))
        :finally (return (values x% y%))))

(defun draw-list% (pad x y list attr)
  (loop
    :with attr% = attr
    :with x% = x
    :with y% = y
    :for thing :in list
    :do (etypecase thing
          (string (setf (values x% y%) (draw-string% pad x% y% thing attr%)))
          (character (setf (values x% y%) (draw-character% pad x% y% thing attr%)))
          (attribute (setf attr% thing))
          (null (setf attr% attr)))))


(defun draw (pad x y thing &optional attr)
  (check-types fixnum x y)
  (check-type pad pad)
  ;; This could be a generic function, but it's called a *lot* (possibly
  ;; multiple times for every cell in every widget) so let's make this
  ;; concession for performance.
  (etypecase thing
    (character
      (assert (char/= #\newline thing) () "Cannot draw a newline into a cell.")
      (draw-character% pad x y thing attr))
    (string (draw-string% pad x y thing attr))
    (list (draw-list% pad x y thing attr)))
  nil)

(defun paint (pad character &key
              (x 0) (y 0) (width (pad-w pad)) (height (pad-h pad)) attr)
  (check-types fixnum x y)
  (check-type pad pad)
  (when (and (in-range-p 0 x (pad-w pad))
             (in-range-p 0 y (pad-h pad)))
    (boots/terminals:paint (pad-terminal pad)
                           (+ x (pad-x pad))
                           (+ y (pad-y pad))
                           (max (pad-w pad) width)
                           (max (pad-h pad) height)
                           character
                           attr)))
