(in-package :boots%)

(defstruct pad
  (terminal (error "terminal is required") :type boots/terminals:terminal)
  (w 0 :type size)
  (h 0 :type size)
  (x 0 :type coord)
  (y 0 :type coord))

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
  ;; Don't bother drawing at all if the pad is too small to be seen.
  ;; This will help users avoid edge cases too, for example: something like
  ;; (wrap-text foo (width pad)) might break if the pad were 0 characters wide.
  (unless (or (zerop (pad-w pad))
              (zerop (pad-h pad)))
    (funcall (drawing-function canvas) pad)))

(defun redraw-screen (screen full)
  (require-type screen screen)
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
    (when bl?
      (boots/terminals:paint term left top 1 th *border-vertical-char*)
      (when bt? (boots/terminals:put term left top    *border-top-left-char*))
      (when bb? (boots/terminals:put term left bottom *border-bottom-left-char*))
      (decf tw)
      (incf left))
    (when br?
      (boots/terminals:paint term right top 1 th *border-vertical-char*)
      (when bt? (boots/terminals:put term right top    *border-top-right-char*))
      (when bb? (boots/terminals:put term right bottom *border-bottom-right-char*))
      (decf tw)
      (decf right))
    (when bt?
      (boots/terminals:paint term left top tw 1 *border-horizontal-char*)
      (decf th)
      (incf top))
    (when bb?
      (boots/terminals:paint term left bottom tw 1 *border-horizontal-char*)
      (decf th)
      (decf bottom))
    ;; TODO remove this check when we add backgrounds/transparency
    (when (typep widget 'canvas)
      (boots/terminals:paint term left top tw th #\space))))


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
  (require-types fixnum x y)
  (require-type pad pad)
  ;; This could be a generic function, but it's called a *lot* (possibly
  ;; multiple times for every cell in every widget) so let's make this
  ;; concession for performance.
  (etypecase thing
    (character
      (case thing
        (#\newline (error "Cannot draw a newline into a cell."))
        (#\nul nil) ; todo transparency
        (t (draw-character% pad x y thing attr))))
    (string (draw-string% pad x y thing attr))
    (list (draw-list% pad x y thing attr)))
  nil)

(defun paint (pad character &key
              (x 0) (y 0) (width (pad-w pad)) (height (pad-h pad)) attr)
  (require-types fixnum x y)
  (require-type pad pad)
  (require-type character character)
  (when (and (in-range-p 0 x (pad-w pad))
             (in-range-p 0 y (pad-h pad)))
    (boots/terminals:paint (pad-terminal pad)
                           (+ x (pad-x pad))
                           (+ y (pad-y pad))
                           (max (pad-w pad) width)
                           (max (pad-h pad) height)
                           character
                           attr)))
