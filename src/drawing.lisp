(in-package :boots%)

(defstruct pad
  (terminal (error "terminal is required") :type boots/terminals:terminal)
  (w 0 :type size)
  (h 0 :type size)
  (x 0 :type coord)
  (y 0 :type coord)
  (w% 0 :type size)
  (h% 0 :type size)
  (x% 0 :type coord)
  (y% 0 :type coord)
  (characters (make2d 0 0 'character #\nul) :type char-array)
  (attributes (make2d 0 0 'attribute (default)) :type attr-array))

(defgeneric redraw (widget pad))

(defmethod redraw :before ((widget widget) pad)
  (setf
    ;; content/inner w/h/x/y
    (pad-w pad) (width% widget)
    (pad-h pad) (height% widget)
    (pad-x pad) (content-x% widget)
    (pad-y pad) (content-y% widget)
    ;; window/outer w/h/x/y
    (pad-x% pad) (window-x% widget)
    (pad-y% pad) (window-y% widget)
    (pad-w% pad) (+ (border-left% widget)
                    (padding-left% widget)
                    (width% widget)
                    (padding-right% widget)
                    (border-right% widget))
    (pad-h% pad) (+ (border-top% widget)
                    (padding-top% widget)
                    (height% widget)
                    (padding-bottom% widget)
                    (border-bottom% widget)))
  (draw-frame widget pad))

(defmethod redraw ((container container) pad)
  (render-pad pad)
  (dolist (widget (children container))
    (redraw widget pad)))

(defmethod redraw ((container pile) pad)
  (render-pad pad)
  ;; We need to draw back-to-front, but want to use a stack as a friendly
  ;; interface, so… welp.
  (dolist (widget (reverse (children container)))
    (redraw widget pad)))

(defmethod redraw ((canvas canvas) pad)
  ;; Don't bother drawing the contents at all if the pad is too small to be
  ;; seen.  This will help users avoid edge cases too, e.g. something like
  ;; (wrap-text foo (width pad)) might break if the pad were 0 characters wide.
  (unless (or (zerop (pad-w pad))
              (zerop (pad-h pad)))
    (funcall (drawing-function canvas) pad))
  ;; We might still need to draw the frame though.
  (render-pad pad))

(defun ensure-pad-resized (pad)
  (let ((tw (boots/terminals:width (pad-terminal pad)))
        (th (boots/terminals:height (pad-terminal pad))))
    (destructuring-bind (h w) (array-dimensions (pad-characters pad))
      (unless (and (= tw w) (= th h))
        (setf (pad-characters pad) (make2d th tw 'character #\nul)
              (pad-attributes pad) (make2d th tw 'attribute (default)))))))

(defun redraw-screen (screen mode)
  (require-type screen screen)
  (when (not (boots/terminals:prep (terminal screen) mode))
    ;; If we don't actually need to redraw, bail early.
    (return-from redraw-screen))
  (ensure-screen-resized screen)
  (let ((pad (if (slot-boundp screen 'pad)
               (pad screen)
               (setf (pad screen) (make-pad :terminal (terminal screen))))))
    (ensure-pad-resized pad)
    (redraw (root screen) pad))
  (boots/terminals:blit (terminal screen)))

(defun paint% (pad x y width height character attr)
  (declare (optimize speed (safety 1) (debug 1))
           (type pad pad)
           (type character character)
           (type attribute attr)
           (type coord x y)
           (type size width height))
  (loop :with chars = (pad-characters pad)
        :with attrs = (pad-attributes pad)
        :for y% :from y :below (+ y height)
        :do (loop :for x% :from x :below (+ x width) :do
                  (setf (aref chars y% x%) character
                        (aref attrs y% x%) attr))))

(defun put% (pad x y character attr)
  (declare (optimize speed (safety 1) (debug 1))
           (type pad pad)
           (type character character)
           (type attribute attr)
           (type coord x y))
  (setf (aref (pad-characters pad) y x) character
        (aref (pad-attributes pad) y x) attr))

(defun draw-frame (widget pad)
  (let* ((bt (border-top% widget))      (bt? (plusp bt))
         (br (border-right% widget))    (br? (plusp br))
         (bb (border-bottom% widget))   (bb? (plusp bb))
         (bl (border-left% widget))     (bl? (plusp bl))
         (tw (pad-w% pad))
         (th (pad-h% pad))
         (fc (fill-char widget))
         (fa (fill-attr widget))
         (left (pad-x% pad))
         (right (+ left tw -1))
         (top (pad-y% pad))
         (bottom (+ top th -1)))
    (when bl?
      (paint% pad left top 1 th *border-vertical-char* (default))
      (when bt? (put% pad left top    *border-top-left-char*    (default)))
      (when bb? (put% pad left bottom *border-bottom-left-char* (default)))
      (decf tw)
      (incf left))
    (when br?
      (paint% pad right top 1 th *border-vertical-char* (default))
      (when bt? (put% pad right top    *border-top-right-char*    (default)))
      (when bb? (put% pad right bottom *border-bottom-right-char* (default)))
      (decf tw)
      (decf right))
    (when bt?
      (paint% pad left top tw 1 *border-horizontal-char* (default))
      (decf th)
      (incf top))
    (when bb?
      (paint% pad left bottom tw 1 *border-horizontal-char* (default))
      (decf th)
      (decf bottom))
    (when fc
      (paint% pad left top tw th fc fa))))

(defun render-pad (pad)
  (boots/terminals:draw-region (pad-terminal pad)
                               (pad-x% pad) (pad-y% pad)
                               (pad-w% pad) (pad-h% pad)
                               (pad-characters pad) (pad-attributes pad)))

(defun draw-character% (pad x y character attr)
  (declare (optimize speed)
           (type pad pad)
           (type fixnum x y)
           (type character character))
  (when (and (in-range-p 0 x (pad-w pad))
             (in-range-p 0 y (pad-h pad)))
    (let ((tx (+ x (pad-x pad)))
          (ty (+ y (pad-y pad))))
      (setf (aref (pad-characters pad) ty tx) character
            (aref (pad-attributes pad) ty tx) attr)))
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


;;;; API ----------------------------------------------------------------------
(defun draw (pad x y thing &optional (attr (default)))
  (require-types fixnum x y)
  (require-type attr attribute)
  (require-type pad pad)
  ;; This could be a generic function, but it's called a *lot* (possibly
  ;; multiple times for every cell in every widget) so let's make this
  ;; concession for performance.
  (etypecase thing
    (character
      (if (char= #\newline thing)
        (error "Cannot draw a newline into a cell.")
        (draw-character% pad x y thing attr)))
    (string (draw-string% pad x y thing attr))
    (null (draw-character% pad x y #\nul attr))
    (list (draw-list% pad x y thing attr)))
  nil)

(defun paint (pad character &key
              (x 0) (y 0)
              (width (pad-w pad)) (height (pad-h pad))
              (attr (boots:default)))
  (require-types fixnum x y width height)
  (require-type pad pad)
  (require-type character character)
  (require-type attr attribute)
  ; todo should we allow this and make it mean to reverse the direction of paint?
  (when (or (<= width 0) (<= height 0))
    (return-from paint))
  (let ((left   (alexandria:clamp x            0 (pad-w pad)))
        (right  (alexandria:clamp (+ x width)  0 (pad-w pad)))
        (top    (alexandria:clamp y            0 (pad-h pad)))
        (bottom (alexandria:clamp (+ y height) 0 (pad-h pad))))
    (paint% pad
            (+ (pad-x pad) left)
            (+ (pad-y pad) top)
            (- right left)
            (- bottom top)
            character attr)))
