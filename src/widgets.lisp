(in-package :boots%)

;;;; Notes --------------------------------------------------------------------
;;; Widgets are laid out much like the box model in HTML:
;;;
;;;                                                -
;;;                                                |   margin
;;;                                                -
;;;            +---------------------------------+     border
;;;            |                                 | -
;;;            |                                 | |   padding
;;;            |                                 | -
;;;            |       .......................   | -
;;;            |       .......................   | |
;;;            |       ......content..........   | |   height
;;;            |       .......................   | |
;;;            |       .......................   | |
;;;            |       .......................   | -
;;;            |                                 |
;;;            +---------------------------------+     border
;;;                    |---------------------|         width
;;;             |-----|                       |-|      padding
;;; |---------|                                        margin
;;;
;;; For each measure (width, height, padding-(top|right|bottom|left), etc)
;;; widgets generally have two slots:
;;;
;;; * foo:  the DESIRED measure, as a designator (e.g. 100, 0.5, t).
;;; * foo%: the COMPUTED measure, always a nonnegative integer (if bound).


;;;; Types --------------------------------------------------------------------
(deftype length-designator ()
  '(or (eql t) (float 0.0 1.0) (integer 0 *)))

(deftype computed-length ()
  '(or (eql t) size))

(deftype border-designator ()
  'boolean)

(deftype computed-border ()
  '(integer 0 1))

(deftype function-designator ()
  '(or function symbol))


;;;; Widgets ------------------------------------------------------------------
(defclass* widget ()
  ;; Desired
  ((width  :type length-designator)
   (height :type length-designator)
   (fill-char :type (or character null))
   (fill-attr :type attribute)
   (margin-top    :type length-designator)
   (margin-right  :type length-designator)
   (margin-bottom :type length-designator)
   (margin-left   :type length-designator)
   (padding-top    :type length-designator)
   (padding-right  :type length-designator)
   (padding-bottom :type length-designator)
   (padding-left   :type length-designator)
   (border-top    :type border-designator)
   (border-right  :type border-designator)
   (border-bottom :type border-designator)
   (border-left   :type border-designator)
   ;; Computed
   (width%  :type computed-length)
   (height% :type computed-length)
   (margin-top%    :type computed-length)
   (margin-right%  :type computed-length)
   (margin-bottom% :type computed-length)
   (margin-left%   :type computed-length)
   (padding-top%    :type computed-length)
   (padding-right%  :type computed-length)
   (padding-bottom% :type computed-length)
   (padding-left%   :type computed-length)
   (border-top%    :type computed-border)
   (border-right%  :type computed-border)
   (border-bottom% :type computed-border)
   (border-left%   :type computed-border)
   (window-x% :type coord)
   (window-y% :type coord)
   (content-x% :type coord)
   (content-y% :type coord)))

(defmethod print-object ((object widget) stream)
  (flet ((slot (slot)
           (if (slot-boundp object slot)
             (slot-value object slot)
             '?)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~Ax~A @~A,~A/~A,~A ~S ~S ~S ~S"
              (slot 'width%) (slot 'height%)
              (slot 'window-x%) (slot 'window-y%)
              (slot 'content-x%) (slot 'content-y%)
              (list :m
                    (slot 'margin-top%)
                    (slot 'margin-right%)
                    (slot 'margin-bottom%)
                    (slot 'margin-left%))
              (list :b
                    (slot 'border-top%)
                    (slot 'border-right%)
                    (slot 'border-bottom%)
                    (slot 'border-left%))
              (list :p
                    (slot 'padding-top%)
                    (slot 'padding-right%)
                    (slot 'padding-bottom%)
                    (slot 'padding-left%))
              (list :f (slot 'fill-char))))))

(defclass* container (widget)
  ((children :type list :initform nil)))

(defclass* stack (container) ())
(defclass* shelf (container) ())
(defclass* pile (container) ())

(defclass* canvas (widget)
  ((drawing-function :type function-designator)))

(defclass* screen ()
  ((root :type widget :accessor root%)
   (terminal :type boots/terminals:terminal)
   (width :type size)
   (height :type size)
   (pad)))

(defun root (screen)
  (if (slot-boundp screen 'root)
    (root% screen)
    nil))

(defun (setf root) (new-value screen)
  (setf (root% screen) new-value))


(defmethod width% ((screen screen))
  (width screen))

(defmethod height% ((screen screen))
  (height screen))


;;;; Constructors -------------------------------------------------------------
(defun make-screen (terminal &key root)
  (let ((result (make-instance 'screen
                  :terminal terminal
                  :width 0 ; dummy fixnums, will get clobbered later
                  :height 0)))
    (when root
      (setf (root result) root))
    result))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *widget-args*
    '((width t) (height t)
      (fill-char nil) (fill-attr nil)
      (margin 0) (padding 0) (border nil)
      (margin-vertical margin)
      (margin-horizontal margin)
      (margin-top margin-vertical)
      (margin-bottom margin-vertical)
      (margin-left margin-horizontal)
      (margin-right margin-horizontal)
      (padding-vertical padding)
      (padding-horizontal padding)
      (padding-top padding-vertical)
      (padding-bottom padding-vertical)
      (padding-left padding-horizontal)
      (padding-right padding-horizontal)
      (border-vertical border)
      (border-horizontal border)
      (border-top border-horizontal)
      (border-bottom border-horizontal)
      (border-left border-vertical)
      (border-right border-vertical)))

  (defparameter *widget-make-instance-args*
    '(:width width
       :height height
       :fill-char fill-char
       :fill-attr fill-attr
       :margin-top margin-top
       :margin-right margin-right
       :margin-bottom margin-bottom
       :margin-left margin-left
       :padding-top padding-top
       :padding-right padding-right
       :padding-bottom padding-bottom
       :padding-left padding-left
       :border-top border-top
       :border-right border-right
       :border-bottom border-bottom
       :border-left border-left)))


(defmethod initialize-instance :before ((widget widget) &key fill-char fill-attr &allow-other-keys)
  (when (and fill-attr (not fill-char))
    (error "Widget~%    ~A~%has a FILL-ATTR ~S ~A but has no FILL-CHAR."
           widget fill-attr (print-attr fill-attr nil))))

(defun make-stack #.`(&key ,@*widget-args* children)
  #.`(make-instance 'stack ,@*widget-make-instance-args* :children children))

(defun make-shelf #.`(&key ,@*widget-args* children)
  #.`(make-instance 'shelf ,@*widget-make-instance-args* :children children))

(defun make-pile #.`(&key ,@*widget-args* children)
  #.`(make-instance 'pile ,@*widget-make-instance-args* :children children))

(defun make-canvas #.`(&key ,@*widget-args* (draw (constantly nil)))
  #.`(make-instance 'canvas ,@*widget-make-instance-args* :drawing-function draw))

(defmacro stack (#.`(&rest args &key ,@*widget-args*) &rest children)
  (declare #.`(ignorable ,@(mapcar #'car *widget-args*)))
  `(make-stack ,@args :children (list ,@children)))

(defmacro shelf (#.`(&rest args &key ,@*widget-args*) &rest children)
  (declare #.`(ignorable ,@(mapcar #'car *widget-args*)))
  `(make-shelf ,@args :children (list ,@children)))

(defmacro pile (#.`(&rest args &key ,@*widget-args*) &rest children)
  (declare #.`(ignorable ,@(mapcar #'car *widget-args*)))
  `(make-pile ,@args :children (list ,@children)))

(defmacro canvas (#.`(&rest args &key ,@*widget-args*) (pad-argument) &body body)
  (declare #.`(ignorable ,@(mapcar #'car *widget-args*)))
  `(make-canvas ,@args :draw (lambda (,pad-argument) ,@body)))
