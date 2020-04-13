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
  '(or (eql t) (integer 0 *)))

(deftype border-designator ()
  'boolean)

(deftype computed-border ()
  '(integer 0 1))

(deftype location ()
  '(integer 0 *))

(deftype function-designator ()
  '(or function symbol))


;;;; Widgets ------------------------------------------------------------------
(defclass* widget ()
  ;; Desired
  ((width  :type length-designator)
   (height :type length-designator)
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
   (window-x% :type location)
   (window-y% :type location)
   (content-x% :type location)
   (content-y% :type location)))

(defmethod print-object ((object widget) stream)
  (flet ((slot (slot)
           (if (slot-boundp object slot)
             (slot-value object slot)
             '?)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~Ax~A @~A,~A/~A,~A ~S ~S ~S"
              (slot 'width%)
              (slot 'height%)
              (slot 'window-x%)
              (slot 'window-y%)
              (slot 'content-x%)
              (slot 'content-y%)
              (list "m"
                    (slot 'margin-top%)
                    (slot 'margin-right%)
                    (slot 'margin-bottom%)
                    (slot 'margin-left%))
              (list "b"
                    (slot 'border-top%)
                    (slot 'border-right%)
                    (slot 'border-bottom%)
                    (slot 'border-left%))
              (list "p"
                    (slot 'padding-top%)
                    (slot 'padding-right%)
                    (slot 'padding-bottom%)
                    (slot 'padding-left%))))))

(defclass* container (widget)
  ((children :type list :initform nil)))

(defclass* stack (container) ())
(defclass* shelf (container) ())
(defclass* pile (container) ())

(defclass* canvas (widget)
  ((drawing-function :type function-designator :initarg :draw)))

(defclass* screen ()
  ((root :type widget)
   (terminal :type boots/terminals:terminal)
   ;; todo: make these more specific
   (width  :type (and fixnum (integer 0)))
   (height :type (and fixnum (integer 0)))))

(defmethod width% ((screen screen))
  (width screen))

(defmethod height% ((screen screen))
  (height screen))


;;;; Constructors -------------------------------------------------------------
(defun make-screen (terminal &key root)
  (let ((result (make-instance 'screen
                  :terminal terminal
                  :width 1 ; dummy fixnums, will get clobbered later
                  :height 1)))
    (when root
      (setf (root result) root))
    result))

(defmacro define-make-widget (name class &rest extra)
  `(defun ,name (&key
                 (width t) (height t)
                 (margin 0) (padding 0) (border nil)
                 (margin-top margin)
                 (margin-right margin)
                 (margin-bottom margin)
                 (margin-left margin)
                 (padding-top padding)
                 (padding-right padding)
                 (padding-bottom padding)
                 (padding-left padding)
                 (border-top border)
                 (border-right border)
                 (border-bottom border)
                 (border-left border)
                 ,@(mapcar #'car extra))
     (make-instance ',class
       :width width
       :height height
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
       :border-left border-left
       ,@(apply #'append (mapcar #'cdr extra)))))

(define-make-widget make-stack stack (children . (:children children)))
(define-make-widget make-shelf shelf (children . (:children children)))
(define-make-widget make-pile pile (children . (:children children)))
(define-make-widget make-canvas canvas ((draw 'fill-with-random-char) . (:draw draw)))
