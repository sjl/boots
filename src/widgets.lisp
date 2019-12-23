(in-package :boots)

;;;; Types --------------------------------------------------------------------
(deftype length-designator ()
  '(or (eql t) (float 0.0 1.0) (integer 0 *)))

(deftype computed-length ()
  '(or (eql t) (integer 0 *)))

(deftype border-designator ()
  '(member nil t))

(deftype computed-border ()
  '(integer 0 1))

(deftype location ()
  '(or null (integer 0 *)))

(deftype function-designator ()
  '(or function symbol))


;;;; Widgets ------------------------------------------------------------------
(defclass* screen ()
  ((width%)
   (height%)
   (root)

   (text%)
   (color%)
   (formatting%)

   (dirty%)

   ))

(defclass* widget ()
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

(defmethod print-object ((o widget) s)
  (flet ((slot (slot)
           (if (slot-boundp o slot)
             (slot-value o slot)
             '?)))
    (print-unreadable-object (o s :type t :identity t)
      (format s "~Ax~A ~S ~S ~S"
              (slot 'width%)
              (slot 'height%)
              (list 'm
                    (slot 'margin-top%)
                    (slot 'margin-right%)
                    (slot 'margin-bottom%)
                    (slot 'margin-left%))
              (list 'b
                    (slot 'border-top%)
                    (slot 'border-right%)
                    (slot 'border-bottom%)
                    (slot 'border-left%))
              (list 'p
                    (slot 'padding-top%)
                    (slot 'padding-right%)
                    (slot 'padding-bottom%)
                    (slot 'padding-left%))))))

(defclass* container (widget)
  ((children :type vector
             :initform (make-array 4
                         :fill-pointer 0
                         :adjustable t))))

(defclass* stack (container) ())
(defclass* shelf (container) ())
(defclass* pile (container) ())

(defclass* canvas (widget)
  ((draw% :type function-designator)))

(defmethod children ((object screen))
  (vector (root object)))


(defun make-widget (class &key
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
                    children
                    draw)
  (let ((w (make-instance class
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
             :border-left border-left)))
    (doseq (c children)
      (vector-push-extend c (children w)))
    w))

(defun make-canvas (&key
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
                    (draw #'fill-with-random-chars))
  (make-instance 'canvas
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
    :draw% draw))
