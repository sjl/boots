(in-package :boots)

(declaim (optimize (debug 2) (safety 3)))


;;;; Utils --------------------------------------------------------------------
(defmacro-driver (FOR var FINDING value IN list
                  &optional KEY (key '#'identity) TEST (test '#'eql))
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (v l key% test%)
      `(progn
         (with ,v = ,value)
         (with ,key% = ,key)
         (with ,test% = ,test)
         (initially (setf ,l (cons nil ,list)))
         (generate ,l :next (member ,v (cdr ,l) :key ,key% :test ,test%))
         (,kwd ,var next (progn (next ,l)
                                (if ,l
                                  (car ,l)
                                  (terminate))))))))

(defmacro-driver (FOR var SATISFYING predicate IN list
                  &optional KEY (key '#'identity))
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (l pred key%)
      `(progn
         (with ,pred = ,predicate)
         (with ,key% = ,key)
         (initially (setf ,l (cons nil ,list)))
         (generate ,l :next (member-if ,pred (cdr ,l) :key ,key%))
         (,kwd ,var next (progn (next ,l)
                                (if ,l
                                  (car ,l)
                                  (terminate))))))))

;;;; Types --------------------------------------------------------------------
(deftype dimension-designator ()
  '(or (eql :auto)
       (and fixnum (integer 0))
       (float 0.0 1.0)))

(deftype dimension ()
  '(and fixnum (integer 0)))

(deftype coordinate ()
  '(and fixnum (integer 0)))

(deftype function-designator ()
  '(or function symbol))


;;;; Data Model ---------------------------------------------------------------
(defclass* (base :conc-name "") ()
  ((desired-height :type dimension-designator)
   (desired-width :type dimension-designator)
   (actual-height :type dimension)
   (actual-width :type dimension)
   (row :type coordinate)
   (col :type coordinate)))

(defclass* (container :conc-name "") (base)
  ((contents :type sequence)))

(defclass* (stack :conc-name "") (container) ())
(defclass* (shelf :conc-name "") (container) ())

(defclass* (widget :conc-name "") (base) ())
(defclass* (canvas :conc-name "") (widget)
  ((fill-char :type character)))


;;;; Constructors -------------------------------------------------------------
(defun make-stack (width height &rest contents)
  (make-instance 'stack
    :desired-height height
    :desired-width width
    :contents contents))

(defun make-shelf (width height &rest contents)
  (make-instance 'shelf
    :desired-height height
    :desired-width width
    :contents contents))

(defun make-canvas (width height fill-char)
  (make-instance 'canvas
    :desired-height height
    :desired-width width
    :fill-char fill-char))

(defmacro stack ((width height) &rest contents)
  `(make-stack ,width ,height ,@contents))

(defmacro shelf ((width height) &rest contents)
  `(make-shelf ,width ,height ,@contents))

(defmacro canvas ((width height) fill-char)
  `(make-canvas ,width ,height ,fill-char))


;;;; Sizing -------------------------------------------------------------------
(defgeneric resize (object row col width height))

(defmethod resize ((object base) row col width height)
  (setf (row object) row
        (col object) col
        (actual-width object) width
        (actual-height object) height))

(defun normalize-float (total pair)
  (let ((v (car pair)))
    (when (floatp v)
      (setf (car pair) (round (* v total))))))

(defun normalize-floats (total pairs)
  (map nil (curry #'normalize-float total) pairs))

(defun sum-fixed (pairs)
  (iterate (for (v . nil) :in pairs)
           (unless (eq :auto v)
             (summing v))))

(defun count-autos (pairs)
  (count-if (lambda (c) (eq :auto (car c)))
            pairs))


(defun distribute-autos (total pairs)
  (let ((n (count-autos pairs))
        (remaining (- total (sum-fixed pairs))))
    (when (plusp n)
      (multiple-value-bind (v extra) (floor remaining n)
        (iterate
          (for i :from 0)
          (for c :finding :auto :in pairs :key #'car)
          (setf (car c) v)
          (when (< i extra)
            (incf (car c))))))))

(defun flush-stack (pairs row col width)
  (iterate
    (for r :first row :then (+ r h))
    (for (h . child) :in pairs)
    (resize child
            r col
            width h)))

(defun flush-shelf (pairs row col height)
  (iterate
    (for c :first col :then (+ c w))
    (for (w . child) :in pairs)
    (resize child
            row c
            w height)))


(defmethod resize ((stack stack) row col width height)
  (call-next-method)
  (let ((pairs (map 'list (lambda (child)
                            (cons (desired-height child) child))
                    (contents stack))))
    (normalize-floats height pairs)
    (distribute-autos height pairs)
    (flush-stack pairs row col width)))

(defmethod resize ((shelf shelf) row col width height)
  (call-next-method)
  (let ((pairs (map 'list (lambda (child)
                            (cons (desired-width child) child))
                    (contents shelf))))
    (normalize-floats width pairs)
    (distribute-autos width pairs)
    (flush-shelf pairs row col height)))


;;;; Drawing ------------------------------------------------------------------
(defparameter *out* nil)

(defgeneric draw (object))

(defmethod draw ((container container))
  (map nil #'draw (contents container)))

(defmethod draw ((canvas canvas))
  (iterate
    (for row :from (row canvas))
    (repeat (actual-height canvas))
    (replace (aref *out* row)
             (make-string (actual-width canvas)
                          :initial-element (fill-char canvas))
             :start1 (col canvas))))

(defun clear (width height)
  (setf *out* (make-array height))
  (dotimes (row height)
    (setf (aref *out* row) (make-string width :initial-element #\.))))

(defun blit ()
  (iterate (for s :in-vector *out*)
           (princ s)
           (terpri))
  (values))


;;;; Scratch ------------------------------------------------------------------
;; (defparameter *root*
;;   (stack (1.0 1.0)
;;     (canvas (1.0 1) #\H)
;;     (shelf (1.0 :auto)
;;       (canvas (:auto 1.0) #\@)
;;       (canvas (15 1.0) #\S))
;;     (canvas (1.0 6) #\L)))

(defparameter *root*
  (stack (1.0 1.0)
    (canvas (1.0 4) #\x)
    (shelf (1.0 :auto)
      (canvas (2 1.0) #\A)
      (canvas (:auto 1.0) #\B)
      (canvas (:auto 1.0) #\C)
      (canvas (0.1 1.0) #\D))))

(let ((w 30)
      (h 18))
  (clear w h)
  (resize *root* 0 0 w h)
  (draw *root*)
  (blit))
