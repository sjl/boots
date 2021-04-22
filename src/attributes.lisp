(in-package :boots%)

;;;; Notes --------------------------------------------------------------------
;;; For efficiency, attributes are packed into a fixnum.  This allows fast
;;; comparison of all attributes at once, and avoids tons of consing (assuming
;;; we have at least 54 bit fixnums).  The bit structure looks like this:
;;;
;;;        6         5         4         3         2         1             bit
;;;     3210987654321098765432109876543210987654321098765432109876543210   index
;;;     ..........xBBBBBBBBBBBBBBBBBBBBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFuib
;;;
;;; b = bold, i = italic, u = underline, F = fg color, B = bg color, x = invalid
;;;
;;; Colors are packed into 25 bits:
;;;
;;;         2         1
;;;     4321098765432109876543210
;;;     Dbbbbbbbbggggggggrrrrrrrr
;;;
;;; D = default or explicit color (0 = default, 1 = explicit)
;;; r = red   color bits
;;; g = green color bits
;;; b = blue  color bits
;;;
;;; TODO: is it worth trying to align color channels on byte boundaries, like
;;; this?
;;;
;;;     .......?bbbbbbbbggggggggrrrrrrrr...biux?bbbbbbbbggggggggrrrrrrrr
;;;     .......BBBBBBBBBBBBBBBBBBBBBBBBB...biuxFFFFFFFFFFFFFFFFFFFFFFFFF
;;;            7       6       5       4       3       2       1       0   byte


;;;; Types --------------------------------------------------------------------
(deftype attribute ()
  '(unsigned-byte 54))

(deftype color ()
  '(unsigned-byte 25))

(deftype channel ()
  '(unsigned-byte 8))


;;;; Readers ------------------------------------------------------------------
(defun-inline invalidp (attr) (logbitp 53 attr))
(defun-inline boldp (attr) (logbitp 0 attr))
(defun-inline italicp (attr) (logbitp 1 attr))
(defun-inline underlinep (attr) (logbitp 2 attr))

(defun-inline fg (attr) (ldb (byte 25 3) attr))
(defun-inline bg (attr) (ldb (byte 25 28) attr))

(defun-inline r (color) (ldb (byte 8 0) color))
(defun-inline g (color) (ldb (byte 8 8) color))
(defun-inline b (color) (ldb (byte 8 16) color))
(defun-inline colorp (color) (logbitp 24 color))


;;;; Constructors -------------------------------------------------------------
(defun-inline rgb (r g b)
  (require-types channel r g b)
  (_ (dpb 1 (byte 1 24) 0)
    (dpb r (byte 8 0) _)
    (dpb g (byte 8 8) _)
    (dpb b (byte 8 16) _)))

(defun-inline default ()
  0)

(defun-inline invalid-attribute ()
  (dpb 1 (byte 1 53) 0))


(defun-inline attr (&key bold italic underline fg bg)
  (require-types (or null color) fg bg)
  (logior (dpb (if bold 1 0) (byte 1 0) 0)
          (dpb (if italic 1 0) (byte 1 1) 0)
          (dpb (if underline 1 0) (byte 1 2) 0)
          (dpb (or fg 0) (byte 25 3) 0)
          (dpb (or bg 0) (byte 25 28) 0)))


;;;; Destructuring ------------------------------------------------------------
(defmacro with-color ((has-color r g b) color &body body)
  "Bind the given symbols to the parts of `color` and evaluate `body`."
  (alexandria:once-only (color)
    `(let ((,has-color (colorp ,color))
           (,r (r ,color))
           (,g (g ,color))
           (,b (b ,color)))
       (declare (type channel ,r ,g ,b)
                (type boolean ,has-color))
       ,@body)))

(defmacro with-fg ((has-color r g b) attr &body body)
  `(with-color (,has-color ,r ,g ,b) (fg ,attr) ,@body))

(defmacro with-bg ((has-color r g b) attr &body body)
  `(with-color (,has-color ,r ,g ,b) (bg ,attr) ,@body))


;;;; Printing/Debugging -------------------------------------------------------
(defun pprint-color (color &optional (stream *standard-output*))
  (with-color (has-color r g b) color
    (if has-color
      (format stream "(r ~3D) (g ~3D) (b ~3D)~%" r g b)
      (progn (assert (= 0 r g b))
             (format stream "default")))))

(defun pretty-bits (n)
  (require-type n (unsigned-byte 64))
  (subseq (format nil "~,'0,' ,8:B" (logior n (expt 2 64))) 2))

(defun pprint-attr (attr &optional (stream *standard-output*))
  (require-type attr attribute)
  (format stream "Attribute ~D~%" attr)
  (format stream "       bits: ~A~%" (pretty-bits attr))
  (format stream "       bold: ~A~%" (boldp attr))
  (format stream "     italic: ~A~%" (italicp attr))
  (format stream "  underline: ~A~%" (underlinep attr))
  (format stream "    invalid: ~A~%" (invalidp attr))
  (write-string  "         FG: " stream)
  (pprint-color (fg attr) stream)
  (write-string  "         BG: " stream)
  (pprint-color (bg attr) stream)
  (values))

(defun print-attr (attr &optional (stream *standard-output*))
  (require-type attr attribute)
  (format stream "#<ATTR~A~A~A [fg ~A] [bg ~A]>"
          (if (boldp attr) " bold" "")
          (if (italicp attr) " italic" "")
          (if (underlinep attr) " underline" "")
          (pprint-color (fg attr) nil)
          (pprint-color (bg attr) nil)))

