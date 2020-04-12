(in-package :boots%)

;;;; Notes --------------------------------------------------------------------
;;; For efficiency, attributes are packed into a fixnum.  This allows fast
;;; comparison of all attributes at once, and avoids tons of consing (assuming
;;; we have at least 56 bit fixnums).  The bit structure looks like this:
;;;
;;;        6         5         4         3         2         1
;;;     3210987654321098765432109876543210987654321098765432109876543210
;;;     ........xBBBBBBBBBBBBBBBBBBBBBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFuib
;;;
;;; b = bold, i = italic, u = underline, F = fg color, B = bg color, x = invalid
;;;
;;; Colors are packed into 26 bits:
;;;
;;;          2         1
;;;     54321098765432109876543210
;;;     BBBBBbbbGGGGGgggRRRRRrrrmd
;;;
;;; d = default or explicit color (0 = default, 1 = explicit)
;;; m = color mode (0 = 256 color, 1 = truecolor)
;;; r+R = red   color bits (only 3 bits used for 256 color, all 8 for truecolor)
;;; g+G = green color bits (only 3 bits used for 256 color, all 8 for truecolor)
;;; b+B = blue  color bits (only 3 bits used for 256 color, all 8 for truecolor)


;;;; Types --------------------------------------------------------------------
(deftype attribute ()
  '(unsigned-byte 56))

(deftype color ()
  '(unsigned-byte 26))

(deftype truecolor ()
  '(unsigned-byte 8))

(deftype 256color ()
  '(unsigned-byte 3))


;;;; Readers ------------------------------------------------------------------
(defun-inline invalidp (attr) (logbitp 55 attr))
(defun-inline boldp (attr) (logbitp 0 attr))
(defun-inline italicp (attr) (logbitp 1 attr))
(defun-inline underlinep (attr) (logbitp 2 attr))

(defun-inline fg (attr) (ldb (byte 25 3) attr))
(defun-inline bg (attr) (ldb (byte 25 29) attr))

(defun-inline colorp (color) (logbitp 0 color))
(defun-inline truecolorp (color) (logbitp 1 color))
(defun-inline r (color) (ldb (byte 8 2) color))
(defun-inline g (color) (ldb (byte 8 10) color))
(defun-inline b (color) (ldb (byte 8 18) color))


;;;; Constructors -------------------------------------------------------------
(defun-inline rgb*% (r g b)
  (_ #b11
    (dpb r (byte 8 2) _)
    (dpb g (byte 8 10) _)
    (dpb b (byte 8 18) _)))

(defun rgb* (r g b)
  (check-type r truecolor)
  (check-type g truecolor)
  (check-type b truecolor)
  (rgb*% r g b))

(define-compiler-macro rgb* (&whole form r g b &environment env)
  (if (and (constantp r env)
           (constantp g env)
           (constantp b env))
    (rgb*% r g b)
    form))


(defun-inline rgb% (r g b)
  (_ #b01
    (dpb r (byte 8 2) _)
    (dpb g (byte 8 10) _)
    (dpb b (byte 8 18) _)))

(defun rgb (r g b)
  (check-type r 256color)
  (check-type g 256color)
  (check-type b 256color)
  (rgb% r g b))

(define-compiler-macro rgb (&whole form r g b &environment env)
  (if (and (constantp r env)
           (constantp g env)
           (constantp b env))
    (rgb% r g b)
    form))


(defun-inline default ()
  0)

(defun invalid-attribute ()
  (dpb 1 (byte 1 55) 0))


(defun-inline attr% (bold italic underline fg bg)
  (logior (dpb (if bold 1 0) (byte 1 0) 0)
          (dpb (if italic 1 0) (byte 1 1) 0)
          (dpb (if underline 1 0) (byte 1 2) 0)
          (dpb (or fg 0) (byte 26 3) 0)
          (dpb (or bg 0) (byte 26 29) 0)))

(defun attr (&key bold italic underline fg bg)
  (check-type fg (or null color))
  (check-type bg (or null color))
  (attr% bold italic underline fg bg))

(define-compiler-macro attr
    (&whole form &key bold italic underline fg bg &environment env)
  (if (and (constantp bold env)
           (constantp italic env)
           (constantp underline env)
           (constantp fg env)
           (constantp bg env))
    (attr% bold italic underline fg bg)
    form))


;;;; Destructuring ------------------------------------------------------------
(defmacro with-color ((has-color is-truecolor r g b) color &body body)
  "Bind the given symbols to the parts of `color` and evaluate `body`."
  (alexandria:once-only (color)
    `(let ((,has-color (colorp ,color))
           (,is-truecolor (truecolorp ,color))
           (,r (r ,color))
           (,g (g ,color))
           (,b (b ,color)))
       ,@body)))

(defmacro with-fg ((has-color is-truecolor r g b) attr &body body)
  `(with-color (,has-color ,is-truecolor ,r ,g ,b) (fg ,attr) ,@body))

(defmacro with-bg ((has-color is-truecolor r g b) attr &body body)
  `(with-color (,has-color ,is-truecolor ,r ,g ,b) (bg ,attr) ,@body))


;;;; Printing/Debugging -------------------------------------------------------
(defun pprint-color (color &optional (stream *standard-output*))
  (with-color (has-color is-truecolor r g b) color
    (if has-color
      (if is-truecolor
        (format stream "truecolor: (r ~3D) (g ~3D) (b ~3D)~%" r g b)
        (progn
          (check-type r 256color)
          (check-type g 256color)
          (check-type b 256color)
          (format stream "256 color: (r ~3D) (g ~3D) (b ~3D)~%" r g b)))
      (progn
        (assert (null is-truecolor))
        (assert (zerop r))
        (assert (zerop g))
        (assert (zerop b))
        (write-line "default" stream)))))

(defun pretty-bits (n)
  (check-type n (unsigned-byte 64))
  (subseq (format nil "~,'0,' ,8:B" (logior n (expt 2 64))) 2))

(defun pprint-attr (attr &optional (stream *standard-output*))
  (check-type attr attribute)
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


