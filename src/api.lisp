(in-package :boots)

(defmacro with-screen ((symbol terminal &key root) &body body)
  (setf symbol (or symbol '*screen*))
  `(let* ((,symbol (boots%:make-screen ,terminal :root ,root))
          ,@(unless (eql symbol '*screen*)
              `((*screen* ,symbol))))
     ,@body))


(defun redraw (&key (screen *screen*) (full nil))
  (boots%:require-type screen boots%:screen)
  (boots%:redraw-screen screen (if full :full :default)))

(defun width (pad)
  (boots%:pad-w pad))

(defun height (pad)
  (boots%:pad-h pad))

(defmacro with-simple-borders (&body body)
  `(let ((*border-top-left-char* #\+)
         (*border-top-right-char* #\+)
         (*border-bottom-left-char* #\+)
         (*border-bottom-right-char* #\+)
         (*border-vertical-char* #\|)
         (*border-horizontal-char* #\-))
     ,@body))

(defmacro with-light-borders (&body body)
  `(let ((*border-top-left-char*     (char "┌" 0))
         (*border-top-right-char*    (char "┐" 0))
         (*border-bottom-left-char*  (char "└" 0))
         (*border-bottom-right-char* (char "┘" 0))
         (*border-vertical-char*     (char "│" 0))
         (*border-horizontal-char*   (char "─" 0)))
     ,@body))

(defmacro with-heavy-borders (&body body)
  `(let ((*border-top-left-char*     (char "┏" 0))
         (*border-top-right-char*    (char "┓" 0))
         (*border-bottom-left-char*  (char "┗" 0))
         (*border-bottom-right-char* (char "┛" 0))
         (*border-vertical-char*     (char "┃" 0))
         (*border-horizontal-char*   (char "━" 0)))
     ,@body))

(defun wait (seconds &key (screen *screen*))
  (boots%:require-type screen boots%:screen)
  (boots%:require-type seconds (real 0 *))
  (loop :with remaining = seconds
        :for amount = (min 1/60 remaining)
        :do (progn (sleep amount)
                   (boots%:redraw-screen screen :minimal)
                   (decf remaining amount))
        :while (plusp amount)))


;;;; Events -------------------------------------------------------------------
(defun read-event (&optional (screen *screen*))
  (boots%:require-type screen boots%:screen)
  (boots/terminals:read-event (boots%::terminal screen)))

(defun read-event-no-hang (&optional (screen *screen*))
  (boots%:require-type screen boots%:screen)
  (boots/terminals:read-event-no-hang (boots%::terminal screen)))

(defun shiftp (modifiers) (boots%:shiftp modifiers))
(defun ctrlp (modifiers) (boots%:ctrlp modifiers))
(defun altp (modifiers) (boots%:altp modifiers))
(defun vanillap (modifiers) (boots%:vanillap modifiers))

(defmacro event-case (event-values &body clauses)
  "Dispatch on `event-values`.

  Each clause is of the form `(keys body…)`.

  `keys` is a list of possible keys for this clause.  If only one key is needed,
  it can be used on its own without wrapping it in a list.

  Each `key` in `keys` is of the form `#(key modifiers…)`.  If no modifiers are
  needed, `key` can be used on its own without wrapping it in a vector.  The
  order of `key` and the `modifiers` does not matter.

    (event-case (boots:read-event-no-hang)
      (#\x                   (foo \"got x\"))
      (#\X                   (foo \"got X\"))
      (#\newline             (foo \"got return\"))
      (#(:ctrl #\x)          (foo \"got ctrl-x\"))
      ((#\a #\e #\i #\o #\u) (foo \"got a vowel\"))
      ((#(#\s :ctrl) #\s)    (foo \"got ctrl-s or S\"))
      (nil                   (foo \"no event\"))
      (t                     (error \"bad key\")))

  TODO: more docs

  "
  `(boots%:event-case ,event-values ,@clauses))
