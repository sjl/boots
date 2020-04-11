(in-package :boots%)

;;;; Utils --------------------------------------------------------------------
(defun slots->list (objects slots)
  (loop :for object :in objects
        :appending (loop :for slot :in slots
                         :collect (slot-value object slot))))

(defun list->slots (objects slots values)
  (loop
    :with n = (length slots)
    :for object :in objects
    :for vals :on values :by (alexandria:curry #'nthcdr n)
    :do (loop :for slot :in slots
              :for value :in vals
              :do (setf (slot-value object slot) value))))


(defmacro do-list ((symbol list) &body body)
  "Like `cl:dolist`, but `symbol-macrolet`s `symbol` so it can be `setf`ed."
  (alexandria:with-gensyms (current)
    `(do ((,current ,list (cdr ,current)))
         ((null ,current))
       (symbol-macrolet ((,symbol (car ,current)))
         ,@body))))


(defun minimum-positive (list)
  (loop :with result = nil
        :for n :in list
        :when (and (plusp n)
                   (or (null result)
                       (< n result)))
        :do (setf result n)
        :finally (return result)))

(defun count-positive (list)
  (count-if #'plusp list))

(defun safe-sum (list)
  (loop :for n :in list :when (numberp n) :sum n))


(defun total-width (widget)
  (safe-sum (list (margin-left% widget)
                  (border-left% widget)
                  (padding-left% widget)
                  (width% widget)
                  (padding-right% widget)
                  (border-right% widget)
                  (margin-right% widget))))

(defun total-height (widget)
  (safe-sum (list (margin-top% widget)
                  (border-top% widget)
                  (padding-top% widget)
                  (height% widget)
                  (padding-bottom% widget)
                  (border-bottom% widget)
                  (margin-bottom% widget))))


(defun difference (widgets total axis)
  (- total (reduce #'+ widgets :key (ecase axis
                                      (:horizontal #'total-width)
                                      (:vertical #'total-height)))))


(alexandria:define-constant +vertical-slots+
  '(margin-top% border-top% padding-top% height% padding-bottom% border-bottom% margin-bottom%)
  :test #'equal)

(alexandria:define-constant +vertical-deficit-slot-groups+
  '((margin-top% padding-top% padding-bottom% margin-bottom%)
    (height%)
    (border-top% border-bottom%))
  :test #'equal)

(alexandria:define-constant +horizontal-slots+
  '(margin-left% border-left% padding-left% width% padding-right% border-right% margin-right%)
  :test #'equal)

(alexandria:define-constant +horizontal-deficit-slot-groups+
  '((margin-left% padding-left% padding-right% margin-right%)
    (width%)
    (border-left% border-right%))
  :test #'equal)


;;;; Desired ------------------------------------------------------------------
;;; The first step in resizing a widget is to compute what the widget WANTS its
;;; size to be.  For dimensions/margin/padding:
;;;
;;; * If the desired size is an integer, that's it.
;;; * If the desired size is a float, it is resolved to a percentage of the parent.
;;; * If the desired size is t (auto), it's left unchanged and will be resolved later.
;;;
;;; Desired border sizes are always 0 or 1.

(defun compute-desired (widget parent-width parent-height)
  (flet ((calc (designator total)
           (etypecase designator
             (integer designator)
             (float (truncate (* total designator)))
             ((eql t) t))))
    (setf
      (width% widget)          (calc (width widget) parent-width)
      (height% widget)         (calc (height widget) parent-height)

      (margin-top% widget)     (calc (margin-top widget) parent-height)
      (margin-right% widget)   (calc (margin-right widget) parent-width)
      (margin-bottom% widget)  (calc (margin-bottom widget) parent-height)
      (margin-left% widget)    (calc (margin-left widget) parent-width)

      (padding-top% widget)    (calc (padding-top widget) parent-height)
      (padding-right% widget)  (calc (padding-right widget) parent-width)
      (padding-bottom% widget) (calc (padding-bottom widget) parent-height)
      (padding-left% widget)   (calc (padding-left widget) parent-width)

      (border-top% widget)     (if (border-top widget) 1 0)
      (border-right% widget)   (if (border-right widget) 1 0)
      (border-bottom% widget)  (if (border-bottom widget) 1 0)
      (border-left% widget)    (if (border-left widget) 1 0)))
  (values))


;;;; Credit/Debit -------------------------------------------------------------
;;; To make my life easier, instead of trying to work with the widgets directly
;;; when adding/removing length to fit, I'm going to model the problem as one
;;; I've solved before: distributing credits/debits to bank accounts.
;;;
;;; The `credit` and `debit` functions take a list of account values and an
;;; amount, and destructively update the list to distribute the amount as evenly
;;; as possibly between the given accounts.
;;;
;;; `credit` will only ever distribute to `t` accounts, and will ensure that all
;;; `t` accounts are resolved to numbers (possibly 0's) by the time it is
;;; finished.
;;;
;;; `debit` will never bring accounts negative.  If the given accounts are not
;;; sufficient to cover the debt, the remaining amount will be returned.

(defun credit (accounts amount)
  "Destructively update `accounts` to distribute `amount` to all `t` accounts.

  `accounts` should be a list of numbers or `t`.

  `amount` will be distributed as evenly as possible to all `t` accounts.  After
  this process there will no longer be any `t` accounts in the list.

  "
  (let ((autos (count t accounts)))
    (when (plusp autos)
      (multiple-value-bind (each remainder) (truncate amount autos)
        (do-list (account accounts)
          (when (eql t account)
            (setf account
                  (if (zerop remainder)
                    each
                    (progn (decf remainder) (1+ each))))))))))


(defun debit-remainder (accounts remainder)
  "Distribute `remainder` between all remaining `accounts`.

  At most 1 will be taken from each positive account.

  "
  (do-list (account accounts)
    (when (plusp account)
      (decf account)
      (decf remainder)
      (when (zerop remainder)
        (return))))
  remainder)

(defun debit-quotient (accounts amount quotient)
  ;; In an ideal world, we would take the quotient from each positive account
  ;; and be done.  But some positive accounts may have less than the quotient in
  ;; them, and we don't want to take them negative.
  ;;
  ;; To solve this, in each round we take at most N from all positive accounts,
  ;; where N is the smallest positive account.  This prevents us from ever
  ;; taking an account negative, while still takes fairly evenly from among all
  ;; the accounts.
  (let ((cap (minimum-positive accounts)))
    (unless (null cap)
      (let ((each (min cap quotient)))
        (do-list (account accounts)
          (when (plusp account)
            (decf account each)
            (decf amount each))))))
  ;; Once we've distributed this round of the quotient, we recur to try again.
  ;; Eventually we'll bottom out on either distributing the remainder, or having
  ;; no remaining positive accounts.
  (debit accounts amount))

(defun debit (accounts amount)
  "Destructively distribute `amount` between `accounts` as evenly as possible.

  This may not distribute ALL of the amount, if there's not enough in the
  accounts to cover all of it.

  Returns the remaining deficit.

  "
  (if (zerop amount)
    0 ; done
    (let ((n (count-positive accounts)))
      (if (zerop n)
        amount ; no more accounts to pull from
        (multiple-value-bind (quotient remainder) (truncate amount n)
          (if (zerop quotient)
            (debit-remainder accounts remainder)
            (debit-quotient accounts amount quotient)))))))


;;;; Distribution -------------------------------------------------------------
;;; These functions adapt the bank-account-like functions to work with widgets
;;; by reading the appropriate slot values into a flat list, calling the
;;; function, then writing the (mutated) list values back into the slots.

(defmacro with-slots-as-list ((symbol objects slots) &body body)
  (alexandria:once-only (objects slots)
    `(let ((,symbol (slots->list ,objects ,slots)))
       (prog1 (progn ,@body)
         (list->slots ,objects ,slots ,symbol)))))

(defun distribute-excess (widgets slots excess)
  "Distribute `excess` between any auto `slots` of `widgets`."
  (with-slots-as-list (accounts widgets slots)
    (credit accounts excess))
  (values))

(defun distribute-deficit (widgets slots deficit)
  "Distribute `deficit` between `slots` of `widgets`."
  (if (plusp deficit)
    (with-slots-as-list (accounts widgets slots)
      (debit accounts deficit))
    0))


;;;; Growing ------------------------------------------------------------------
;;; The second step in resizing a widget is to distribute any excess to any of
;;; the t/auto dimensions in its children as evenly as possible.

(defun grow% (widgets total axis)
  (let ((slots (ecase axis
                 (:horizontal +horizontal-slots+)
                 (:vertical +vertical-slots+)))
        (excess (max 0 (difference widgets total axis))))
    (distribute-excess widgets slots excess))
  (values))

(defgeneric grow-children (widget))

(defmethod grow-children ((s stack))
  (grow% (children s) (height% s) :vertical)
  (loop :for child :in (children s)
        :do (grow% (list child) (width% s) :horizontal)))

(defmethod grow-children ((s shelf))
  (grow% (children s) (width% s) :horizontal)
  (loop
    :for child :in (children s)
    :do (grow% (list child) (height% s) :vertical)))

(defmethod grow-children ((p pile))
  (loop
    :with w = (width% p)
    :with h = (height% p)
    :for child :in (children p)
    :for children = (list child)
    :do (progn (grow% children w :horizontal)
               (grow% children h :vertical)))
  (values))

(defmethod grow-children ((s screen))
  (let ((children (list (root s))))
    (grow% children (width% s) :horizontal)
    (grow% children (height% s) :vertical))
  (values))


;;;; Shrinking ----------------------------------------------------------------
;;; After the auto slots have been resolved, the next step is to shrink the
;;; children to fit within the parent (if they don't already).  To do this we
;;; figure out how much we need to shrink, then distribute that deficit among
;;; the children as evenly as possible.
;;;
;;; However, there's a catch — if we DO need to take some space from the
;;; children, we prefer to take it from particular slots if at all possible:
;;;
;;; * The margin/padding slots are the first on the chopping block, because
;;;   they're generally less important than anything else.
;;; * The main dimension (height/width) is next.
;;; * The border is last, because if there IS a border it defines the shape of
;;;   the window which is helpful for debugging, and can only buy you 2
;;;   characters at most anyway.

(defun shrink% (widgets total axis)
  (let ((slot-groups (ecase axis
                       (:horizontal +horizontal-deficit-slot-groups+)
                       (:vertical +vertical-deficit-slot-groups+)))
        (deficit (- (difference widgets total axis))))
    (when (plusp deficit)
      (dolist (slots slot-groups)
        (when (zerop deficit)
          (return))
        (setf deficit (distribute-deficit widgets slots deficit)))))
  (values))

(defgeneric shrink-children (widget))

(defmethod shrink-children ((s stack))
  (shrink% (children s) (height% s) :vertical)
  (loop
    :for child :in (children s)
    :do (shrink% (list child) (width% s) :horizontal)))

(defmethod shrink-children ((s shelf))
  (shrink% (children s) (width% s) :horizontal)
  (loop
    :for child :in (children s)
    :do (shrink% (list child) (height% s) :vertical)))

(defmethod shrink-children ((p pile))
  (loop
    :with w = (width% p)
    :with h = (height% p)
    :for child :in (children p)
    :for children = (list child)
    :do (progn (shrink% children w :horizontal)
               (shrink% children h :vertical)))
  (values))

(defmethod shrink-children ((s screen))
  (let ((children (list (root s))))
    (shrink% children (width% s) :horizontal)
    (shrink% children (height% s) :vertical))
  (values))


;;;; Resizing -----------------------------------------------------------------
;;; Resizing an object resizes its children.  This assumes the object has
;;; already had its width and height set by an earlier step.

(defgeneric resize (widget))

(defmethod resize ((c canvas))
  nil)

(defmethod resize ((s screen))
  (compute-desired (root s) (width s) (height s))
  (grow-children s)
  (shrink-children s)
  (resize (root s)))

(defmethod resize ((c container))
  (loop :with w = (width% c)
        :with h = (height% c)
        :for child :in (children c)
        :do (compute-desired child w h))
  (grow-children c)
  (shrink-children c)
  (map nil #'resize (children c)))


;;;; Positioning --------------------------------------------------------------
;;; Once the sizes have been computed, determining the positions is almost trivial.
(defun reposition-widget (widget x y)
  (setf
    (window-x% widget) (+ x (margin-left% widget))
    (window-y% widget) (+ y (margin-top% widget))
    (content-x% widget) (+ (margin-left% widget)
                           (border-left% widget)
                           (padding-left% widget)
                           x)
    (content-y% widget) (+ (margin-top% widget)
                           (border-top% widget)
                           (padding-top% widget)
                           y)))

(defgeneric reposition (widget x y))

(defmethod reposition ((s screen) x y)
  (reposition (root s) x y)
  (values))

(defmethod reposition ((c canvas) x y)
  (reposition-widget c x y)
  (values))

(defmethod reposition ((p pile) x y)
  (reposition-widget p x y)
  (loop :for c :in (children p)
        :do (reposition c x y))
  (values))

(defmethod reposition ((s stack) x y)
  (reposition-widget s x y)
  (loop :with x = (content-x% s)
        :with y = (content-y% s)
        :for c :in (children s) :do
        (reposition c x y)
        (incf y (total-height c)))
  (values))

(defmethod reposition ((s shelf) x y)
  (reposition-widget s x y)
  (loop :with x = (content-x% s)
        :with y = (content-y% s)
        :for c :in (children s) :do
        (reposition c x y)
        (incf x (total-width c)))
  (values))


;;;; API ----------------------------------------------------------------------
(defun ensure-screen-resized (screen)
  "Ensure `screen` matches its terminal's dimensions.

  Child sizes/positions will be recomputed if necessary.

  "
  (let ((tw (boots/terminals:width (terminal screen)))
        (th (boots/terminals:height (terminal screen))))
    (unless (and (= tw (width screen))
                 (= th (height screen)))
      (setf (width screen) tw
            (height screen) th)
      (resize screen)
      (reposition screen 0 0))))
