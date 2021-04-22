(in-package :boots%)

(deftype normal-key ()
  'standard-char)

(deftype fancy-key ()
  '(member
     :backspace :delete
     :page-up :page-down :home :end
     :left :right :up :down
     :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15))

(deftype event ()
  '(or normal-key fancy-key))

(deftype modifiers ()
  '(unsigned-byte 3))

(defun-inline modifiers (shift ctrl alt)
  (logior (if shift #b001 0)
          (if ctrl  #b010 0)
          (if alt   #b100 0)))

(defun-inline vanilla ()
  (modifiers nil nil nil))

(defun vanillap (modifiers) (zerop modifiers))
(defun shiftp (modifiers) (logbitp 0 modifiers))
(defun ctrlp (modifiers) (logbitp 1 modifiers))
(defun altp (modifiers) (logbitp 2 modifiers))

(defun print-modifiers (modifiers &optional (stream *standard-output*))
  (format stream "~A"
          (with-output-to-string (s)
            (print-unreadable-object (modifiers s)
              (cond
                ((null modifiers) (princ nil s))
                ((vanillap modifiers) (princ "no modifiers" s))
                (t (format s "~{~A~^-~}" (append (when (shiftp modifiers) '("SHIFT"))
                                                 (when (ctrlp modifiers) '("CTRL"))
                                                 (when (altp modifiers) '("ALT"))))))))))


(defun modifier-designator-p (object)
  (member object '(:shift :ctrl :alt)))

(defun add-mod (mod mods)
  (concatenate 'vector mods (list mod)))

(defun parse-spec (spec)
  ;; Parse a spec into (key . modifiers-fixnum).
  (let* ((normalized-spec (etypecase spec
                            (vector spec)
                            ((or symbol character boolean) (vector spec))))
         (key (remove-if #'modifier-designator-p normalized-spec))
         (mods (remove-if-not #'modifier-designator-p normalized-spec)))
    (when (or (/= 1 (length key))
              (and (member (elt key 0) '(nil t otherwise))
                   (plusp (length mods))))
      (error "Malformed event spec ~S (key ~S modifiers ~S)." spec key mods))
    (setf key (elt key 0))
    (case key
      (#\tab (setf key #\i mods (add-mod :ctrl mods)))
      (#\newline (setf key #\m mods (add-mod :ctrl mods)))
      (#\escape (setf key #\[ mods (add-mod :ctrl mods))))
    (cons key (modifiers (find :shift mods)
                         (find :ctrl mods)
                         (find :alt mods)))))

(defun-inline event=% (key-spec mod-spec event mods)
  (case key-spec
    ((nil) (null event))
    ((t) t)
    (t (and (eql event key-spec)
            (eql mods mod-spec)))))

(defun-inline event= (event-spec event mods)
  (let ((spec (parse-spec event-spec)))
    (event=% (car spec) (cdr spec) event mods)))

(define-compiler-macro event= (&whole form spec event modifiers)
  (if (constantp spec)
      (let ((spec (parse-spec spec)))
        `(event=% ,(car spec) ,(cdr spec) ,event ,modifiers))
    form))


(defmacro event-case (event-values &body clauses)
  ;; TODO Bisect on modifiers to make this faster?
  (alexandria:with-gensyms (e m)
    (labels ((spec->check (spec)
               `(event= ,spec ,e ,m))
             (specs->check (specs)
               `(or ,@(mapcar #'spec->check (if (consp specs) specs (list specs))))))
      (let ((specs (mapcar #'car clauses))
            (bodies (mapcar #'cdr clauses)))
        `(multiple-value-bind (,e ,m) ,event-values
           (cond ,@(loop
                    :for specs :in specs
                    :for body :in bodies
                    :collect `(,(specs->check specs) ,@body))))))))



