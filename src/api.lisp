(in-package :boots)

(defparameter *screen* nil)
(defparameter *terminal* nil)


(boots%:defun-inline ensure-initialized ()
  (assert *screen* () "Boots must be initialized using ~A before redrawing."
    'with-boots))

(defmacro with-boots (terminal &body body)
  (alexandria:once-only (terminal)
    `(let* ((*terminal* ,terminal)
            (*screen* (boots%:make-screen ,terminal)))
       (boots/terminals:start ,terminal)
       (unwind-protect (progn ,@body)
         (boots/terminals:stop ,terminal)))))


(defun read-event ()
  (ensure-initialized)
  (boots/terminals:read-event *terminal*))

(defun read-event-no-hang ()
  (ensure-initialized)
  (boots/terminals:read-event-no-hang *terminal*))

(defun redraw ()
  (ensure-initialized)
  (boots%:redraw-screen *screen*))


(defun width (pad)
  (boots%:pad-w pad))

(defun height (pad)
  (boots%:pad-h pad))


(defmacro define-widget-macro (name constructor extra-arglist &rest widget-specific-args)
  `(defmacro ,name ((&rest args &key
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
                           (border-left border))
                    ,@extra-arglist)
     (declare (ignore
                width height
                margin-top margin-right margin-bottom margin-left
                padding-top padding-right padding-bottom padding-left
                border-top border-right border-bottom border-left))
     `(,',constructor ,@args ,,@widget-specific-args)))

(define-widget-macro stack make-stack (&rest children) :children `(list ,@children))
(define-widget-macro shelf make-shelf (&rest children) :children `(list ,@children))
(define-widget-macro pile make-pile (&rest children) :children `(list ,@children))
(define-widget-macro canvas make-canvas ((pad-argument) &body body)
  :draw `(lambda (,pad-argument) ,@body))

