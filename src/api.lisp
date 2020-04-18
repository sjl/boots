(in-package :boots)

(defmacro with-screen ((symbol terminal &key root) &body body)
  `(let* ((,symbol (boots%:make-screen ,terminal :root ,root))
          ,@(unless (eql symbol '*screen*)
              `((*screen* ,symbol))))
     ,@body))

(defun read-event (&optional (screen *screen*))
  (boots%:require-type screen boots%:screen)
  (boots/terminals:read-event (boots%::terminal screen)))

(defun read-event-no-hang (&optional (screen *screen*))
  (boots%:require-type screen boots%:screen)
  (boots/terminals:read-event-no-hang (boots%::terminal screen)))

(defun redraw (&key (screen *screen*) (full nil))
  (boots%:require-type screen boots%:screen)
  (boots%:redraw-screen screen full))

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
