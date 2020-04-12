(in-package :boots/terminals/ansi)

(defparameter *debug* nil)


;;;; Plumbing ------------------------------------------------------------------
(define-modify-macro logandf (&rest args) logand)

(defun e (stream string)
  (write-char #\Escape stream)
  (write-string string stream))

(defun save-terminal (stream)
  (e stream "[?1049h"))

(defun restore-terminal (stream)
  (e stream "[?1049l"))

(defun clear-terminal (stream)
  (e stream "[2J"))

(defun fd (stream)
  ;; todo this isn't right, but it's good enough for the moment
  (cond
    ((eql stream *standard-input*) +STDIN+)
    ((eql stream *standard-output*) +STDOUT+)
    ((eql stream *error-output*) +STDERR+)
    (t
     #+sbcl (sb-posix:file-descriptor stream)
     #-(or sbcl) (error "Unsupported implementation."))))

(cffi:defcvar "errno" :int)
(cffi:defcfun "strerror" :string
  (errno :int))

(cffi:defcfun "ioctl" :int
  (file-descriptor :int)
  (request :unsigned-long)
  &rest)

(cffi:defcfun "tcgetattr" :int
  (file-descriptor :int)
  (termios :pointer))

(cffi:defcfun "tcsetattr" :int
  (file-descriptor :int)
  (optional-actions :int)
  (termios :pointer))

(defun check% (value form acceptable-errors)
  (assert (or (/= -1 value)
              (member *errno* acceptable-errors))
      ()
    "Error in ~S: ~A" form (strerror *errno*))
  value)

(defmacro check (form &key acceptable-errors)
  `(check% ,form ',form ,acceptable-errors))

(defun get-terminal-size (terminal)
  (cffi:with-foreign-object (ws '(:struct winsize))
    (check (ioctl (fd (output terminal)) +TIOCGWINSZ+ :pointer ws))
    (let ((result (cffi:convert-from-foreign ws '(:struct winsize))))
      (values (getf result 'ws-col)
              (getf result 'ws-row)))))

(defun save-termios (terminal)
  (setf (original-termios terminal) (cffi:foreign-alloc '(:struct termios)))
  (check (tcgetattr (fd (input terminal)) (original-termios terminal))))

(defun enable-raw (terminal)
  (save-termios terminal)
  (let ((fd (fd (input terminal))))
    (cffi:with-foreign-object (new-termios '(:struct termios))
      (check (tcgetattr fd new-termios))
      (logandf (cffi:foreign-slot-value new-termios '(:struct termios) 'local-modes)
               (lognot (logior +echo+ +icanon+ +isig+ +iexten+)))
      (logandf (cffi:foreign-slot-value new-termios '(:struct termios) 'input-modes)
               (lognot (logior +ixon+ +brkint+)))
      (logandf (cffi:foreign-slot-value new-termios '(:struct termios) 'output-modes)
               (lognot (logior)))
      (check (tcsetattr fd +tcsaflush+ new-termios))))
  (values))

(defun disable-raw (terminal)
  (let ((termios (original-termios terminal)))
    (when termios
      (check (tcsetattr (fd (input terminal)) +tcsaflush+ termios))
      (cffi:foreign-free termios))))


;;;; SIGWINCH -----------------------------------------------------------------
(alexandria:define-constant +SIGWINCH+ 28)


(defparameter *sigwinch* nil)

(cffi:defcallback handle-sigwinch :void ((signo :int))
  (declare (ignore signo))
  (setf *sigwinch* t))

(defun install-sigwinch-handler (&optional (handler (cffi:callback handle-sigwinch)))
  (cffi:foreign-funcall "signal" :int +SIGWINCH+ :pointer handler :pointer))


;;;; Terminal -----------------------------------------------------------------
(deftype size ()
  `(integer 0 ,(1- array-dimension-limit)))

(defclass* ansi-terminal (terminal)
  ((input :type stream)
   (output :type stream)
   (needs-blit :type boolean :initform t)
   (handle-sigwinch :type boolean)
   (previous-handler :initform nil)
   (original-termios :initform nil)
   (buffer :initform (make-string-output-stream))
   (characters          :type (simple-array character (* *)))
   (previous-characters :type (simple-array character (* *)))
   (attributes          :type (simple-array attribute (* *)))
   (previous-attributes :type (simple-array attribute (* *)))))

(defun resize (terminal)
  (multiple-value-bind (width height) (get-terminal-size terminal)
    (setf (width terminal) width
          (height terminal) height

          (characters terminal)
          (make-array (list height width)
            :element-type 'character
            :initial-element #\space)

          (previous-characters terminal)
          (make-array (list height width)
            :element-type 'character
            :initial-element #\nul)

          (attributes terminal)
          (make-array (list height width)
            :element-type 'fixnum
            :initial-element (boots%:default))

          (previous-attributes terminal)
          (make-array (list height width)
            :element-type 'boots%:attribute
            :initial-element (boots%:invalid-attribute))
          ))
  (values))


(defun make-ansi-terminal (&key
                           (handle-sigwinch t)
                           (input-stream *standard-input*)
                           (output-stream *standard-output*))
  (make-instance 'ansi-terminal
    :handle-sigwinch handle-sigwinch
    :input input-stream
    :output output-stream))

(defun blit-attr (attr stream)
  (when (zerop attr)
    (mansion::reset stream)
    (return-from blit-attr))
  ;; todo optimize this process further
  (if (boldp attr)
    (mansion::bold stream)
    (mansion::no-bold stream))
  (if (italicp attr)
    (mansion::italic stream)
    (mansion::no-italic stream))
  (if (underlinep attr)
    (mansion::underline stream)
    (mansion::no-underline stream))
  (with-fg (c tc r g b) attr
    (if c
      (if tc
        (mansion::truecolor r g b stream)
        (mansion::rgb r g b stream))
      (mansion::fg-default stream)))
  (with-bg (c tc r g b) attr
    (if c
      (if tc
        (mansion::bg-truecolor r g b stream)
        (mansion::bg-rgb r g b stream))
      (mansion::bg-default stream)))
  (values))

(defun clear-array (array value)
  (destructuring-bind (h w) (array-dimensions array)
    (dotimes (y h)
      (dotimes (x w)
        (setf (aref array y x) value)))))


(defmethod blit ((terminal ansi-terminal))
  (setf *sigwinch* nil)
  (let ((chars (characters terminal))
        (attrs (attributes terminal))
        (pchars (previous-characters terminal))
        (pattrs (previous-attributes terminal))
        (buffer (buffer terminal))
        (stream (output terminal))
        (last-attr -1)
        (move t))
    (dotimes (y (height terminal))
      (dotimes (x (width terminal))
        (let ((char (aref chars y x))
              (attr (aref attrs y x))
              (pchar (aref pchars y x))
              (pattr (aref pattrs y x)))
          (if (and (char= char pchar)
                   (= attr pattr))
            (setf move t)
            (progn (when move
                     (mansion::move-cursor (1+ y) (1+ x) buffer)
                     (setf move nil last-attr -1))
                   (unless (= attr last-attr)
                     (blit-attr attr buffer))
                   (write-char char buffer)))))
      (setf move t last-attr -1))
    ;; todo let user move cursor
    (write-string (get-output-stream-string buffer) stream)
    (force-output stream))
  (rotatef (characters terminal) (previous-characters terminal))
  (rotatef (attributes terminal) (previous-attributes terminal))
  (clear-array (characters terminal) #\Space)
  (clear-array (attributes terminal) (boots%:default)))

(defmethod paint ((terminal ansi-terminal) x y width height character &optional (attr (boots%:default)))
  (declare (optimize speed))
  (check-types fixnum x y width height)
  (check-type character character)
  (let ((chars (characters terminal))
        (attrs (attributes terminal)))
    (declare (type (simple-array character (* *)) chars)
             (type (simple-array attribute (* *)) attrs))
    (loop :for x% fixnum :from x :repeat width :do
          (loop :for y% fixnum :from y :repeat height :do
                (setf (aref chars y% x%) character
                      (aref attrs y% x%) attr)))))

(defmethod put ((terminal ansi-terminal) x y character &optional attr)
  (declare (optimize speed))
  (let ((chars (characters terminal)))
    (declare (type (simple-array character (* *)) chars))
    (setf (aref chars y x) character))
  (when attr
    (let ((attrs (attributes terminal)))
      (declare (type (simple-array attribute (* *)) attrs))
      (setf (aref attrs y x) attr)))
  (values))

(defmethod read-event-no-hang ((terminal ansi-terminal))
  (when *sigwinch*
    (resize terminal)
    (boots:redraw))
  (read-char-no-hang (input terminal)))

(defmethod start ((terminal ansi-terminal))
  (save-terminal (output terminal))
  (clear-terminal (output terminal))
  (resize terminal)
  (mansion::hide-cursor (output terminal))
  (when (handle-sigwinch terminal)
    (setf (previous-handler terminal) (install-sigwinch-handler)))
  (enable-raw terminal))

(defmethod stop ((terminal ansi-terminal))
  (disable-raw terminal)
  (when (handle-sigwinch terminal)
    (install-sigwinch-handler (previous-handler terminal)))
  (clear-terminal (output terminal))
  (mansion::show-cursor (output terminal))
  (restore-terminal (output terminal))
  (print (original-termios terminal)))

