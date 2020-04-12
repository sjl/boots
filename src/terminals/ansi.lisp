(in-package :boots/terminals/ansi)

(defparameter *debug* nil)


;;;; Plumbing -----------------------------------------------------------------
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
     #-(or sbcl)
     (error "Don't know how to get a file descriptor for a stream in this implementation."))))

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


;;;; Terminal -----------------------------------------------------------------
(deftype size ()
  `(integer 0 ,(1- array-dimension-limit)))

(deftype char-array ()
  '(simple-array character (* *)))

(deftype attr-array ()
  '(simple-array attribute (* *)))

(defclass* ansi-terminal (terminal)
  ((input :type stream)
   (output :type stream)
   (previous-handler :initform nil)
   (original-termios :initform nil)
   (buffer :initform (make-string-output-stream))
   (characters          :type char-array)
   (previous-characters :type char-array)
   (attributes          :type attr-array)
   (previous-attributes :type attr-array)))


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
            :initial-element (boots%:invalid-attribute))))
  (values))

(defun needs-resize-p (terminal)
  (multiple-value-bind (width height) (get-terminal-size terminal)
    (or (/= width (width terminal))
        (/= height (height terminal)))))


(defun make-ansi-terminal (&key
                           (input-stream *standard-input*)
                           (output-stream *standard-output*))
  (make-instance 'ansi-terminal
    :input input-stream
    :output output-stream))

(defun blit-attr (prev attr stream)
  (declare (optimize speed)
           (type attribute prev attr))
  (when (zerop attr)
    (mansion::reset stream)
    (return-from blit-attr))
  (cond ((and (boldp attr) (not (boldp prev)))
         (mansion::bold stream))
        ((and (not (boldp attr)) (boldp prev))
         (mansion::no-bold stream)))
  (cond ((and (italicp attr) (not (italicp prev)))
         (mansion::italic stream))
        ((and (not (italicp attr)) (italicp prev))
         (mansion::no-italic stream)))
  (cond ((and (underlinep attr) (not (underlinep prev)))
         (mansion::underline stream))
        ((and (not (underlinep attr)) (underlinep prev))
         (mansion::no-underline stream)))
  (when (/= (fg attr) (fg prev))
    (with-fg (c tc r g b) attr
      (if c
        (if tc
          (mansion::truecolor r g b stream)
          (mansion::rgb r g b stream))
        (mansion::fg-default stream))))
  (when (/= (bg attr) (bg prev))
    (with-bg (c tc r g b) attr
      (if c
        (if tc
          (mansion::bg-truecolor r g b stream)
          (mansion::bg-rgb r g b stream))
        (mansion::bg-default stream))))
  nil)

(defun clear-array (array value)
  #+sbcl
  (fill (sb-ext:array-storage-vector array) value)
  #-(or sbcl)
  (destructuring-bind (h w) (array-dimensions array)
    (dotimes (y h)
      (dotimes (x w)
        (setf (aref array y x) value)))))


(defun blit% (terminal)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type terminal terminal))
  (let ((chars (characters terminal))
        (attrs (attributes terminal))
        (pchars (previous-characters terminal))
        (pattrs (previous-attributes terminal))
        (buffer (buffer terminal))
        (stream (output terminal))
        (last-attr (boots%:default))
        (move t))
    (declare (type char-array chars pchars)
             (type attr-array attrs pattrs))
    (mansion::reset stream)
    (dotimes (y (the fixnum (height terminal)))
      (dotimes (x (the fixnum (width terminal)))
        (let ((char (aref chars y x))
              (attr (aref attrs y x))
              (pchar (aref pchars y x))
              (pattr (aref pattrs y x)))
          (if (and (char= char pchar)
                   (= attr pattr))
            (setf move t)
            (progn (when move
                     (mansion::move-cursor (1+ y) (1+ x) buffer)
                     (setf move nil))
                   (unless (= attr last-attr)
                     (blit-attr last-attr attr buffer)
                     (setf last-attr attr))
                   (write-char char buffer)))))
      (setf move t))
    ;; todo let user move cursor
    (write-string (get-output-stream-string buffer) stream)
    (force-output stream))
  (rotatef (characters terminal) (previous-characters terminal))
  (rotatef (attributes terminal) (previous-attributes terminal))
  (clear-array (characters terminal) #\Space)
  (clear-array (attributes terminal) (boots%:default)))

(defmethod blit ((terminal ansi-terminal))
  (blit% terminal))

(defun paint% (terminal x y width height character attr)
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type fixnum x y width height)
           (type character character)
           (type attribute attr))
  (let ((chars (characters terminal))
        (attrs (attributes terminal)))
    (declare (type char-array chars)
             (type attr-array attrs))
    (loop :for x% fixnum :from x :below (the fixnum (+ x width)) :do
          (loop :for y% fixnum :from y :below (the fixnum (+ y height)) :do
                (setf (aref chars y% x%) character
                      (aref attrs y% x%) attr)))))

(defmethod paint ((terminal ansi-terminal) x y width height character &optional attr)
  (check-types fixnum x y width height)
  (check-type character character)
  (check-type attr (or null attribute))
  (paint% terminal x y width height character (or attr (boots%:default))))

(defmethod put ((terminal ansi-terminal) x y character &optional attr)
  (declare (optimize speed))
  (let ((chars (characters terminal)))
    (declare (type (simple-array character (* *)) chars))
    (setf (aref chars y x) character))
  (when attr
    (let ((attrs (attributes terminal)))
      (declare (type (simple-array attribute (* *)) attrs))
      (setf (aref attrs y x) attr)))
  nil)

(defmethod prep ((terminal ansi-terminal))
  (when (needs-resize-p terminal)
    (resize terminal))
  (values))

(defmethod read-event-no-hang ((terminal ansi-terminal))
  (when (needs-resize-p terminal)
    (resize terminal)
    (boots:redraw))
  (read-char-no-hang (input terminal)))

(defmethod start ((terminal ansi-terminal))
  (save-terminal (output terminal))
  (clear-terminal (output terminal))
  (resize terminal)
  (mansion::hide-cursor (output terminal))
  (enable-raw terminal))

(defmethod stop ((terminal ansi-terminal))
  (disable-raw terminal)
  (clear-terminal (output terminal))
  (mansion::show-cursor (output terminal))
  (restore-terminal (output terminal)))
