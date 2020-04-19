(in-package :boots/terminals/ansi)

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

(defun fd (stream direction)
  (declare (ignorable direction))
  ;; todo this isn't right, but it's good enough for the moment
  (cond
    ((eql stream *standard-input*) +STDIN+)
    ((eql stream *standard-output*) +STDOUT+)
    ((eql stream *error-output*) +STDERR+)
    (t
     #+sbcl (sb-posix:file-descriptor stream)
     #+ccl (ccl:stream-device stream direction)
     #-(or sbcl ccl)
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
    (check (ioctl (fd (output terminal) :output) +TIOCGWINSZ+ :pointer ws))
    (let ((result (cffi:convert-from-foreign ws '(:struct winsize))))
      (values (getf result 'ws-col)
              (getf result 'ws-row)))))

(defun save-termios (terminal)
  (setf (original-termios terminal) (cffi:foreign-alloc '(:struct termios)))
  (check (tcgetattr (fd (input terminal) :input) (original-termios terminal))))

(defun enable-raw (terminal)
  (save-termios terminal)
  (let ((fd (fd (input terminal) :input)))
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
      (check (tcsetattr (fd (input terminal) :input) +tcsaflush+ termios))
      (cffi:foreign-free termios))))


;;;; Terminal -----------------------------------------------------------------
(defclass* ansi-terminal (terminal)
  ((input :type stream)
   (output :type stream)
   (truecolor :type boolean)
   (original-termios :initform nil)
   (buffer :initform (make-string-output-stream))
   ;; https://github.com/Clozure/ccl/issues/291
   (characters          #-ccl :type #-ccl char-array)
   (previous-characters #-ccl :type #-ccl char-array)
   (attributes          #-ccl :type #-ccl attr-array)
   (previous-attributes #-ccl :type #-ccl attr-array)))


(defmacro with-arrays ((chars attrs &optional pchars pattrs) terminal &body body)
  (alexandria:once-only (terminal)
    `(let ((,chars (characters ,terminal))
           (,attrs (attributes ,terminal))
           ,@(when pchars `((,pchars (previous-characters ,terminal))))
           ,@(when pattrs `((,pattrs (previous-attributes ,terminal)))))
       (declare (type char-array ,chars ,@(when pchars '(pchars)))
                (type attr-array ,attrs ,@(when pattrs '(pattrs))))
       ,@body)))


(defun resize (terminal)
  (multiple-value-bind (width height) (get-terminal-size terminal)
    (setf
      (width terminal)  width
      (height terminal) height
      (characters terminal)          (make2d height width 'character #\space)
      (previous-characters terminal) (make2d height width 'character #\nul)
      (attributes terminal)          (make2d height width 'attribute (default))
      (previous-attributes terminal) (make2d height width 'attribute (invalid-attribute))))
  (values))

(defun needs-resize-p (terminal)
  (multiple-value-bind (width height) (get-terminal-size terminal)
    (or (/= width (width terminal))
        (/= height (height terminal)))))


(defmacro with-ansi-terminal ((symbol &key
                                      (input-stream '*standard-input*)
                                      (output-stream '*standard-output*)
                                      (truecolor t))
                              &body body)
  `(let ((,symbol (make-instance 'ansi-terminal
                    :input ,input-stream
                    :output ,output-stream
                    :truecolor ,truecolor)))
     (start ,symbol)
     (unwind-protect (progn ,@body)
       (stop ,symbol))))

(defun-inline simplify-color (value)
  "Convert an 8-bit truecolor channel value to a 0-6 256color value."
  ;; todo make this smarter
  (cond
    ((<= value 10) 0)
    ((<= value 50) 1)
    ((<= value 100) 2)
    ((<= value 150) 3)
    ((<= value 200) 4)
    (t 5)))

(defun blit-attr (truecolor prev attr stream)
  (declare (optimize speed)
           (type attribute prev attr))
  (when (zerop attr)
    (mansion::reset stream)
    (return-from blit-attr))
  (let ((b (boldp attr))      (bp (boldp prev))
        (i (italicp attr))    (ip (italicp prev))
        (u (underlinep attr)) (up (underlinep prev)))
    (cond ((and b (not bp)) (mansion::bold stream))
          ((and (not b) bp) (mansion::no-bold stream)))
    (cond ((and i (not ip)) (mansion::italic stream))
          ((and (not i) ip) (mansion::no-italic stream)))
    (cond ((and u (not up)) (mansion::underline stream))
          ((and (not u) up) (mansion::no-underline stream))))
  (when (/= (fg attr) (fg prev))
    (with-fg (c r g b) attr
      (if c
        (if truecolor
          (mansion::truecolor r g b stream)
          (mansion::rgb (simplify-color r) (simplify-color g) (simplify-color b)
                        stream))
        (mansion::fg-default stream))))
  (when (/= (bg attr) (bg prev))
    (with-bg (c r g b) attr
      (if c
        (if truecolor
          (mansion::bg-truecolor r g b stream)
          (mansion::bg-rgb (simplify-color r) (simplify-color g) (simplify-color b)
                           stream))
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
  (with-arrays (chars attrs pchars pattrs) terminal
    (let ((truecolor (truecolor terminal))
          (buffer (buffer terminal))
          (stream (output terminal))
          (last-attr (boots%:default))
          (move t))
      (mansion::reset stream)
      (dotimes (y (the size (height terminal)))
        (dotimes (x (the size (width terminal)))
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
                       (blit-attr truecolor last-attr attr buffer)
                       (setf last-attr attr))
                     (write-char char buffer)))))
        (setf move t))
      ;; todo let user move cursor
      (write-string (get-output-stream-string buffer) stream)
      (force-output stream)))
  (rotatef (characters terminal) (previous-characters terminal))
  (rotatef (attributes terminal) (previous-attributes terminal))
  (clear-array (characters terminal) #\Space)
  (clear-array (attributes terminal) (boots%:default)))

(defmethod blit ((terminal ansi-terminal))
  (blit% terminal))


(defmethod draw-region ((terminal ansi-terminal) x y width height characters attributes)
  (declare (optimize speed))
  (require-types coord x y)
  (require-types size width height)
  (require-type characters char-array)
  (require-type attributes attr-array)
  (with-arrays (chars attrs) terminal
    (loop :for x% :from x :below (+ x width) :do
          (loop :for y% :from y :below (+ y height)
                :for c = (aref characters y% x%)
                :unless (char= #\nul c)
                :do (setf (aref chars y% x%) c
                          (aref attrs y% x%) (aref attributes y% x%))))))


(defmethod prep ((terminal ansi-terminal) full)
  (when (or full (needs-resize-p terminal))
    (resize terminal)) ; todo name this something better
  (values))

(defmethod read-event-no-hang ((terminal ansi-terminal))
  (when (needs-resize-p terminal)
    (resize terminal)
    (boots:redraw))
  (read-char-no-hang (input terminal)))


(defun start (terminal)
  (save-terminal (output terminal))
  (clear-terminal (output terminal))
  (resize terminal)
  (mansion::hide-cursor (output terminal))
  (enable-raw terminal))

(defun stop (terminal)
  (disable-raw terminal)
  (clear-terminal (output terminal))
  (mansion::show-cursor (output terminal))
  (restore-terminal (output terminal)))
