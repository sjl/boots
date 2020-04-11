(in-package :boots/terminals/ansi)

(deftype size ()
  `(integer 0 ,(1- array-dimension-limit)))

(defclass* ansi-terminal (terminal)
  ((input :type stream)
   (output :type stream)
   (characters :type (simple-array character (* *)))
   (attributes :type (simple-array attribute (* *)))))

(defun resize (terminal width height)
  (setf (width terminal) width
        (height terminal) height
        (characters terminal) (make-array (list height width)
                                :element-type 'character
                                :initial-element #\space)
        (attributes terminal) (make-array (list height width)
                                :element-type 'boots%:attribute
                                :initial-element 0))
  (values))

(defun make-ansi-terminal (&key (width 60) (height 20)
                           (input-stream *standard-input*)
                           (output-stream *standard-output*))
  (let ((terminal (make-instance 'ansi-terminal
                    :input input-stream
                    :output output-stream)))
    (resize terminal width height)
    terminal))

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

(defun clear (array value)
  (destructuring-bind (h w) (array-dimensions array)
    (dotimes (y h)
      (dotimes (x w)
        (setf (aref array y x) value)))))


(defmethod blit ((terminal ansi-terminal))
  ;; todo is it worth using a string-output-stream here to write everything into
  ;; the terminal at once?
  (let ((chars (characters terminal))
        (attrs (attributes terminal))
        (stream (output terminal))
        (last-attr -1))
    (mansion::clear-screen stream)
    (dotimes (y (height terminal))
      (dotimes (x (width terminal))
        (let ((attr (aref attrs y x))
              (char (aref chars y x)))
          (unless (= attr last-attr)
            (blit-attr attr stream))
          (write-char char stream)))
      (mansion::reset stream)
      (terpri stream))
    (clear chars #\space)
    (clear attrs 0)))

(defmethod put ((terminal ansi-terminal) x y character &optional attr)
  (setf (aref (characters terminal) y x) character)
  (when attr
    (setf (aref (attributes terminal) y x) attr))
  (values))

(defmethod read-event ((terminal ansi-terminal))
  (read-char (input terminal)))

(defmethod read-event-no-hang ((terminal ansi-terminal))
  (read-char-no-hang (input terminal)))

