(in-package :boots/terminal)

(defcfun "ioctl" :int
  (file-descriptor :int)
  (request :unsigned-long)
  &rest)

(defcfun "tcgetattr" :int
  (file-descriptor :int)
  (termios :pointer))

(defcfun "tcsetattr" :int
  (file-descriptor :int)
  (optional-actions :int)
  (termios :pointer))

(defcfun ("read" c-read) :int
  (file-descriptor :int)
  (buffer :pointer)
  (bytes :int)) ; close enough

(defcvar "errno" :int)
(defcfun "strerror" :string
  (errno :int))

(defun check% (value form acceptable-errors)
  (assert (or (/= -1 value)
              (member *errno* acceptable-errors))
      ()
    "Error in ~S: ~A" form (strerror *errno*))
  value)

(defmacro check (form &key acceptable-errors)
  `(check% ,form ',form ,acceptable-errors))

(define-modify-macro logandf (&rest args) logand)

(defun enable-raw ()
  (with-foreign-object (term '(:struct termios))
    (check (tcgetattr +stdin+ term))
    (logandf (foreign-slot-value term '(:struct termios) 'local-modes)
             (lognot (logior +echo+ +icanon+)))
    (check (tcsetattr +stdin+ +tcsaflush+ term)))
  (values))

(defun disable-raw (original-termios)
  (check (tcsetattr +stdin+ +tcsaflush+ original-termios)))

(defun terminal-size ()
  (cffi:with-foreign-object (ws '(:struct winsize))
    (pr (ioctl 1 +tiocgwinsz+ :pointer ws))
    (let ((result (cffi:convert-from-foreign ws '(:struct winsize))))
      (values (getf result 'ws-col)
              (getf result 'ws-row)))))

(defun read-key (&key (wait t))
  (with-foreign-object (ch :char)
    (loop :for bytes-read = (check (c-read +stdin+ ch 1)
                                   :acceptable-errors '(+eagain+))
          :do (if (= 1 bytes-read)
                (return (mem-ref ch :char))
                (when (not wait)
                  (return nil))))))

(defun esc (string)
  (write-char #\Escape)
  (write-string string)
  (force-output))

(defun save-terminal ()
  (esc "[?1049h"))

(defun restore-terminal ()
  (esc "[?1049l"))

(defun clear-terminal ()
  (esc "[2J"))

(defmacro with-raw-mode (&body body)
  (alexandria:with-gensyms (original-termios)
    `(with-foreign-object (,original-termios '(:struct termios))
       (check (tcgetattr +stdin+ ,original-termios))
       (enable-raw)
       (save-terminal)
       (clear-terminal)
       (unwind-protect
           (progn ,@body)
         (disable-raw ,original-termios)
         (restore-terminal)))))



(defun foo ()
  (with-raw-mode
    (loop :for k = (read-key)
          :until (= k (char-code #\q))
          :do (format t "Got: ~S~%" k))))
