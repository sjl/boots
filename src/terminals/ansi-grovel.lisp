(in-package :boots/terminals/ansi)

(include
  "unistd.h"
  "termios.h"
  "fcntl.h"
  "errno.h"
  "sys/ioctl.h")

(constant (+TIOCGWINSZ+ "TIOCGWINSZ"))
(constant (+STDIN+ "STDIN_FILENO"))
(constant (+STDOUT+ "STDOUT_FILENO"))
(constant (+STDERR+ "STDERR_FILENO"))
(constant (+TCSAFLUSH+ "TCSAFLUSH"))
(constant (+ECHO+ "ECHO"))
(constant (+ICANON+ "ICANON"))
(constant (+IXON+ "IXON"))
(constant (+ICRNL+ "ICRNL"))
(constant (+IEXTEN+ "IEXTEN"))
(constant (+ISIG+ "ISIG"))
(constant (+OPOST+ "OPOST"))
(constant (+BRKINT+ "BRKINT"))
(constant (+F-SETFL+ "F_SETFL"))
(constant (+F-GETFL+ "F_SETFL"))
(constant (+O-NONBLOCK+ "O_NONBLOCK"))
(constant (+EAGAIN+ "EAGAIN"))
(constant (+EWOULDBLOCK+ "EWOULDBLOCK"))

(cstruct winsize "struct winsize"
  (ws-row "ws_row" :type :unsigned-short)
  (ws-col "ws_col" :type :unsigned-short))

(ctype :tcflag-t "tcflag_t")
;; (ctype :cc-t "cc_t")

(cstruct termios "struct termios"
  (input-modes   "c_iflag" :type :tcflag-t)
  (output-modes  "c_oflag" :type :tcflag-t)
  (control-modes "c_cflag" :type :tcflag-t)
  (local-modes   "c_lflag" :type :tcflag-t)
  ;; (special-characters "c_cc" :type :cc-t :size )
  )
