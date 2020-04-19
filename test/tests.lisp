(in-package :boots/test)


;;;; Utilities ----------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(intern (concatenate 'string (symbol-name 'test-) (symbol-name name)))
    (let ((*package* ,*package*))
      ,@body)))

(defun run-tests ()
  (1am:run))


;;;; Attributes ---------------------------------------------------------------
;;; unfortunately 1am outputs a . for every test, so if we try to exhaustively
;;; test the output will be megabytes of . characters.  Good and cool.

(defparameter *interesting-numbers*
  '(0 1 2 3 4 5 7 8 9 15 16 17 31 32 33 42 63 64 65 100 127 128 129 254 255))

(define-test test-colors
  (dolist (r *interesting-numbers*)
    (dolist (g *interesting-numbers*)
      (dolist (b *interesting-numbers*)
        (let ((color (boots:rgb r g b)))
          (boots%::with-color (has-color r% g% b%) color
            (is (eql t has-color))
            (is (eql t (boots%::colorp color)))
            (is (= r r%))
            (is (= r (boots%::r color)))
            (is (= g g%))
            (is (= g (boots%::g color)))
            (is (= b b%))
            (is (= b (boots%::b color)))))))))

(define-test test-invalid-attribute
  (is (boots%::invalidp (boots%:invalid-attribute))))

(define-test test-default-attribute
  (let ((a (boots:default)))
    (is (not (boots%::invalidp a)))
    (is (not (boots%::boldp a)))
    (is (not (boots%::italicp a)))
    (is (not (boots%::underlinep a)))
    (is (not (boots%::colorp (boots%:fg a))))
    (is (not (boots%::colorp (boots%:bg a))))))

(define-test test-attributes
  (let ((r (boots:rgb 200 50 10))
        (g (boots:rgb 50 200 10))
        (b (boots:rgb 10 50 200))
        (k (boots:rgb 0 0 0))
        (w (boots:rgb 255 255 255)))
    (dolist (bold '(nil t))
      (dolist (italic '(nil t))
        (dolist (underline '(nil t))
          (dolist (bg (list nil r g b k w))
            (dolist (fg (list nil r g b k w))
              (let ((a (boots:attr
                         :bold bold :italic italic :underline underline
                         :fg fg :bg bg)))
                (is (not (boots%::invalidp a)))
                (is (eql bold (boots%::boldp a)))
                (is (eql italic (boots%::italicp a)))
                (is (eql underline (boots%::underlinep a)))
                (if fg
                  (is (eql fg (boots%:fg a)))
                  (is (not (boots%::colorp (boots%:fg a)))))
                (if bg
                  (is (eql bg (boots%:bg a)))
                  (is (not (boots%::colorp (boots%:bg a)))))))))))))


;;;; Layout -------------------------------------------------------------------
(defun fill-with (char &optional (attr (boots:default)))
  (lambda (pad)
    (dotimes (y (boots:height pad))
      (dotimes (x (boots:width pad))
        (boots:draw pad x y char attr)))))

(defmacro define-layout-tests (name-and-options &rest width-height-root-expected-tuples)
  (destructuring-bind
      (name &key (border-wrapper 'boots:with-simple-borders))
      (alexandria:ensure-list name-and-options)
    (alexandria:with-gensyms (terminal)
      `(define-test ,name
         ,@(loop
             :for (width height root expected) :in width-height-root-expected-tuples
             :collect `(boots/terminals/static:with-static-terminal
                         (,terminal :width ,width :height ,height)
                         (boots:with-screen (boots:*screen* ,terminal :root ,root)
                           (,border-wrapper (boots:redraw))
                           (is (string= ,(format nil expected)
                                        (boots/terminals/static:stringify ,terminal))))))))))


(defmacro c (char &rest args)
  `(boots:make-canvas :draw (fill-with ,char) ,@args))

(defparameter *a* (c #\a))
(defparameter *b* (c #\b))
(defparameter *c* (c #\c))
(defparameter *d* (c #\d))
(defparameter *w* (c #\w))
(defparameter *x* (c #\x))
(defparameter *y* (c #\y))
(defparameter *z* (c #\z))
(defparameter *.* (c #\.))

(defparameter *stack-x.y* (boots:stack () *x* *.* *y*))
(defparameter *stack-w.z* (boots:stack () *w* *.* *z*))
(defparameter *shelf-a.b* (boots:shelf () *a* *.* *b*))
(defparameter *shelf-c.d* (boots:shelf () *c* *.* *d*))

(define-layout-tests trivial-layout
  (10 4 *x*
   "xxxxxxxxxx~@
    xxxxxxxxxx~@
    xxxxxxxxxx~@
    xxxxxxxxxx"))


(define-layout-tests stack-of-three
  (10 3 *stack-x.y*
   "xxxxxxxxxx~@
    ..........~@
    yyyyyyyyyy")
  (10 6 *stack-x.y*
   "xxxxxxxxxx~@
    xxxxxxxxxx~@
    ..........~@
    ..........~@
    yyyyyyyyyy~@
    yyyyyyyyyy")
  (10 7 *stack-x.y*
   "xxxxxxxxxx~@
    xxxxxxxxxx~@
    xxxxxxxxxx~@
    ..........~@
    ..........~@
    yyyyyyyyyy~@
    yyyyyyyyyy")
  (10 8 *stack-x.y*
   "xxxxxxxxxx~@
    xxxxxxxxxx~@
    xxxxxxxxxx~@
    ..........~@
    ..........~@
    ..........~@
    yyyyyyyyyy~@
    yyyyyyyyyy")
  (10 9 *stack-x.y*
   "xxxxxxxxxx~@
    xxxxxxxxxx~@
    xxxxxxxxxx~@
    ..........~@
    ..........~@
    ..........~@
    yyyyyyyyyy~@
    yyyyyyyyyy~@
    yyyyyyyyyy")
  (10 2 *stack-x.y*
   "xxxxxxxxxx~@
    ..........")
  (10 1 *stack-x.y*
   "xxxxxxxxxx")
  (10 0 *stack-x.y*
   ""))

(define-layout-tests shelf-of-three
  (3 2 *shelf-a.b*
   "a.b~@
    a.b")
  (6 3 *shelf-a.b*
   "aa..bb~@
    aa..bb~@
    aa..bb")
  (7 3 *shelf-a.b*
   "aaa..bb~@
    aaa..bb~@
    aaa..bb")
  (8 3 *shelf-a.b*
   "aaa...bb~@
    aaa...bb~@
    aaa...bb")
  (9 3 *shelf-a.b*
   "aaa...bbb~@
    aaa...bbb~@
    aaa...bbb")
  (2 3 *shelf-a.b*
   "a.~@
    a.~@
    a.")
  (1 3 *shelf-a.b*
   "a~@
    a~@
    a")
  (0 3 *shelf-a.b*
   "~@
    ~@
    "))

(define-layout-tests stack-of-shelves
  (6 2 (boots:stack ()
         (boots:shelf () *a* *b* *c*)
         (boots:shelf () *x* *y* *z*))
   "aabbcc~@
    xxyyzz")
  (6 3 (boots:stack ()
         (boots:shelf () *a* *b* *c*)
         (boots:shelf () *x* *y* *z*))
   "aabbcc~@
    aabbcc~@
    xxyyzz")
  (7 4 (boots:stack ()
         (boots:shelf () *a* *b* *c*)
         (boots:shelf () *x* *y* *z*))
   "aaabbcc~@
    aaabbcc~@
    xxxyyzz~@
    xxxyyzz"))

(define-layout-tests shelf-of-stacks
  (6 3 (boots:shelf ()
         (boots:stack () *a* *b* *c*)
         (boots:stack () *x* *y* *z*))
   "aaaxxx~@
    bbbyyy~@
    ccczzz")
  (1 3 (boots:shelf ()
         (boots:stack () *a* *b* *c*)
         (boots:stack () *x* *y* *z*))
   "a~@
    b~@
    c"))

(define-layout-tests trivial-piles
  (2 2 (boots:pile () *a*)
   "aa~@
    aa")
  (2 2 (boots:pile () *a* *b*)
   "aa~@
    aa")
  (2 2 (boots:pile () *b* *a*)
   "bb~@
    bb"))

(define-layout-tests basic-margins
  (10 5 (boots:pile () (c #\o :margin 1) *.*)
   "..........~@
    .oooooooo.~@
    .oooooooo.~@
    .oooooooo.~@
    ..........")
  (10 5 (boots:pile () (c #\o :margin-vertical 1) *.*)
   "..........~@
    oooooooooo~@
    oooooooooo~@
    oooooooooo~@
    ..........")
  (10 5 (boots:pile () (c #\o :margin-horizontal 2) *.*)
   "..oooooo..~@
    ..oooooo..~@
    ..oooooo..~@
    ..oooooo..~@
    ..oooooo..")
  (10 5 (boots:pile () (c #\o :margin-left 3) *.*)
   "...ooooooo~@
    ...ooooooo~@
    ...ooooooo~@
    ...ooooooo~@
    ...ooooooo")
  (10 5 (boots:pile () (c #\o :margin-right 3) *.*)
   "ooooooo...~@
    ooooooo...~@
    ooooooo...~@
    ooooooo...~@
    ooooooo...")
  (10 5 (boots:pile () (c #\o :margin-top 3) *.*)
   "..........~@
    ..........~@
    ..........~@
    oooooooooo~@
    oooooooooo")
  (10 5 (boots:pile () (c #\o :margin-bottom 3) *.*)
   "oooooooooo~@
    oooooooooo~@
    ..........~@
    ..........~@
    ..........")
  (10 5 (boots:pile () (c #\o :margin-bottom 1 :margin-left 2) *.*)
   "..oooooooo~@
    ..oooooooo~@
    ..oooooooo~@
    ..oooooooo~@
    .........."))

(define-layout-tests basic-borders
  (10 5 (c #\o :border nil)
   "oooooooooo~@
    oooooooooo~@
    oooooooooo~@
    oooooooooo~@
    oooooooooo")
  (10 5 (c #\o :border t)
   "+--------+~@
    |oooooooo|~@
    |oooooooo|~@
    |oooooooo|~@
    +--------+")
  (10 5 (c #\o :border-horizontal t)
   "----------~@
    oooooooooo~@
    oooooooooo~@
    oooooooooo~@
    ----------")
  (10 5 (c #\o :border-vertical t)
   "|oooooooo|~@
    |oooooooo|~@
    |oooooooo|~@
    |oooooooo|~@
    |oooooooo|")
  (10 5 (c #\o :border-left t)
   "|ooooooooo~@
    |ooooooooo~@
    |ooooooooo~@
    |ooooooooo~@
    |ooooooooo")
  (10 5 (c #\o :border-right t)
   "ooooooooo|~@
    ooooooooo|~@
    ooooooooo|~@
    ooooooooo|~@
    ooooooooo|")
  (10 5 (c #\o :border-top t)
   "----------~@
    oooooooooo~@
    oooooooooo~@
    oooooooooo~@
    oooooooooo")
  (10 5 (c #\o :border-bottom t)
   "oooooooooo~@
    oooooooooo~@
    oooooooooo~@
    oooooooooo~@
    ----------")
  (10 5 (c #\o :border-left t :border-bottom t)
   "|ooooooooo~@
    |ooooooooo~@
    |ooooooooo~@
    |ooooooooo~@
    +---------")
  (10 5 (c #\o :border-top t :border-right t)
   "---------+~@
    ooooooooo|~@
    ooooooooo|~@
    ooooooooo|~@
    ooooooooo|")
  (10 5 (c #\o :border t :border-right nil)
   "+---------~@
    |ooooooooo~@
    |ooooooooo~@
    |ooooooooo~@
    +---------"))

(define-layout-tests (light-borders :border-wrapper boots:with-light-borders)
  (10 5 (c #\o :border t)
   "┌────────┐~@
    │oooooooo│~@
    │oooooooo│~@
    │oooooooo│~@
    └────────┘"))

(define-layout-tests (heavy-borders :border-wrapper boots:with-heavy-borders)
  (10 5 (c #\o :border t)
   "┏━━━━━━━━┓~@
    ┃oooooooo┃~@
    ┃oooooooo┃~@
    ┃oooooooo┃~@
    ┗━━━━━━━━┛"))

(define-layout-tests basic-padding
  (10 5 (boots:pile () (c #\o :padding 1) *.*)
   "~:
          ~%~:
 oooooooo ~%~:
 oooooooo ~%~:
 oooooooo ~%~:
          ")
  (10 5 (boots:pile () (c #\o :padding-vertical 1) *.*)
   "~:
          ~%~:
oooooooooo~%~:
oooooooooo~%~:
oooooooooo~%~:
          ")
  (10 5 (boots:pile () (c #\o :padding-horizontal 2) *.*)
   "~:
  oooooo  ~%~:
  oooooo  ~%~:
  oooooo  ~%~:
  oooooo  ~%~:
  oooooo  ")
  (10 5 (boots:pile () (c #\o :padding-left 3) *.*)
   "~:
   ooooooo~%~:
   ooooooo~%~:
   ooooooo~%~:
   ooooooo~%~:
   ooooooo")
  (10 5 (boots:pile () (c #\o :padding-right 3) *.*)
   "~:
ooooooo   ~%~:
ooooooo   ~%~:
ooooooo   ~%~:
ooooooo   ~%~:
ooooooo   ")
  (10 5 (boots:pile () (c #\o :padding-top 3) *.*)
   "~:
          ~%~:
          ~%~:
          ~%~:
oooooooooo~%~:
oooooooooo")
  (10 5 (boots:pile () (c #\o :padding-bottom 3) *.*)
   "~:
oooooooooo~%~:
oooooooooo~%~:
          ~%~:
          ~%~:
          ")
  (10 5 (boots:pile () (c #\o :padding-bottom 1 :padding-left 2) *.*)
   "~:
  oooooooo~%~:
  oooooooo~%~:
  oooooooo~%~:
  oooooooo~%~:
          "))

(define-layout-tests margin-border-padding
  (10 8 (boots:pile () (c #\o :margin 1 :border t :padding 1) *.*)
   "..........~@
    .+------+.~@
    .|      |.~@
    .| oooo |.~@
    .| oooo |.~@
    .|      |.~@
    .+------+.~@
    ..........")
  (10 8 (boots:pile () (c #\o :margin 1 :border t :padding-left 1) *.*)
   "..........~@
    .+------+.~@
    .| ooooo|.~@
    .| ooooo|.~@
    .| ooooo|.~@
    .| ooooo|.~@
    .+------+.~@
    ..........")
  (10 8 (boots:pile () (c #\o :margin-right 1 :border-vertical t :padding-left 1) *.*)
   "| oooooo|.~@
    | oooooo|.~@
    | oooooo|.~@
    | oooooo|.~@
    | oooooo|.~@
    | oooooo|.~@
    | oooooo|.~@
    | oooooo|.")
  (10 8 (boots:pile () (c #\o :margin-left 1 :margin-top 2
                          :border-horizontal t
                          :padding-right 2 :padding-bottom 1)
                    *.*)
   "..........~@
    ..........~@
    .---------~@
    .ooooooo  ~@
    .ooooooo  ~@
    .ooooooo  ~@
    .         ~@
    .---------"))

(define-layout-tests fancy-layout
  (60 10 (boots:shelf ()
           (c #\L :width 1)
           (boots:stack (:border-right t)
             (c #\? :height 1 :border-bottom t :margin-bottom 1)
             *x*)
           (boots:pile ()
             (c #\x :width 5 :height 3 :margin-top 1 :margin-left 1 :border t)
             (c #\y :width 5 :height 3 :margin-top 3 :margin-left 3 :border t)
             (c #\z :width 5 :height 3 :margin-top 5 :margin-left 5 :border t)
             (c #\a :width 2 :height 2 :margin t :margin-top 2 :margin-right 1 :padding 1 :border t)
             *.*)
           (c #\R :width 1))
   "L?????????????????????????????|............................R~@
    L-----------------------------|.+-----+....................R~@
    L                             |.|xxxxx|.............+----+.R~@
    Lxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|.|xxxxx|-+...........|    |.R~@
    Lxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|.|xxxxx|y|...........| aa |.R~@
    Lxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|.+-----+y|-+.........| aa |.R~@
    Lxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|...|yyyyy|z|.........|    |.R~@
    Lxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|...+-----+z|.........+----+.R~@
    Lxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|.....|zzzzz|................R~@
    Lxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|.....+-----+................R"))

(define-layout-tests squeezing-vertically
  (18 16 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "..................~@
    ..................~@
    ..................~@
    ...+----------+...~@
    ...|          |...~@
    ...|          |...~@
    ...|          |...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...|          |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................~@
    ..................~@
    ..................")
  (18 15 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "..................~@
    ..................~@
    ...+----------+...~@
    ...|          |...~@
    ...|          |...~@
    ...|          |...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...|          |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................~@
    ..................~@
    ..................")
  (18 14 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "..................~@
    ..................~@
    ...+----------+...~@
    ...|          |...~@
    ...|          |...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...|          |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................~@
    ..................~@
    ..................")
  (18 13 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "..................~@
    ..................~@
    ...+----------+...~@
    ...|          |...~@
    ...|          |...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................~@
    ..................~@
    ..................")
  (18 12 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "..................~@
    ..................~@
    ...+----------+...~@
    ...|          |...~@
    ...|          |...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................~@
    ..................")
  (18 8 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "..................~@
    ...+----------+...~@
    ...|          |...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................")
  (18 7 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "...+----------+...~@
    ...|          |...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................")
  (18 6 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "...+----------+...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...|          |...~@
    ...+----------+...~@
    ..................")
  (18 5 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "...+----------+...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...+----------+...~@
    ..................")
  (18 4 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "...+----------+...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...+----------+...")
  (18 3 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "...+----------+...~@
    ...|   oooo   |...~@
    ...+----------+...")
  (18 2 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "...+----------+...~@
    ...+----------+...")
  (18 1 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   "...+----------+...")
  (18 0 (boots:pile () (c #\o :width 4 :height 2 :margin 3 :padding 3 :border t) *.*)
   ""))

(define-layout-tests squeezing-horizontally
  (18 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "...+----------+...~@
    ...|   oooo   |...~@
    ...|   oooo   |...~@
    ...+----------+...")
  (17 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "..+----------+...~@
    ..|   oooo   |...~@
    ..|   oooo   |...~@
    ..+----------+...")
  (16 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "..+---------+...~@
    ..|  oooo   |...~@
    ..|  oooo   |...~@
    ..+---------+...")
  (15 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "..+--------+...~@
    ..|  oooo  |...~@
    ..|  oooo  |...~@
    ..+--------+...")
  (14 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "..+--------+..~@
    ..|  oooo  |..~@
    ..|  oooo  |..~@
    ..+--------+..")
  (10 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   ".+------+.~@
    .| oooo |.~@
    .| oooo |.~@
    .+------+.")
  (9 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "+------+.~@
    | oooo |.~@
    | oooo |.~@
    +------+.")
  (8 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "+-----+.~@
    |oooo |.~@
    |oooo |.~@
    +-----+.")
  (7 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "+----+.~@
    |oooo|.~@
    |oooo|.~@
    +----+.")
  (6 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "+----+~@
    |oooo|~@
    |oooo|~@
    +----+")
  (5 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "+---+~@
    |ooo|~@
    |ooo|~@
    +---+")
  (3 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "+-+~@
    |o|~@
    |o|~@
    +-+")
  (2 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "++~@
    ||~@
    ||~@
    ++")
  (1 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "+~@
    |~@
    |~@
    +")
  (0 4 (boots:pile () (c #\o :width 4 :height 2 :margin-horizontal 3 :padding-horizontal 3 :border t) *.*)
   "~@
    ~@
    ~@
    "))


(define-layout-tests pile-margin
  (8 4 (boots:pile ()
         (c #\x :margin t :width 2 :height 2)
         *.*)
   "........~@
    ...xx...~@
    ...xx...~@
    ........")
  (8 4 (boots:pile ()
         (boots:stack ()
           (c #\x :margin t :width 2 :height 2))
         *.*)
   "........~@
    ...xx...~@
    ...xx...~@
    ........"))

(defparameter *squeezing-multi*
  (boots:pile ()
    (boots:shelf ()
      (c #\a :width 3 :height 1 :border t :margin-horizontal 2 :padding-horizontal 2)
      (c #\b :width 3 :height 1 :border t :margin-horizontal 2 :padding-horizontal 2)
      (c #\c :width 3 :height 1 :border t :margin-horizontal 2 :padding-horizontal 2))
    *.*))

(define-layout-tests squeezing-multi
  (44 3 *squeezing-multi*
   "..+-------+....+-------+....+-------+.......~@
    ..|  aaa  |....|  bbb  |....|  ccc  |.......~@
    ..+-------+....+-------+....+-------+.......")
  (43 3 *squeezing-multi*
   "..+-------+....+-------+....+-------+......~@
    ..|  aaa  |....|  bbb  |....|  ccc  |......~@
    ..+-------+....+-------+....+-------+......")
  (40 3 *squeezing-multi*
   "..+-------+....+-------+....+-------+...~@
    ..|  aaa  |....|  bbb  |....|  ccc  |...~@
    ..+-------+....+-------+....+-------+...")
  (39 3 *squeezing-multi*
   "..+-------+....+-------+....+-------+..~@
    ..|  aaa  |....|  bbb  |....|  ccc  |..~@
    ..+-------+....+-------+....+-------+..")
  (38 3 *squeezing-multi*
   ".+-------+....+-------+....+-------+..~@
    .|  aaa  |....|  bbb  |....|  ccc  |..~@
    .+-------+....+-------+....+-------+..")
  (37 3 *squeezing-multi*
   ".+------+....+-------+....+-------+..~@
    .| aaa  |....|  bbb  |....|  ccc  |..~@
    .+------+....+-------+....+-------+..")
  (36 3 *squeezing-multi*
   ".+-----+....+-------+....+-------+..~@
    .| aaa |....|  bbb  |....|  ccc  |..~@
    .+-----+....+-------+....+-------+..")
  (35 3 *squeezing-multi*
   ".+-----+...+-------+....+-------+..~@
    .| aaa |...|  bbb  |....|  ccc  |..~@
    .+-----+...+-------+....+-------+..")
  (34 3 *squeezing-multi*
   ".+-----+..+-------+....+-------+..~@
    .| aaa |..|  bbb  |....|  ccc  |..~@
    .+-----+..+-------+....+-------+..")
  (33 3 *squeezing-multi*
   ".+-----+..+------+....+-------+..~@
    .| aaa |..| bbb  |....|  ccc  |..~@
    .+-----+..+------+....+-------+..")
  (32 3 *squeezing-multi*
   ".+-----+..+-----+....+-------+..~@
    .| aaa |..| bbb |....|  ccc  |..~@
    .+-----+..+-----+....+-------+..")
  (31 3 *squeezing-multi*
   ".+-----+..+-----+...+-------+..~@
    .| aaa |..| bbb |...|  ccc  |..~@
    .+-----+..+-----+...+-------+..")
  (30 3 *squeezing-multi*
   ".+-----+..+-----+..+-------+..~@
    .| aaa |..| bbb |..|  ccc  |..~@
    .+-----+..+-----+..+-------+..")
  (29 3 *squeezing-multi*
   ".+-----+..+-----+..+------+..~@
    .| aaa |..| bbb |..| ccc  |..~@
    .+-----+..+-----+..+------+..")
  (28 3 *squeezing-multi*
   ".+-----+..+-----+..+-----+..~@
    .| aaa |..| bbb |..| ccc |..~@
    .+-----+..+-----+..+-----+..")
  (27 3 *squeezing-multi*
   ".+-----+..+-----+..+-----+.~@
    .| aaa |..| bbb |..| ccc |.~@
    .+-----+..+-----+..+-----+.")
  (26 3 *squeezing-multi*
   "+-----+..+-----+..+-----+.~@
    | aaa |..| bbb |..| ccc |.~@
    +-----+..+-----+..+-----+.")
  (25 3 *squeezing-multi*
   "+----+..+-----+..+-----+.~@
    |aaa |..| bbb |..| ccc |.~@
    +----+..+-----+..+-----+.")
  (24 3 *squeezing-multi*
   "+---+..+-----+..+-----+.~@
    |aaa|..| bbb |..| ccc |.~@
    +---+..+-----+..+-----+.")
  (23 3 *squeezing-multi*
   "+---+.+-----+..+-----+.~@
    |aaa|.| bbb |..| ccc |.~@
    +---+.+-----+..+-----+.")
  (22 3 *squeezing-multi*
   "+---++-----+..+-----+.~@
    |aaa|| bbb |..| ccc |.~@
    +---++-----+..+-----+.")
  (20 3 *squeezing-multi*
   "+---++---+..+-----+.~@
    |aaa||bbb|..| ccc |.~@
    +---++---+..+-----+.")
  (18 3 *squeezing-multi*
   "+---++---++-----+.~@
    |aaa||bbb|| ccc |.~@
    +---++---++-----+.")
  (16 3 *squeezing-multi*
   "+---++---++---+.~@
    |aaa||bbb||ccc|.~@
    +---++---++---+.")
  (15 3 *squeezing-multi*
   "+---++---++---+~@
    |aaa||bbb||ccc|~@
    +---++---++---+")
  (12 3 *squeezing-multi*
   "+--++--++--+~@
    |aa||bb||cc|~@
    +--++--++--+")
  (9 3 *squeezing-multi*
   "+-++-++-+~@
    |a||b||c|~@
    +-++-++-+")
  (8 3 *squeezing-multi*
   "+++-++-+~@
    |||b||c|~@
    +++-++-+")
  (7 3 *squeezing-multi*
   "+++++-+~@
    |||||c|~@
    +++++-+")
  (6 3 *squeezing-multi*
   "++++++~@
    ||||||~@
    ++++++"))

(define-layout-tests fill-chars
  (10 5 (boots:pile ()
          (boots:shelf (:height 1 :fill-char #\_ :padding 1)
            (boots:make-canvas :width 3 :fill-char #\a)
            (boots:make-canvas :fill-char nil)
            (boots:make-canvas :width 3 :fill-char #\b))
          *.*)
   "__________~@
    _aaa__bbb_~@
    __________~@
    ..........~@
    ..........")
  (10 5 (boots:pile ()
          (boots:shelf (:height 1 :fill-char #\_ :padding 1)
            (boots:make-canvas :width 3 :fill-char #\a :margin-right t)
            (boots:make-canvas :width 3 :fill-char #\b))
          *.*)
   "__________~@
    _aaa__bbb_~@
    __________~@
    ..........~@
    ..........")
  (10 5 (boots:pile ()
          (boots:shelf (:height 1 :fill-char #\_ :padding 1)
            (boots:make-canvas :width 3 :fill-char #\a)
            (boots:make-canvas :width 3 :fill-char #\b :margin-left t))
          *.*)
   "__________~@
    _aaa__bbb_~@
    __________~@
    ..........~@
    ..........")
  (10 5 (boots:pile ()
          (boots:shelf (:height 1 :fill-char #\_ :padding 1)
            (boots:make-canvas :width 3 :fill-char #\a)
            (boots:make-canvas :width 3 :margin-left t))
          *.*)
   "__________~@
    _aaa__   _~@
    __________~@
    ..........~@
    ..........")
  (10 5 (boots:pile ()
          (boots:shelf (:height 2 :fill-char #\_ :padding 1)
            (boots:make-canvas :width 3 :fill-char #\a)
            (boots:make-canvas :width 3 :margin-left t))
          *.*)
   "__________~@
    _aaa__   _~@
    _aaa__   _~@
    __________~@
    ..........")
  (10 5 (boots:shelf (:fill-char #\_ :padding 1)
          (boots:make-canvas :fill-char #\a)
          (boots:make-canvas :fill-char #\b))
   "__________~@
    _aaaabbbb_~@
    _aaaabbbb_~@
    _aaaabbbb_~@
    __________")
  (10 5 (boots:shelf (:fill-char #\_ :padding 1)
          (boots:make-canvas :fill-char #\a)
          (boots:make-canvas))
   "__________~@
    _aaaa    _~@
    _aaaa    _~@
    _aaaa    _~@
    __________")
  (10 5 (boots:stack (:fill-char #\_ :padding 1)
          (boots:make-canvas :fill-char #\a)
          (boots:make-canvas :fill-char #\b))
   "__________~@
    _aaaaaaaa_~@
    _aaaaaaaa_~@
    _bbbbbbbb_~@
    __________")
  (10 5 (boots:stack (:fill-char #\_ :padding 1)
          (boots:make-canvas :fill-char #\space)
          (boots:make-canvas :fill-char #\b))
   "__________~@
    _        _~@
    _        _~@
    _bbbbbbbb_~@
    __________"))


