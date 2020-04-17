;;;; A port of http://robotfindskitten.org/ to Common Lisp and Boots.

(ql:quickload '(:boots :bobbin :alexandria :chancery))

(defpackage :boots/examples/afk
  (:use :cl)
  (:export :run))

(in-package :boots/examples/afk)

;;;; Configuration ------------------------------------------------------------
(defparameter *version* "1.0.0")
(defconstant +world-width+ 200)
(defconstant +world-height+ 100)


;;;; Entities -----------------------------------------------------------------
(defclass entity ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (glyph :accessor glyph :initarg :glyph)
   (desc :accessor desc :initarg :desc)
   (attr :accessor attr :initarg :attr :initform (boots:default))))


(defparameter *player* nil)
(defparameter *kitten* nil)
(defparameter *world* nil)


(defmacro wref (x y)
  `(aref *world* (mod ,x +world-width+) (mod ,y +world-height+)))

(defun move (entity x y)
  (rotatef (wref (x entity) (y entity))
           (wref x y))
  (setf (x entity) x
        (y entity) y))

(defun draw (entity pad x y)
  (boots:draw pad x y (glyph entity) (attr entity)))


;;;; Whimsy -------------------------------------------------------------------
(defun randomp ()
  (< (random 1.0) 0.5))

(defun random-glyph ()
  (alexandria:random-elt "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(defun random-color ()
  (let ((r (random 255))
        (g (random 255))
        (b (random 255)))
    (if (and (< r 40) (< g 40) (< b 60))
      (random-color)
      (boots:rgb r g b))))


(chancery:define-string color
  "red" "blue" "green" "cyan" "magenta" "yellow" "black")

(chancery:define-string name
  "Steve" "Bob" "Sam" "Todd" "Billy" "John" "Jon" "James" "Frank" "Gary"
  "Rachel" "Amy" "Elaine" "Laura" "Sarah" "Edie" "Mabel" "Liz" "Florence")


(chancery:define-string clothing-noun
  "pair of socks" "pair of ankle socks"
  "pair of shoes" "pair of boots" "pair of sandals" "pair of crocs"
  "t-shirt" "sweatshirt" "dress shirt" "blouse" "tank top"
  "pair of pants" "pair of slacks" "pair of jeans" "jean shorts" "skirt"
  "denim jacket" "leather jacket" "peacoat" "trenchcoat"
  "belt" "necklace" "choker" "bracelet" "anklet" "pair of earrings" "ring"
  "suit" "bespoke suit"
  "dress" "sundress")

(chancery:define-string clothing-adjective
  color "smelly" "freshly-laundered" "worn-out" "brand-new" "tight"
  "loose-fitting" "wet" "fancy" "expensive-looking" "cheap-looking")

(chancery:define-string clothing%
  clothing-noun
  (clothing-adjective clothing-noun))

(chancery:define-string clothing
  (#(clothing% chancery:a)))


(chancery:define-string flower%
  "rose" "tulip" "carnation" "dandelion" "iris" "orchid" "sunflower" "violet"
  "daffodil" "tiger lily" "lily" "hyacinth" "lilac" "poppy" "windflower")

(chancery:define-string flower
  (#(flower% chancery:a))
  ("a bouquet of" #(flower% chancery:s)))


(chancery:define-string raw-food
  "corned beef" "salmon meunière" "hash browns" "scrambled eggs" "ribeye steak"
  "potatos" "horseradish" "pickles" "pickled onions" "jambalaya" "hamburger"
  "french fries" "spam" "basmati rice" "sushi" "scalloped potatos" "grits"
  "hot dogs" "home fries" "beets" "turnips")

(chancery:define-string food
  ("a tin of" raw-food)
  ("a plate of" raw-food)
  ("a can of" raw-food)
  ("a bag of" raw-food)
  ("some" raw-food))


(chancery:define-string potion%
  "healing" "giant strength" "mutation" "experience" "degeneration" "agility"
  "might" "holy water" "unholy water" "detect magic")

(chancery:define-string liquid%
  "water" "soda" "birch beer" "root beer" "ginger beer" "tea" "coffee"
  "oil" "canola oil" "olive oil" "peanut oil" "sesame oil" "vegetable oil"
  "alcohol" "beer" "red wine" "white wine" "sherry" "whiskey" "whisky"
  "bourbon" "rum" "vodka" "mead")

(chancery:define-string liquid
  ("a potion of" potion%)
  ("a can of" liquid%)
  ("a pitcher of" liquid%)
  ("a glass of" liquid%))


(chancery:define-string animal-noun
  "cat" "dog" "mouse" "rabbit" "kangaroo" "naked mole rat" "zebra" "butterfly"
  "slug" "banana slug" "turtle" "hedgehog" "fox" "squirrel" "turkey" "horse")

(chancery:define-string animal%
  (animal-noun)
  (animal-noun "named" name))

(chancery:define-string animal
  (#(animal% chancery:a))
  (#(animal% chancery:a) :. ", dressed in" clothing)
  (#(animal% chancery:a) :. ", eating" food)
  (#(animal% chancery:a) :. ", drinking" liquid))


(chancery:define-string location-noun
  "forest" "mountain" "river" "stream" "parking lot" "field of grass")

(chancery:define-string location-adjective
  "beautiful" "peaceful" "snowy" "frozen" "rain-soaked" "majestic" "dry")

(chancery:define-string location
  (#(location-noun chancery:a))
  (#(location-adjective chancery:a) location-noun))


(chancery:define-string programming-language
  "Lisp" "COBOL" "C" "C++" "Ruby" "Golang" "Python" "Java" "FORTRAN")

(chancery:define-string profession
  "doctor" "nurse" "teacher" "programmer" "manager" "cashier" "worker" "writer"
  "author" "editor" "director" "actor" "speechwriter" "janitor" "crossing guard"
  (programming-language "programmer"))

(chancery:define-string vendor-wares
  food clothing liquid flower
  (#(animal-noun chancery:s)))

(chancery:define-string human%
  profession
  ("street vendor selling" vendor-wares)
  ("explorer named" name :. ", returning from" location))

(chancery:define-string prey
  (#(animal-noun chancery:s))
  (color #(animal-noun chancery:s))
  (#(animal-noun chancery:s) "that wear" clothing))

(chancery:define-string hunter
  ("a man hunting" prey)
  ("a woman hunting" prey)
  (name :. ", the legendary hunter of" prey))

(chancery:define-string human
  (#(human% chancery:a))
  (#(human% chancery:a) :. ", wearing" clothing)
  (#(human% chancery:a) :. ", eating" food)
  (#(human% chancery:a) :. ", drinking" liquid)
  hunter)


(chancery:define-string random-description
  food animal human location clothing liquid)


;;;; World Generation ---------------------------------------------------------
(defun make-player (x y)
  (setf (wref x y)
        (make-instance 'entity
          :x x :y y
          :glyph #\@
          :attr (boots:attr :bold t
                            :fg (boots:rgb 0 0 0)
                            :bg (boots:rgb 50 255 90))
          :desc "The Lisp alien.")))

(defun make-thing (x y)
  (setf (wref x y)
        (make-instance 'entity
          :x x :y y
          :glyph (random-glyph)
          :attr (boots:attr :bold (randomp) :fg (random-color))
          :desc (format nil "~A." (chancery:cap (random-description))))))

(defun make-kitten (x y)
  (setf (wref x y)
        (make-instance 'entity
          :x x :y y
          :glyph (random-glyph)
          :attr (boots:attr :bold (randomp) :fg (random-color))
          :desc "Kitten.")))

(defun find-free-space ()
  (loop
    :for x = (random +world-width+)
    :for y = (random +world-height+)
    :when (null (wref x y))
    :do (return (values x y))))

(defun initialize ()
  (setf *world* (make-array (list +world-width+ +world-height+) :initial-element nil)
        *player* (make-player 5 5))
  (dotimes (i (round (* 1/200 (* +world-width+ +world-height+))))
    (multiple-value-bind (x y) (find-free-space)
      (make-thing x y)))
  (multiple-value-bind (x y) (find-free-space)
    (setf *kitten* (make-kitten x y)))
  nil)


;;;; Win Screen ---------------------------------------------------------------
(defun press-any-key ()
  (boots:read-event)
  nil)

(defun draw/win (frame pad)
  (let ((aframe (min frame 4)))
    (draw *player* pad (+ 13 aframe) 1)
    (draw *kitten* pad (+ 13 (- 9 aframe)) 1))
  (when (> frame 4) (boots:draw pad 17 0 "!!" (boots:attr :bold t :fg (boots:rgb 255 100 180))))
  (when (> frame 5) (boots:draw pad 0  2 "You found kitten!  Way to go, alien!") ))

(defun win (&aux (frame 0))
  (push (boots:canvas (:border t :margin t :height 3 :width 36) (pad)
          (draw/win frame pad))
        (boots:children (boots:root boots:*screen*)))
  (loop :repeat 7 :do (boots:redraw) (incf frame) (sleep 1/2))
  (loop :until (member (boots:read-event) '(#\newline #\q #\space))))


;;;; Game Loop ----------------------------------------------------------------
(defparameter *log* nil)

(defun draw/status (pad)
  (boots:draw pad 0 0 (format nil "alienfindskitten v~A" *version*))
  (when *log*
    (boots:draw pad 0 1 *log*)))

(defun draw/world (pad)
  (loop
    :for wy :from (- (y *player*) (truncate (boots:height pad) 2))
    :for py :from 0 :below (boots:height pad)
    :do (loop
          :for wx :from (- (x *player*) (truncate (boots:width pad) 2))
          :for px :from 0 :below (boots:width pad)
          :for thing = (wref wx wy)
          :when thing :do (draw thing pad px py))))

(defun handle-move (dx dy)
  (let* ((tx (mod (+ dx (x *player*)) +world-width+))
         (ty (mod (+ dy (y *player*)) +world-height+))
         (blocker (wref tx ty)))
    (if blocker
        (if (eql blocker *kitten*)
          (throw 'kitten :meow)
          (setf *log* (desc blocker)))
      (move *player* tx ty))))

(defun game ()
  (initialize)
  (setf *log* nil
        (boots:root boots:*screen*)
        (boots:pile ()
          (boots:stack ()
            (boots:make-canvas :height 2 :draw #'draw/status)
            (boots:make-canvas :border t :draw #'draw/world))))
  (ecase (catch 'kitten
           (loop
             (boots:redraw)
             (case (boots:read-event)
               (#\q (return :quit))
               (#\h (handle-move -1  0))
               (#\j (handle-move  0  1))
               (#\k (handle-move  0 -1))
               (#\l (handle-move  1  0))
               (#\y (handle-move -1 -1))
               (#\u (handle-move  1 -1))
               (#\b (handle-move -1  1))
               (#\n (handle-move  1  1)))))
    (:meow (win))
    (:quit nil)))


;;;; Intro --------------------------------------------------------------------
(defparameter *intro-text* (format nil "~
  alienfindskitten v~A~@
  a port of robotfindskitten to Common Lisp and boots~@
  by Steve Losh <steve@stevelosh.com> (C) 2020~@
  ~@
  In this game, you are the Lisp alien (@).  Your job is to find kitten.  This ~
  task is complicated by the existence of various things which are not kitten.  ~
  Alien must touch items to determine if they are kitten or not.  The game ends ~
  when alienfindskitten.  Alternatively, you may end the game by hitting the ~
  q key.~@
  ~@
  Unlike the origina robotfindskitten game, the world inhabited by the Lisp ~
  alien is a torus, not a flat plane.  There is also no danger of crushing the ~
  simulation (and alien, and kitten).~@
  ~@
  Press any key to start." *version*))

(defun draw/intro (pad)
  (boots:draw pad 0 0 (bobbin:wrap *intro-text* (boots:width pad))))

(defun intro ()
  (setf (boots:root boots:*screen*)
        (boots:make-canvas :width 80 :draw #'draw/intro))
  (boots:redraw)
  (press-any-key)
  (game))


;;;; Toplevel -----------------------------------------------------------------
(defun run ()
  (setf *random-state* (make-random-state t))
  (boots/terminals/ansi:with-ansi-terminal (terminal :truecolor t)
    (boots:with-screen (boots:*screen* terminal)
      (intro))))
