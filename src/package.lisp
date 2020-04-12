;;;; Low-level implementation -------------------------------------------------
(defpackage :boots%
  (:use :cl)
  (:export
    :defun-inline :_ :defclass* :in-range-p :check-types

    :attribute
    :attr
    :rgb :rgb*
    :default
    :invalid-attribute
    :boldp :italicp :underlinep
    :with-fg :with-bg :with-color
    :r :g :b

    :event

    :*border-top-left-char*
    :*border-top-right-char*
    :*border-bottom-left-char*
    :*border-bottom-right-char*
    :*border-vertical-char*
    :*border-horizontal-char*

    :pad-w
    :pad-h
    :draw
    :redraw-screen

    :make-stack
    :make-shelf
    :make-pile
    :make-canvas
    :make-screen

    :root
    :children))


;;;; Terminals ----------------------------------------------------------------
(defpackage :boots/terminals
  (:use :cl :boots%)
  (:export
    :terminal
    :start
    :stop
    :blit
    :put
    :paint
    :read-event
    :read-event-no-hang
    :width
    :height))

(defpackage :boots/terminals/ansi
  (:use :cl :boots/terminals :boots%)
  (:export :make-ansi-terminal))


;;;; User-facing API ----------------------------------------------------------
(defpackage :boots
  (:use :cl)
  (:import-from :boots%
    :attr
    :rgb
    :rgb*
    :default

    :event

    :*border-top-left-char*
    :*border-top-right-char*
    :*border-bottom-left-char*
    :*border-bottom-right-char*
    :*border-vertical-char*
    :*border-horizontal-char*

    :draw

    :make-stack
    :make-shelf
    :make-pile
    :make-canvas
    :make-screen

    :root
    :children)
  (:export
    :attr
    :rgb
    :rgb*
    :default

    :event

    :*border-top-left-char*
    :*border-top-right-char*
    :*border-bottom-left-char*
    :*border-bottom-right-char*
    :*border-vertical-char*
    :*border-horizontal-char*

    :draw
    :width
    :height

    :make-stack
    :make-shelf
    :make-pile
    :make-canvas

    :stack
    :shelf
    :pile
    :canvas

    :root
    :children

    :*screen*
    :with-boots
    :redraw
    :read-event
    :read-event-no-hang))
