;;;; Low-level implementation -------------------------------------------------
(defpackage :boots%
  (:use :cl)
  (:export
    :*screen*
    :*border-top-left-char*
    :*border-top-right-char*
    :*border-bottom-left-char*
    :*border-bottom-right-char*
    :*border-vertical-char*
    :*border-horizontal-char*

    :size :coord :char-array :attr-array

    :defun-inline :_ :defclass* :in-range-p :make2d :fill2d
    :require-type :require-types

    :attribute :color
    :attr :rgb
    :default :invalid-attribute
    :boldp :italicp :underlinep
    :with-fg :with-bg :with-color
    :fg :bg :r :g :b

    :event

    :pad
    :pad-w
    :pad-h
    :draw
    :paint
    :redraw-screen

    :stack :make-stack
    :shelf :make-shelf
    :pile :make-pile
    :canvas :make-canvas
    :screen :make-screen

    :root
    :children))


;;;; Terminals ----------------------------------------------------------------
(defpackage :boots/terminals
  (:use :cl :boots%)
  (:shadow :paint)
  (:export
    :terminal
    :prep
    :blit
    :put
    :paint
    :read-event
    :read-event-no-hang
    :width
    :height))

(defpackage :boots/terminals/ansi
  (:use :cl :boots% :boots/terminals)
  (:shadowing-import-from :boots/terminals :paint) ; todo find a less awful way to deal with this 
  (:export :with-ansi-terminal))

(defpackage :boots/terminals/static
  (:use :cl :boots% :boots/terminals)
  (:shadowing-import-from :boots/terminals :paint) ; todo find a less awful way to deal with this
  (:export :with-static-terminal :stringify))


;;;; User-facing API ----------------------------------------------------------
(defpackage :boots
  (:use :cl)
  (:import-from :boots%
    :*screen*

    :attribute :color
    :attr :rgb :default

    :event

    :*border-vertical-char* :*border-horizontal-char*
    :*border-top-left-char* :*border-top-right-char*
    :*border-bottom-left-char* :*border-bottom-right-char*

    :draw :paint

    :make-stack :make-shelf :make-pile :make-canvas
    :stack :shelf :pile :canvas

    :root :children)
  (:export
    :*screen*

    :attribute :color
    :attr :rgb :default

    :event

    :*border-vertical-char* :*border-horizontal-char*
    :*border-top-left-char* :*border-top-right-char*
    :*border-bottom-left-char* :*border-bottom-right-char*
    :with-simple-borders :with-light-borders :with-heavy-borders

    :draw :paint
    :width :height

    :make-stack :make-shelf :make-pile :make-canvas
    :stack :shelf :pile :canvas

    :with-screen

    :root :children

    :redraw
    :read-event
    :read-event-no-hang))
