(defpackage :boots
  (:use :cl :iterate :losh :boots.quickutils)
  (:shadow :fill)
  (:export
    :with-boots

    :with-layer
    :stack
    :shelf
    :canvas

    :read-event
    :read-event-no-hang

    :draw
    :clear
    :fill
    :blit
    :border

    :width
    :height

    :move-cursor

    :*global-input-hook*

    ))
