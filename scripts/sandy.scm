;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of guile-sdl2.
;;;
;;; Guile-sdl2 is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-sdl2 is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-sdl2.  If not, see
;;; <http://www.gnu.org/licenses/>.

(import (scheme base)
        (srfi srfi-11)
        (sdl2)
        (sdl2 events)
        (sdl2 rect)
        (sdl2 render)
        (sdl2 input keyboard)
        (sdl2 input mouse)
        (sdl2 video)
        (sandy sandbox)
        (sandy elements)
        (sandy grid))


(sdl-init)

(define window (make-window))
(define renderer (make-renderer window))
(define running #t)


(define latest 0)
(define sandbox (make-sandbox 100 100 640 480))

(define (draw ren)
  (let-values ([(width height) (window-size window)])
    (sandbox-width! sandbox width)
    (sandbox-height! sandbox height)
    (set-renderer-draw-color! ren 0 0 100 255)
    (clear-renderer ren)
    (sandbox-draw! sandbox ren)
    (present-renderer ren)
    )
  )

(set-window-size! window 1000 1000)

(while running
  (let* ([current (sdl-ticks)]
         [delta (- current latest)])
    (set! latest current)
    (display delta)
    (newline)
    

    (draw renderer)
    (usleep 250)
    (let lp ([event (poll-event)])
      (when event
        (cond
         ((quit-event? event)
          (set! running #f)))
        (lp (poll-event))))
    
    (sandbox-tick! sandbox)
    (when (mouse-button-pressed? 'left)
    (sandbox-set! sandbox (mouse-x) (mouse-y) (element->u8 'sand)))
    ))

(sdl-quit)
