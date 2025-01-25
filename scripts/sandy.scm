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
(define ticking #t)


(define current-element 'sand)
(define latest 0)
(define %min-delay-press 200)
(define last-key-pressed 0)
(define last-mouse-pressed 0)
(define sandbox (make-sandbox 100 100 640 480))
(define refresh-delay 20)
(define last-refresh -1000)
(sandbox-oob-element! sandbox 'solid)

(define (draw ren)
  (display (sandbox-number-elements sandbox 'wood))
  (display " wood")
  (newline)
  (let-values ([(width height) (window-size window)])
    (sandbox-width! sandbox width)
    (sandbox-height! sandbox height)
    (set-renderer-draw-color! ren 30 30 30 255)
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

    (when ticking
      (when (> current (+ last-refresh refresh-delay))
        (set! last-refresh current)
        (sandbox-tick! sandbox)))

    (when (key-pressed? 1)
      (when (> current (+ %min-delay-press last-key-pressed))
        (set! last-key-pressed current)
        (set! current-element 'sand)
        (display "Set to sand\n")))

    (when (key-pressed? 2)
      (when (> current (+ %min-delay-press last-key-pressed))
        (set! last-key-pressed current)
        (set! current-element 'water)
        (display "Set to water\n")))

    (when (key-pressed? 3)
      (when (> current (+ %min-delay-press last-key-pressed))
        (set! last-key-pressed current)
        (set! current-element 'solid)
        (display "Set to solid\n")))

    (when (key-pressed? 4)
      (when (> current (+ %min-delay-press last-key-pressed))
        (set! last-key-pressed current)
        (set! current-element 'wood)
        (display "Set to wood\n")))

    (when (key-pressed? 0)
      (when (> current (+ %min-delay-press last-key-pressed))
        (set! last-key-pressed current)
        (set! current-element 'empty)
        (display "Set to empty\n")))
    
    (when (mouse-button-pressed? 'right)
      (when (> current (+ %min-delay-press last-mouse-pressed))
        (set! last-mouse-pressed current)        
        (display "Pause/play")
        (newline)
        (set! ticking (not ticking))))
    
    (when (mouse-button-pressed? 'left)
    (sandbox-set! sandbox
                  (mouse-x)
                  (mouse-y)
                  (element->u8 current-element)))
    ))

(sdl-quit)
