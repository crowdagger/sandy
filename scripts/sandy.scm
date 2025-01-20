(install-r7rs!)

(import (scheme base)
        (chickadee)
        (chickadee graphics path)
        (chickadee graphics text)
        (chickadee graphics color)
        (chickadee math vector)
        (chickadee scripting)
        (sandy sandbox)
        (sandy grid))

(define start-time 0.0)
(define avg-frame-time 16.0)
(define stats-text "Pouet")
(define stats-position (vec2 4.0 100.0))
(define position (vec2 160.0 240.0))
(define (stats-message)
  (format #f "fps: ~1,2f, width: ~a"
          (/ 1.0 avg-frame-time)
          (window-width (current-window))))


(define sandbox (make-sandbox 20 20 1280 720))
(define canvas (make-empty-canvas))

(define (load)
  (script
   (forever
    (sleep 60)
 ;   (set-canvas-painter! canvas (rect-painter))
    (set! stats-text (stats-message)))))

(define (rect-painter)
  (with-style ((fill-color (string->color "#FF0000")))
              (fill
               (rectangle (vec2 100.0 100.0) 1080.0 520.0))))


(define (draw alpha)
  (draw-canvas canvas)
  (let* ([width (window-width (current-window))]
         [height (window-height (current-window))]
         [position (vec2 (/ width 2.0)
                         (/ height 2.0))])
    (draw-text stats-text stats-position)
    (let ((current-time (elapsed-time)))
      (set! avg-frame-time
            (+ (* (- current-time start-time) 0.1)
               (* avg-frame-time 0.9)))
      (set! start-time current-time))))

(define (update dt)
  (display (grid-get-all (sandbox-grid sandbox)))
  (newline)

  (when (mouse-button-pressed? 'left)
    (sandbox-set! sandbox (mouse-x) (mouse-y) 'sand)
    (set-canvas-painter! canvas (sandbox-painter sandbox)))
  (update-agenda 1))

(define (key-press key modifiers repeat?)
  (when (eq? key 'q)
    (abort-game)))


(run-game #:draw draw
          #:key-press key-press
          #:load load
          #:update update
          #:window-title "=(^·^)="
          #:window-width 1280
          #:window-height 720)


