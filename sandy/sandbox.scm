(define-library (sandy sandbox)
  (import (scheme base)
          (scheme write)
          (srfi srfi-27)
          (sdl2)
          (sdl2 video)
          (sdl2 render)
          (sdl2 rect)
          (crow-utils checked)
          (sandy grid)
          (sandy elements)
          (sandy types))
  (export make-sandbox sandbox? sandbox-draw!
          sandbox-data
          sandbox-width sandbox-height
          sandbox-width! sandbox-height!
          sandbox-rows sandbox-cols sandbox-set!
          sandbox-tick! sandbox-oob-element!)
  (begin
    ;;; Colors for different elements
    (define c-sand '(255 255 0 255))
    (define c-water '(0 0 255 255))
    (define c-empty '(0 0 0 255))
    (define c-solid '(128 128 128 255))

    (define (caddr xs)
      (car (cddr xs)))
    
    (define-checked (set-renderer-draw-u8! [ren renderer?]
                                           [x u8?])
      (let ([color (case (u8->element x)
                     ((empty) c-empty)
                     ((sand) c-sand)
                     ((water) c-water)
                     ((solid) c-solid)
                     (else (error "Unsupported element" x)))])
        (apply set-renderer-draw-color!
               (cons ren
                     color))))

    
    ;;; Sandbox: a grid, plus info on the width/height and so on
    (define-record-type <sandbox>
      (_make-sandbox elements delta time rows cols width height)
      sandbox?
      (elements sandbox-data)
      (delta sandbox-delta)
      (time sandbox-time sandbox-time!)
      (rows sandbox-rows)
      (cols sandbox-cols)
      (width sandbox-width sandbox-width!)
      (height sandbox-height sandbox-height!))

    (define-checked (make-sandbox [rows posint?]
                                  [cols posint?]
                                  [width pos-real?]
                                  [height pos-real?])
      #:doc "Creates a new, empty sandbox"
      (_make-sandbox (make-grid rows cols)
                     (make-grid rows cols)
                     0
                     rows
                     cols
                     width
                     height))

    (define-checked (sandbox-draw! [s sandbox?]
                                   [ren renderer?])
      #:doc "Returns a chickadee painter allowing to draw the sandbox content"
      (let ([dx (/ (sandbox-width s) (sandbox-cols s))]
            [dy (/ (sandbox-height s) (sandbox-rows s))])
        (for-each (lambda (e)
                    (let* ([element (car e)]
                           [row (cadr e)]
                           [col (caddr e)])
                      (set-renderer-draw-u8! ren element)
                      (fill-rect ren (make-rect (round (* col dx))
                                                (round (* row dy))
                                                (round dx)
                                                (round dy)))))
                  (grid-get-all (sandbox-data s)))))

    (define-checked (sandbox-oob-element! [s sandbox?]
                                          [e element?])
      #:doc "Sets the element returned when oob"
      (grid-set-default! (sandbox-data s)
                         (element->u8 e)))
    
    (define-checked (sandbox-set! [s sandbox?]
                                  [x real?]
                                  [y real?]
                                  val)
      #:doc "Add an element val at the position where (x,y) points to"
      ; x and y can be outside the appropriate coordinates
      (when (and
               (<= 0 x (sandbox-width s))
               (<= 0 y (sandbox-height s)))
        (let* (;[y (- (sandbox-height s) y)] ; 0 is at bottom 
             [dx (/ (sandbox-width s)
                    (sandbox-cols s))]
             [dy (/ (sandbox-height s)
                    (sandbox-rows s))]
             [c (floor (/ x dx))]
             [r (floor (/ y dy))])
          (grid-set! (sandbox-data s) r c val))))

    (define-checked (sandbox-tick! [s sandbox?])
      #:doc "Update the content of the sandbox"
      ;; First, update time and maybe reset delta
      (sandbox-time! s (+ 1 (sandbox-time s)))
      (when (> (sandbox-time s) 255)
          (sandbox-time! s 0)
          (grid-empty! (sandbox-delta s)))

      (let ([grid (sandbox-data s)]
            [delta (sandbox-delta s)]
            [rows (sandbox-rows s)]
            [cols (sandbox-cols s)])
        (let lp-row ([row 0])
          (unless (>= row rows)
              (let lp-col ([col 0])
              (unless (>= col cols)
                (cell-tick! s row col)
                (lp-col (+ 1 col))
                ))
              (lp-row (+ 1 row))))))

    (define-checked (cell-tick! [s sandbox?]
                                [r posint?]
                                [c posint?])
      #:doc "Update indidual cell (may impact neighbors)"
      ;; In orders for things to not go too bad, we need two
      ;; differents grids : one for live updating, one for checking
      ;; whether a move has already be done on the square this turn.
      ;;
      ;; E.g. if two cells swap positions, these two positions should be marked so as not to be checked twice.
      ;; This should allow for the direct mutation of the grid.

      (let* ([g (sandbox-data s)]
             [val (grid-get g r c)])
        (case (u8->element val)
          ([empty] '()) ; do nothing for empty cell
          ([sand] (tick-sand! g s val r c))
          ([water] (tick-water! g s val r c))
          )))

    (define-checked (try-swap! [g grid?]
                               [s sandbox?]
                               [r1 integer?]
                               [c1 integer?]
                               [r2 integer?]
                               [c2 integer?])
      #:doc "Try swapping two cells, return #f if failure."
      (let ([val1 (grid-get g r1 c1)]
            [val2 (grid-get g r2 c2)]
            [rows (sandbox-rows s)]
            [cols (sandbox-cols s)])
        ; OOB is considered filled
        (if (or (<= rows r1 -1)
                (<= rows r2 -1)
                (<= cols c1 -1)
                (<= cols c2 -1))
            #f
         (if (and
                (sandbox-check? s r1 c1)
                (sandbox-check? s r2 c2)) 
              (begin
                (grid-set! g r1 c1 val2)
                (grid-set! g r2 c2 val1)
                (sandbox-check! s r1 c1)
                (sandbox-check! s r2 c2)
               #t)
              #f))))

    (define-checked (tick-sand! [g grid?]
                                [s sandbox?]
                                [val u8?]
                                [r posint?]
                                [c posint?])
      #:doc "Update cell sand"
      (let* ([down (+ r 1)]
             [dir (if (= 0 (random-integer 2))
                      1
                      -1)]
             [left (- c dir)]
             [right (+ c dir)])
        (or
         ;; If cell below is empty, fall
         (and (empty-or-liquid? (grid-get g down c))
              (try-swap! g s r c down c))
         ;; Else, try falling to the left
         (and (empty-or-liquid? (grid-get g down left))
              (try-swap! g s r c down left))
         (and (empty-or-liquid? (grid-get g down right))
              (try-swap! g s r c down right))
         )))

    (define-checked (tick-water! [g grid?]
                                 [s sandbox?]
                                 [val u8?]
                                 [r posint?]
                                 [c posint?])
      #:doc "Update water cell"
      (let* ([down (+ r 1)]
             [dir (if (= 0 (random-integer 2))
                      1
                      -1)]
             [left (- c dir)]
             [right (+ c dir)])
        (or
         ;; If cell below is empty, fall
         (and (empty? (grid-get g down c))
              (try-swap! g s r c down c))
         ;; Else, try falling to the left
         (and (empty? (grid-get g down left))
              (try-swap! g s r c down left))
         (and (empty? (grid-get g down right))
              (try-swap! g s r c down right))
         ;; Else, else try moving to the side
         (and (empty? (grid-get g r left))
              (try-swap! g s r c r left))
         (and (empty? (grid-get g r right))
              (try-swap! g s r c r right))
         )))


    
    (define-checked (sandbox-check? [s sandbox?]
                                    [r integer?]
                                    [c integer?])
      #:doc "Check if a cellule has been moved this step already: return true if it is ok (if it has NOT been moved)"
      (if (>= (grid-get (sandbox-delta s) r c)
              (sandbox-time s))
          #f
          #t))

    (define-checked (sandbox-check! [s sandbox?]
                                    [r integer?]
                                    [c integer?])
      #:doc "Mark a cell has having been moved this step"
      (grid-set! (sandbox-delta s)
                 r
                 c
                 (sandbox-time s)))
                                       
    ))
  
