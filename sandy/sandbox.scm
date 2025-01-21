(define-library (sandy sandbox)
  (import (scheme base)
          (scheme write)
          (chickadee graphics path)
          (chickadee graphics color)
          (chickadee math vector)
          (crow-utils checked)
          (sandy grid)
          (sandy elements)
          (sandy types))
  (export make-sandbox sandbox? sandbox-painter
          sandbox-width sandbox-height
          sandbox-width! sandbox-height!
          sandbox-rows sandbox-cols sandbox-set!
          sandbox-tick!)
  (begin
    ;;; Colors for different elements
    (define c-sand (string->color "#FFFF00"))
    (define c-empty (string->color "#000000"))

    (define (caddr xs)
      (car (cddr xs)))
    
    (define-checked (u8->color [x u8?])
      (case (u8->element x)
        ('empty c-empty)
        ('sand c-sand)
        (else (error "Unsupported element" x))))

    
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

    (define-checked (sandbox-painter [s sandbox?])
      #:doc "Returns a chickadee painter allowing to draw the sandbox content"
      (let ([dx (/ (sandbox-width s) (sandbox-cols s))]
            [dy (/ (sandbox-height s) (sandbox-rows s))])
        (superimpose ;; (with-style ((fill-color (u8->color 0)))
                     ;;             (fill
                     ;;              (rectangle
                     ;;               (vec2 0 0)
                     ;;               (sandbox-width s)
                     ;;               (sandbox-height s))))
                     (apply superimpose
                            (map (lambda (e)
                                   (let* ([element (car e)]
                                          [row (cadr e)]
                                          [col (caddr e)])
                                     (with-style ((fill-color (u8->color element)))
                                                 (fill
                                                  (rectangle (vec2 (* col dx)
                                                                   (* row dy))
                                                             dx
                                                             dy)))))
                                 (grid-get-all (sandbox-data s)))))))
      
    (define-checked (sandbox-set! [s sandbox?]
                                  [x real?]
                                  [y real?]
                                  val)
      #:doc "Add an element val at the position where (x,y) points to"
      ; x and y can be outside the appropriate coordinates
      (when (and
               (<= 0 x (sandbox-width s))
               (<= 0 y (sandbox-height s)))
        (let* ([y (- (sandbox-height s) y)] ; 0 is at bottom 
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
            [rows (sandbox-rows s)]
            [cols (sandbox-cols s)])
        (let lp-rows ([row 0])
          (unless (>= row rows)
            (let lp-col ([col 0])
              (unless (>= col cols)
                (cell-tick grid row col) 
                ))))))

    (define-checked (cell-tick [g grid?]
                               [r posint?]
                               [c posint?])
      #:doc "Update indidual cell (may impact neighbors)"
      ;; In orders for things to not go too bad, we need two
      ;; differents grids : one for live updating, one for checking
      ;; whether a move has already be done on the square this turn.
      ;;
      ;; E.g. if two cells swap positions, these two positions should be marked so as not to be checked twice.
      ;; This should allow for the direct mutation of the grid.

      '() ; todo
      )
    
    (define-checked (sandbox-checked [s sandbox?]
                                     [r posint?]
                                     [c posint?])
      #:doc "Check if a cellule has been moved this step already"
      (if (>= (grid-get (sandbox-delta s) r c))
          #t
          #f))
    ))
  
