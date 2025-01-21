(define-library (sandy sandbox)
  (import (scheme base)
          (scheme write)
          (chickadee graphics path)
          (chickadee graphics color)
          (chickadee math vector)
          (crow-utils checked)
          (sandy grid)
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
    
    (define-checked (symbol->color [c symbol?])
      (case c
        ('empty c-empty)
        ('sand c-sand)
        (else (error "Unsupported symbol" c))))

    
    ;;; Sandbox: a grid, plus info on the width/height and so on
    (define-record-type <sandbox>
      (_make-sandbox grid rows cols width height)
      sandbox?
      (grid sandbox-grid)
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
                     rows
                     cols
                     width
                     height))


    (define-checked (sandbox-painter [s sandbox?])
      #:doc "Returns a chickadee painter allowing to draw the sandbox content"
      (let ([dx (/ (sandbox-width s) (sandbox-cols s))]
            [dy (/ (sandbox-height s) (sandbox-rows s))])
        (apply superimpose
               (map (lambda (e)
                      (let* ([element (car e)]
                             [row (cadr e)]
                             [col (caddr e)])
                        (with-style ((fill-color (symbol->color element)))
                             (fill
                              (rectangle (vec2 (* col dx)
                                               (* row dy))
                                         dx
                                         dy)))))
                    (grid-get-all (sandbox-grid s))))))

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
          (grid-set! (sandbox-grid s) r c val))))

    (define-checked (sandbox-tick! [s sandbox?])
      #:doc "Update the content of the sandbox"
      ;; In orders for things to not go too bad, we need two
      ;; differents grids : one for live updating, one for checking
      ;; whether a move has already be done on the square.
      ;;
      ;; E.g. if two cells swap positions, these two positions should be marked so as not to be checked twice.
      ;; This should allow for the direct mutation of the grid.
      (display "TODO")
      (newline))
      ))
  
