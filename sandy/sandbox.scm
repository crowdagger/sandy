(define-library (sandy sandbox)
  (import (scheme base)
          (chickadee graphics path)
          (chickadee graphics color)
          (chickadee math vector)
          (crow-utils checked)
          (sandy grid)
          (sandy types))
  (export make-sandbox sandbox? sandbox-grid sandbox-painter)
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
        (map (lambda (e)
               (let* ([element (car e)]
                      [row (cadr e)]
                      [col (caddr e)])
                 (with-style ((fill-color (symbol->color (cadr e))))
                             (fill
                              (rectangle (vec2 (* col dx)
                                               (* row dy))
                                         dx
                                         dy)))))
             (grid-get-all (sandbox-grid s)))))
    ))
