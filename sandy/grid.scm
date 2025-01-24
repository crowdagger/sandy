(define-library (sandy grid)
  (import (scheme base)
          (srfi srfi-43)
          (crow-utils checked)
          (sandy types))
  (export make-grid grid-cols grid-rows grid?
          grid-empty grid-empty! grid-get grid-set!
          grid-get-all grid-mapper-inverse
          grid-default grid-set-default!)
  (begin
    ;;; A 2D grid
    (define-record-type <grid>
      (_make-grid r c v f d)
      grid?
      (c grid-cols)
      (r grid-rows)
      (v grid-vector)
      (f grid-mapper)
      (d grid-default grid-set-default!))

    (define-checked (make-grid rows cols)
      #:doc "Create a 2D grid"
      (let* ([v (make-bytevector (* rows cols) 0)]
             [f (lambda-checked ([row posint?]
                                 [col posint?])
                  (+ (* row cols) col))])
        (_make-grid rows cols v f 0)))

    (define-checked (grid-get [g grid?] [row integer?] [col integer?])
      #:doc "Get element at (row, col)"
      (if (or (< row 0)
              (< col 0)
              (>= row (grid-rows g))
              (>= col (grid-cols g)))
          (grid-default g) ; return default if OOB
          (let* ([f (grid-mapper g)]
                 [idx (f row col)]
                 [v (grid-vector g)])
            (bytevector-u8-ref v idx))))
    
    (define-checked (grid-set! [g grid?] [row integer?] [col integer?] [val u8?])
      #:doc "Set element at (row, col)
Returns #f if (row col) was OOB"
      (if (or (< row 0)
              (< col 0)
              (>= row (grid-rows g))
              (>= col (grid-cols g)))
          #f
          (let* ([f (grid-mapper g)]
                 [idx (f row col)]
                 [v (grid-vector g)])
            (bytevector-u8-set! v idx val)
            #t)))
    
    (define-checked (grid-empty [g grid?])
      #:doc "Returns an empty copy of g"
      (let* ([r (grid-rows g)]
            [c (grid-cols g)]
            [v (make-bytevector (* r c) 0)]
            [f (grid-mapper g)])
        (_make-grid r c v f 0)))

    (define-checked (grid-empty! [g grid?])
      #:doc "Reset the grid to 0"
      (let* ([v (grid-vector g)]
            [n (bytevector-length v)])
        (let lp ([x 0])
          (when (< x n)
            (bytevector-u8-set! v x 0)
            (lp (+ 1 x))))))

    (define-checked (grid-get-all [g grid?])
      "Returns all non null elements in g, under the form of a list containing (value row col)"
      (define rows (grid-rows g))
      (define cols (grid-cols g))

      ;; Each value of the list
      (define (f r c acc)
        (let ([val (grid-get g r c)])
          (if (eq? val 0)
              acc
              (cons (list val r c) acc))))

      (let lp-rows ([row 0]
                    [acc '()])
        (if (>= row rows)
            acc
            (lp-rows (+ row 1)
                     (let lp-cols ([col 0]
                                   [acc acc])
                       (if (>= col cols)
                           acc
                           (lp-cols (+ col 1) (f row col acc))))))))
    ))

