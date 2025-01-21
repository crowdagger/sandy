(define-library (sandy grid)
  (import (scheme base)
          (srfi srfi-43)
          (crow-utils checked)
          (sandy types))
  (export make-grid grid-cols grid-rows grid?
          grid-empty grid-get grid-set!
          grid-get-all grid-mapper-inverse)
  (begin
    ;;; A 2D vector
    (define-record-type <grid>
      (_make-grid r c v f f-1)
      grid?
      (c grid-cols)
      (r grid-rows)
      (v grid-vector)
      (f grid-mapper)
      (f-1 grid-mapper-inverse))

    (define-checked (make-grid rows cols)
      #:doc "Create a 2D grid"
      (let* ([v (make-vector (* rows cols) 'empty)]
             [f (lambda-checked ([row posint?]
                                 [col posint?])
                  (+ (* row cols) col))]
             [f-1 (lambda-checked ([i posint?])
                    (floor/ i cols))])
        (_make-grid rows cols v f f-1)))

    (define-checked (grid-get [g grid?] [row integer?] [col integer?])
      #:doc "Get element at (row, col)"
      (if (or (< row 0)
              (< col 0)
              (>= row (grid-rows g))
              (>= col (grid-cols g)))
          'empty ; return empty if OOB
          (let* ([f (grid-mapper g)]
                 [idx (f row col)]
                 [v (grid-vector g)])
            (vector-ref v idx))))
    
    (define-checked (grid-set! [g grid?] [row integer?] [col integer?] [val any?])
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
            (vector-set! v idx val)
            #t)))
    
    (define-checked (grid-empty [g grid?])
      #:doc "Returns an empty copy of g"
      (let* ([r (grid-rows g)]
            [c (grid-cols g)]
            [v (make-vector (* r c) 'empty)]
            [f (grid-mapper g)]
            [f-1 (grid-mapper-inverse g)])
        (_make-grid r c v f f-1)))

    (define-checked (grid-get-all [g grid?])
      "Returns all non empty elements in g, under the form of a list containing (value row col)"
      (let* ([f-1 (grid-mapper-inverse g)]
             [f (lambda (n curr val)
                 (if (eq? val 'empty)
                     curr
                     (let-values ([[r c] (f-1 n)])
                       (cons (list val r c) curr))))])
        (vector-fold f '() (grid-vector g))))
    ))

