(install-r7rs!)

(define-library (sandy grid)
  (import (scheme base)
          (srfi srfi-43))
  (export check-ints check-posints
          make-grid grid-cols grid-rows grid?
          grid-empty grid-get grid-set!
          grid-get-all grid-mapper-inverse)
  (begin
    (define (check-ints i j)
      "Check that both numbers are integers. 
If not, sends an error."
      (unless
          (and (integer? i)
               (integer? j))
        (error "Indices shoud be integers" i j))
      #t)

    (define (check-posints i j)
      "Check that two numbers are positive"
      (unless
          (and (check-ints i j)
               (>= i 0)
               (>= j 0))
        (error "Indices shoul be positive" i j)))


    
    ;;; A 2D vector
    (define-record-type <grid>
      (_make-grid r c v f f-1)
      grid?
      (c grid-cols)
      (r grid-rows)
      (v grid-inner)
      (f grid-mapper)
      (f-1 grid-mapper-inverse))

    (define (make-grid rows cols)
      "Create a 2D grid"
      (check-posints rows cols)
      (let* ([v (make-vector (* rows cols) 'empty)]
             [f (lambda (row col)
                  (check-posints row col)
                  (+ (* row cols) col))]
             [f-1 (lambda (i)
                    (check-posints i 0)
                    (floor/ i cols))])
        (_make-grid rows cols v f f-1)))

    (define (grid-get g row col)
      "Get element at (row, col)"
      (unless (grid? g)
        (error "Must be a grid" g))
      (check-ints row col)
      (if (or (< row 0)
              (< col 0)
              (>= row (grid-rows g))
              (>= col (grid-cols g)))
          'empty ; return empty if OOB
          (let* ([f (grid-mapper g)]
                 [idx (f row col)]
                 [v (grid-inner g)])
            (vector-ref v idx))))
    
    (define (grid-set! g row col val)
      "Set element at (row, col)
Returns #f if (row col) was OOB"
      (unless (grid? g)
        (error "Must be a grid" g))
      (check-ints row col)
      (if (or (< row 0)
              (< col 0)
              (>= row (grid-rows g))
              (>= col (grid-cols g)))
          #f
          (let* ([f (grid-mapper g)]
                 [idx (f row col)]
                 [v (grid-inner g)])
            (vector-set! v idx val)
            #t)))
    
    (define (grid-empty g)
      "Returns an empty copy of g"
      (unless (grid? g)
        (error "Must be a grid" g))
      (let* ([r (grid-rows g)]
            [c (grid-cols g)]
            [v (make-vector (* r c) 'empty)]
            [f (grid-mapper g)]
            [f-1 (grid-mapper-inverse g)])
        (_make-grid r c v f f-1)))

    (define (grid-get-all g)
      "Returns all non empty elements in g, under the form of a list containing (value row col)"
      (unless (grid? g)
        (error "Must be a grid" g))
      (let* ([f-1 (grid-mapper-inverse g)]
             [f (lambda (n curr val)
                 (if (eq? val 'empty)
                     curr
                     (let-values ([[r c] (f-1 n)])
                       (cons (list val r c) curr))))])
        (vector-fold f '() (grid-inner g))))
    ))

