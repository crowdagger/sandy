(define-library (sandy types)
  (import (scheme base))
  (export any? posint? pos-real?)
  (begin 
    ;;; Some common predicates used elsewhere
    (define (any? x)
      "Returns true whatever x is"
      #t)
    
    (define (posint? x)
      "True iff x is integer and >= 0"
      (and (integer? x) (>= x 0)))

    (define (pos-real? x)
      "True iff is positive real number"
      (and (real? x)
           (>= x 0.0)))
    ))
