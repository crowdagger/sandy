(define-library (sandy types)
  (import (scheme base))
  (export any? posint? pos-real? u8?)
  (begin 
    ;;; Some common predicates used elsewhere
    (define (any? x)
      "Returns true whatever x is"
      #t)

    (define (u8? x)
      "Returns true iff x is a valid u8"
      (and (integer? x)
           (<= 0 x 255)))
    
    (define (posint? x)
      "True iff x is integer and >= 0"
      (and (integer? x) (>= x 0)))

    (define (pos-real? x)
      "True iff is positive real number"
      (and (real? x)
           (>= x 0.0)))
    ))
