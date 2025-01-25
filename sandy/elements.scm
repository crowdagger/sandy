(define-library (sandy elements)
  (import (scheme base)
          (scheme write)
          (crow-utils checked)
          (rnrs enums)
          (sdl2 render))
  (export element? element->u8 u8->element
          empty? empty-or-liquid? liquid?
          set-renderer-draw-u8!)
  (begin
    ;;; All  possible box elements
    (define _elements '(empty water sand solid))
    
    (define elements (make-enumeration _elements))
    
    (define element->u8 (enum-set-indexer elements))

    ;; Common functions for working with elements
    (define-checked (element? (x symbol?))
      (enum-set-member? x elements))
    (define (u8->element n)
      (if (>= n (length _elements))
          (error "Invalid element")
          (list-ref _elements n)))
    
    (define (empty? x)
      "Returns true if x is the empty element"
      (eq? x
           0))
    
    (define (empty-or-liquid? x)
      "Returns true if x is empty, or liquid"
      (<= x 1))
    
    (define (liquid? x)
      (eq? x 1))

    ;;; Colors for different elements
    (define c-sand '(255 255 0 255))
    (define c-water '(0 0 255 255))
    (define c-empty '(0 0 0 255))
    (define c-solid '(128 128 128 255))

    (define-checked (set-renderer-draw-u8! [ren renderer?]
                                           [x u8?])
      #:doc "Set the drawing color of renderer corresponding to x"
      (let ([color (case (u8->element x)
                     ((empty) c-empty)
                     ((sand) c-sand)
                     ((water) c-water)
                     ((solid) c-solid)
                     (else (error "Unsupported element" x)))])
        (apply set-renderer-draw-color!
               (cons ren
                     color))))

    ))
