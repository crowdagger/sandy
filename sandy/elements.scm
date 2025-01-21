(define-library (sandy elements)
  (import (scheme base)
          (crow-utils checked)
          (rnrs enums))
  (export element? element->u8 u8->element
          empty?)
  (begin
    ;;; All  possible box elements
    (define _elements '(empty sand))
    (define elements (make-enumeration _elements))
    (define element->u8 (enum-set-indexer elements))
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
    ))
