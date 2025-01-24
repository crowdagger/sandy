(define-library (sandy elements)
  (import (scheme base)
          (scheme write)
          (crow-utils checked)
          (rnrs enums))
  (export element? element->u8 u8->element
          empty? empty-or-liquid?)
  (begin
    ;;; All  possible box elements
    (define _elements '(empty water sand solid))
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
    (define (empty-or-liquid? x)
      "Returns true if x is empty, or liquid"
      (<= x 1))
    ))
