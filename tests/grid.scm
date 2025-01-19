(install-r7rs!)

(import (scheme base)
        (srfi srfi-64)
        (sandy grid))

(test-begin "Grid")
(test-group "get/set"
  (define g (make-grid 3 10))
  (test-assert "Succesful set" (grid-set! g 1 3 'pouet))
  (test-assert "Succesful set" (grid-set! g 2 4 'pouet))
  (test-eqv "OOB set returns false" #f (grid-set g 1 11 'pouet))
  (test-eqv "Set value is set" 'pouet (grid-get g 1 3))
  (test-eqv "Unset value is not set" 'empty (grid-get g 0 3))
  )

(test-end "Grid")
