#lang s-exp scimitar

(require scimitar/params)

; The solver runs into numerical issues
; so we have to tuck in the bounds.
(racket (begin (omega 100) '()))
(racket (begin (epsilon 0.001) '()))

(optimum-ref x
  (maximize x (x)
    (if x
      (assert (= x 0))
      (assert (<= x 1)))))
