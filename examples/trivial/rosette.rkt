#lang rosette

(require (except-in scimitar/profile #%app))

(define-symbolic* x real?)

(time-expr problem-definition
 (if x
   (assert (= x 0))
   (assert (<= x 1)))
)

(let ((s (time-expr (optimize
             #:maximize `(,x)
             #:guarantee
             #t
             ))))
  (println (evaluate x s)))
