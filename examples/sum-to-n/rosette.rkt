#lang rosette

(require (only-in scimitar/profile time-expr))

(define-symbolic* n integer?)

(time-expr problem-definition
(let* ()
    (letrec
      ((sum-to-n (lambda (n acc)
                   (if (= n 0)
                     acc
                     (sum-to-n (- n 1) (+ n acc))))))
      (assert (>= (sum-to-n n 0) 100)))
))

(let ((s (time-expr (optimize
             #:maximize `(,n)
             #:guarantee
             #t
             ))))
  (println (evaluate n s)))
