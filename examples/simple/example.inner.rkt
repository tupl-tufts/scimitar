#lang s-exp scimitar

(require scimitar)
(require scimitar/ty)
(require scimitar/vec)

(racket (begin (epsilon 0.01) '()))
(racket (begin (omega 100.0) '()))

(optimum-ref (in result)
  (maximize (: result (realty)) (in result)
    (letrec
      ((foo (lambda x
              (if (> x 3)
                (if (< x (: 7 (vecty (interval 0 10) '())))
                  (+ x 2)
                  3)
                (* 2 x)))))
      (assert
        (= result
           (foo in))))))
