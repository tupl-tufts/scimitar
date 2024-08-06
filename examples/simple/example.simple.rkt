#lang s-exp scimitar

(require scimitar)
(require scimitar/ty)
(require scimitar/vec)

(racket (begin (epsilon 0.01) '()))
(racket (begin (omega 100.0) '()))

(letrec
  ((go (lambda (prev upper)
         (if (= prev upper)
           upper
           (go upper
               (optimum-ref in
                 (maximize (: result (realty)) (in result)
                   (letrec
                     ((foo (lambda x
                             (if (> x 3)
                               (if (< x upper)
                                 (+ x 2)
                                 3)
                               (* 2 x)))))
                     (assert
                       (= result
                          (foo in)))))))))))
  (go (: 8 (vecty (interval 0 10) '()))
      (: 7 (vecty (interval 0 10) '()))))
