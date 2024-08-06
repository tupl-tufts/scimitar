#lang racket

(require scimitar)
(require scimitar/ty)
(require scimitar/vec)

(epsilon 0.01)
(omega 100.0)

(displayln "Simple: Minimal Scimitar")

(letrec ((go (lambda (prev upper)
               (if (equal? prev upper)
                 upper
                 (let ((in-result (optimum-ref in
                                    (maximize (: result (realty)) (in result)
                                      (letrec
                                        ((foo (lambda x
                                                (if (> x 3)
                                                  (if (< x (racket upper))
                                                    (+ x 2)
                                                    3)
                                                  (* 2 x)))))
                                        (assert
                                          (= result
                                             (foo in))))))))
                   (println in-result)
                   (go upper in-result))))))
  (go (scimitar (: 8 (vecty (interval 0 10) '())))
      (scimitar (: 7 (vecty (interval 0 10) '())))))
