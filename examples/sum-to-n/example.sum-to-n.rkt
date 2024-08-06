#lang racket

(require scimitar)
(require scimitar/ty)

(inline-amount 16)
(epsilon 1e-4)

;(optimum-ref n
;  (minimize n (n)
;    (assert (>= (sum ([i (range (+ (: n (vecty (interval 0 21) '())) 1))]) i) 100))))

(optimum-ref n
  (minimize n (n)
    (letrec
      ((sum-to-n (lambda (n acc)
                   (if (= n 0)
                     acc
                     (let ((n-hack   (: (symbolic) (vecty (interval 0 21) '())))
                           (acc-hack (: (symbolic) (vecty (interval 0 110) '()))))
                       (begin
                         (assert (= n-hack   n))
                         (assert (= acc-hack acc))
                         (sum-to-n (- n-hack 1) (+ n-hack acc-hack))))))))
      (assert (>= (sum-to-n n 0) 100)))))
