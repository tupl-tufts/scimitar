#lang s-exp scimitar/ir

(require scimitar/ty)
(provide trivial)

(define
  (trivial () (: x (realty)))
  (locals
    (: dt (realty))
    (: xt (realty))
    (: df (realty))
    (: xf (realty))
    (: ite (bitty)))
  (= ite (+ (* x 0) (* (- 1 x) 1)))
  (= xt (+ (* ite dt) (* (- 1 ite) x)))
  (= xt 0)
  (= xf (+ (* ite x) (* (- 1 ite) df)))
  (<= xf 1))
