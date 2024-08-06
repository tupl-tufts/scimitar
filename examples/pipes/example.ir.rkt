#lang s-exp scimitar/ir

(require scimitar/ty)
(provide net)

(define
  (net () (: sink-e (realty)))
  (locals
    (: p-ab (realty))
    (: p-bc (realty))
    (: p-bd (realty))
    (: p-ce (realty))
    (: p-de (realty)))
  (<= p-ab 12)
  (<= p-ab 10)
  (<= p-bc 5)
  (<= p-bd 5)
  (<= p-ce 4)
  (<= p-de 6)
  (= 0 (- p-ab p-bc p-bd))
  (= 0 (- p-bc p-ce))
  (= 0 (- p-bd p-de))
  (= sink-e (+ p-ce p-de)))
