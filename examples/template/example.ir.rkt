#lang s-exp scimitar/ir

(require scimitar/ty)
(provide example)

(define
  (example ((: input (type)))
           (: output (type)))
  (locals
    (: var (type)))
  (= input var)
  (<= (+ var output) 0))
