#lang racket

(require scimitar/contract-utils)
(require scimitar/ty)
(require scimitar/vec)
(require scimitar/round)

(provide
  bit? boolean->bit bit->boolean)

(define (bit? b)
  ((or/c 0 1) (round-scalar b)))

(define/contract (boolean->bit b)
  (-> boolean? (vec/c bit?))
  ((if b vec-1 vec-0) '() bit))

(define/contract (bit->boolean b)
  (-> (vec/c bit?) boolean?)
  (match b
    [(vec-dense b)
     (= 1 (round-scalar b))]))
