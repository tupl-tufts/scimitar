#lang racket

(require scimitar/contract-utils)
(require scimitar/vec)
(require scimitar/ty)
(require "ba-helpers.rkt")

(provide
  bitwidth
  bv? int->bv bv->int gen-all-bvs)

;; The number of bits used to construct bitvectors
(define bitwidth
  (make-parameter 4
    (invariant-assertion
      (-> natural? natural?)
      identity)))

(define bv? (vecof bit?))

(define/contract (int->bv n [k (bitwidth)])
  (->* (natural?) (natural?) bv?)
  (vec-dense (build-list k (lambda (d)
    (quotient (modulo n (expt 2 (add1 d)))
              (expt 2 d))))
    (bitty k)))

(define/contract (bv-coord->int i b)
  (-> (list/c natural?) bit? natural?)
  (* b (expt 2 (car i))))

(define/contract (bv->int bv)
  (-> bv? natural?)
  (match (round-vector bv)
    [(vec-sparse cs)
     (apply + (map (curry apply bv-coord->int) cs))]))

(define/contract (gen-all-bvs [k (bitwidth)])
  (->* () (natural?) (listof (vecof bit?)))
  (build-list (expt 2 k) (curryr int->bv k)))
