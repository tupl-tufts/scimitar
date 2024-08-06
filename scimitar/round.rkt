#lang racket

(require "contract-utils.rkt")
(require (only-in "params.rkt" epsilon))
(require "util.rkt")

(provide
  near-integer?
  round-scalar
    )

(define (near-integer? n [tolerance (epsilon)])
  (integer? (rationalize n tolerance)))

(define/contract (round-scalar b [tolerance (epsilon)])
  (->* (near-integer?) (real?) exact-integer?)
  (inexact->exact (rationalize b tolerance)))
