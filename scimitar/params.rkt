#lang racket

;; The laziness can be either 'set or 'get depending
;; on when you want the dynamic linking to occur.
(define solver-laziness
  (make-parameter 'set))

;; The number of times a loop or recursion will be
;; inlined in the body of a solve expression.
(define inline-amount
  (make-parameter 16
    (invariant-assertion
      (-> (and/c integer? positive?) (and/c integer? positive?))
      identity)))

;; A small number that is larger than the error we're willing to tolerate
(define epsilon
  (make-parameter 1e-6
    (invariant-assertion
      (-> positive? positive?)
      identity)))

;; A large number that doesn't cause the solver to lose precision
(define omega
  (make-parameter 1000000.0
    (invariant-assertion
      (-> positive? positive?)
      identity)))

;; When true, print the solver's complete solution to stderr
(define debug
  (make-parameter #f
    (invariant-assertion
      (-> boolean? boolean?)
      identity)))

;; Enable solver preprocessing
(define preprocess
  (make-parameter #f
    (invariant-assertion
      (-> boolean? boolean?)
      identity)))

(provide
  solver-laziness
  inline-amount epsilon omega
  debug preprocess)
