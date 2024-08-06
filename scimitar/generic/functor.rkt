#lang racket/base

(require (prefix-in base- racket/base))
(require (prefix-in base- racket/vector))
(require racket/generic)
(require "typeable.rkt")

(provide
  gen:functor functor? fmap)

(define-generics functor
  (fmap f functor)
  #:fast-defaults
  ([listlike?
    (define fmap base-map)]
   [base-vector?
    (define fmap base-vector-map)]))
