#lang racket

(require racket/generic)

(provide
  gen:typeable typeable?
  listlike?
  typeof type-ctor)

(define (listlike? xs)
  (or (pair? xs) (null? xs)))

(define-generics typeable
  (typeof typeable)
  (type-ctor typeable)
  #:fast-defaults
  ([listlike?
    (define typeof (const listlike?))
    (define type-ctor (const list))]
   [vector?
    (define typeof (const vector?))
    (define type-ctor (const vector))]))
