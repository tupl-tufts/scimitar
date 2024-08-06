#lang racket/base

(require (for-syntax racket/base))
(require (prefix-in base- racket/base))
(require (prefix-in base- racket/function))
(require (prefix-in base- racket/list))
(require (prefix-in base- racket/vector))
(require (only-in "../util.rkt" [snoc base-snoc] equal/c))
(require "monoid.rkt")
(require "foldable.rkt")
(require "typeable.rkt")
(require racket/generic)
(require racket/match)

(provide
  gen:ordered-container
  ordered-container?
  cons snoc first rest map)

(define (null-cons v _)
  (unknown-container `(,v)))

(define (null-snoc _ v)
  (unknown-container `(,v)))

(define (null-map f . vs)
  (if (andmap null? vs)
    (null)
    (apply base-map f vs)))

(define/match (container-cons v vs)
  [(v (unknown-container vs))
   (unknown-container (base-cons v vs))])
(define/match (container-snoc vs v)
  [((unknown-container vs) v)
   (unknown-container (base-snoc vs v))])
(define/match (container-first vs)
  [((unknown-container `(,v . ,_))) v])
(define/match (container-rest vs)
  [((unknown-container `(,_ . ,vs)))
   (if (base-null? vs)
     (null)
     (unknown-container vs))])
(define/match (container-map f vs . vss)
  [(f (unknown-container vs) (list (unknown-container vss) ...))
   (unknown-container (apply base-map f vs vss))])

(define-generics ordered-container
  (cons val ordered-container)
  (snoc ordered-container val)
  (first ordered-container)
  (rest ordered-container)
  (map fun ordered-container . more)
  #:fast-defaults
  ([listlike?
    (define cons base-cons)
    (define snoc base-snoc)
    (define first base-car)
    (define rest base-cdr)
    (define map base-map)]
   [base-vector?
    (define (cons v vs) (base-vector-append (vector v) vs))
    (define (snoc vs v) (base-vector-append vs (vector v)))
    (define (first xs) (base-vector-ref xs 0))
    (define (rest xs) (base-vector-drop xs 1))
    (define map base-vector-map)]
   [(equal/c (null))
    (define cons null-cons)
    (define snoc null-snoc)
    (define first base-car)
    (define rest base-cdr)
    (define map null-map)]
   [unknown-container?
    (define cons container-cons)
    (define snoc container-snoc)
    (define first container-first)
    (define rest container-rest)
    (define map container-map)]))
