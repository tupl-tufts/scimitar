#lang racket/base

(require (for-syntax racket/base))
(require (prefix-in base- racket/base))
(require (prefix-in base- racket/vector))
(require (only-in "../util.rkt" equal/c))
(require "typeable.rkt")
(require racket/generic)
(require racket/match)

(provide
  gen:monoid monoid?
  make-append
  unknown-container unknown-container?
  null append)

(struct unknown ())
(struct unknown-null unknown () #:transparent)
(struct unknown-container unknown (vals))

(define the-only-null (unknown-null))

(define-match-expander null
  (lambda (stx)
    (syntax-case stx (null) 
      [(null) #'(? (equal/c the-only-null))]))
  (lambda (stx)
    (syntax-case stx (null)
      [(null) #'the-only-null])))

(define (cast-from-unknown a b)
  (if ((typeof b) a) a (apply (type-ctor b) a)))

(define/match (append-unknown-container xs ys)
  [(xs (unknown-null)) xs]
  [((unknown-container xs) (unknown-container ys))
   (unknown-container (append xs ys))]
  [((unknown-container xs) ys)
   (append (cast-from-unknown xs ys) ys)])

(define ((make-append do-append) xs ys)
  (match ys
    [(unknown-null) xs]
    [(unknown-container ys)
     (do-append xs (cast-from-unknown ys xs))]
    [else (do-append xs ys)]))

(define (append-unknown-null _ ys) ys)

(define-generics monoid
  (append monoid monoid2)
  #:fast-defaults
  ([listlike?
    (define append (make-append base-append))]
   [base-vector?
    (define append (make-append base-vector-append))]
   [unknown-null?
    (define append append-unknown-null)]
   [unknown-container?
    (define append append-unknown-container)]))
