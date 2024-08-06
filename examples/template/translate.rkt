#lang racket

(require (for-syntax syntax/parse))
(require "grammar.rkt")

(provide
  translate-module)

(define-syntax (translate-thing stx)
  (syntax-parse stx #:datum-literals ()
    [(_ x)
     #'()]))

(define-syntax (translate-module stx)
  (syntax-parse stx
    [(_ ss ...)
     #'(append (translate-thing ss) ...)]))
