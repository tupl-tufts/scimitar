#lang racket

(require (for-syntax syntax/parse))
(require "grammar.rkt")

(provide
  translate-module)

(define-syntax (translate-decl stx)
  (syntax-parse stx #:datum-literals (source sink)
    [(_ (source x flow))
     #'`(,(syntax->datum #'x) ,(source (syntax->datum #'flow)))]
    [(_ (sink x))
     #'`(,(syntax->datum #'x) ,(sink))]
    [(_ x)
     #'`(,(syntax->datum #'x) ,(junction))]))

(define-syntax (translate-name stx)
  (syntax-parse stx #:datum-literals (source sink)
    [(_ (source x flow))
     #'(syntax->datum #'x)]
    [(_ (sink x))
     #'(syntax->datum #'x)]
    [(_ x)
     #'(syntax->datum #'x)]))

(define-syntax (translate-layout stx)
  (syntax-parse stx #:datum-literals (>== ==>)
    [(_ (input >== flow ==> output))
     #'`(,(translate-decl input)
         ,(translate-decl output)
         (,(gensym 'p) ,(pipe (translate-name input)
                              (translate-name output)
                              (syntax->datum #'flow))))]))

(define-syntax (translate-module stx)
  (syntax-parse stx
    [(_ ss ...)
     #'(append (translate-layout ss) ...)]))
