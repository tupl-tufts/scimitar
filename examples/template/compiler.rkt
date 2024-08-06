#lang racket

(require scimitar/contract-utils)
(require "grammar.rkt")
(require "semantics.ir.rkt")
(require scimitar/scimitar/grammar)

(provide
  compile-module)

(define/contract (compile-module p)
  (-> (listof (list/c symbol? ____?)) scimitar-expr?)
  )
