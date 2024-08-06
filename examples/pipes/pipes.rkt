#lang racket

(require (for-syntax syntax/parse))
(require "compiler.rkt")
(require "translate.rkt")

(provide
  (rename-out [module-body #%module-begin])
  #%top #%app #%datum)

(define-syntax (module-body stx)
  (syntax-parse stx
    [(_ ss ...)
     #`(#%module-begin
        (compile-module
          (translate-module ss ...)))]))
