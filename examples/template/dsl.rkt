#lang racket

(require (for-syntax syntax/parse))
(require "compiler.rkt")
(require "translate.rkt")
(require scimitar)
(require scimitar/scimitar/grammar)
(require scimitar/cps)

(provide
  (rename-out [module-body #%module-begin])
  #%top #%app #%datum)

(define-syntax-rule (run ss ...)
  (let* ((p (translate-module ss ...))
         (p (compile-module p))
         (p (scimitar-compile p))
         (p (cps-compile p))
         (result (scimitar-run p)))
    result))

(define-syntax (module-body stx)
  (syntax-parse stx
    [(_ ss ...)
     #`(#%module-begin
        (run ss ...))]))
