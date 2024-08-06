#lang racket

(require (for-syntax syntax/parse))

(provide
  (rename-out [module-body #%module-begin])
  #%top #%app #%datum)

(module inner racket/base
(provide run)
(require "compiler.rkt")
(require "elab.rkt")
(require "translate.rkt")
(require "semantics/ba-helpers.rkt")
(require "semantics/bv-helpers.rkt")
(require (except-in scimitar #%module-begin))
(require scimitar/scimitar/compiler)
(require scimitar/optomaton)
(require scimitar/cps)
(require scimitar/env)
(require scimitar/util)
(require scimitar/val)
(require (only-in scimitar/profile time-fun))

(define (convert-val v)
  (cond
    [(equal? (val-ty v) boolty)
     (bit->boolean v)]
    [(equal? (val-ty v) naturalty)
     (bv->int v)]))

(define-syntax-rule (run ss ...)
  (let* ((p (imp-block-translate ss ...))
         (Gamma ((time-fun imp-block-elab) p))
         (p ((time-fun imp-compile) p Gamma))
         (p ((time-fun scimitar-compile) p))
         (p ((time-fun cps-compile) p))
         (result ((time-fun optomaton-vm) p))
         (result (if (list? result) result (list result)))
         (result (map convert-val result)))
    (env (zip (env-dom Gamma) result)
         (or/c integer? boolean?))))
) (require 'inner)

(define-syntax (module-body stx)
  (syntax-parse stx
    [(_ ss ...)
     #`(#%module-begin
        (run ss ...))]))
