#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "../elab.rkt")
(require "../env.rkt")
(require "../ty.rkt")

(provide
  cps-exit-sym cps-exit?
  cps-error-sym cps-error?
  cps-basis-define cps-basis-types)

(define cps-exit-sym (string->unreadable-symbol "exit"))
(define cps-error-sym (string->unreadable-symbol "error"))

(define cps-exit? (curryr equal? cps-exit-sym))
(define cps-error? (curryr equal? cps-error-sym))

(define/contract (cps-basis-define p)
  (-> cps-cexp? cps-cexp?)
  (let ((params (gensym 'in)))
    (cps-fix
      ;; This is just to supply valid definitions, which are
      ;; never invoked, since they are special in the run-time.
      `((,cps-exit-sym ,params
                       ,(cps-app cps-exit-sym params))
        (,cps-error-sym ,params
                       ,(cps-app cps-error-sym params))
        ;; Add here
        )
      p)))

(define cps-basis-types
  (env
    `((,cps-exit-sym ,(let ((alpha (fresh-tyvar))) (polyty alpha '() alpha)))
      (,cps-error-sym ,(polyty (vecty (interval 0 255) '(50))  '() (unitty)))
      ;; Add here
      )))
