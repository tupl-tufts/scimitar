#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../val.rkt")

(provide
  cps-cexp?
  cps-fun-entry?
  (struct-out cps-app)
  (struct-out cps-fix)
  (struct-out cps-op)
  (struct-out cps-primop)
  (struct-out cps-switch))

(struct
  cps-cexp ()
  #:transparent)

(struct/contract
  cps-app cps-cexp
  ([f symbol?]
   [args sym-val?])
  #:transparent)

(define cps-fun-entry?
  (list/c symbol? sym? cps-cexp?))

(struct/contract
  cps-fix cps-cexp
  ([fs (listof cps-fun-entry?)]
   [cont cps-cexp?])
  #:transparent)

(struct/contract
  cps-op
  ([dir (or/c 'minimize 'maximize)]
   [obj (listof (list/c symbol? number?))] ;; (+ x*n ...)
   [poly poly?])
  #:transparent)

(struct/contract
  cps-primop cps-cexp
  ([op cps-op?]
   [args sym-val?]
   [out sym?]
   [cont cps-cexp?])
  #:transparent)

(struct/contract
  cps-switch cps-cexp
  ([op (-> ty? poly?)]
   [v sym-val?]
   [conts (non-empty-listof cps-cexp?)])
  #:transparent)
