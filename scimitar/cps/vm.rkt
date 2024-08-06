#lang racket

(require "../contract-utils.rkt")
(require "../poly.rkt")
(require "../prim/fun.ir.ast.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "../vm.rkt")
(require "basis.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- run))

(struct closure-base () #:transparent)
(struct/contract
  closure closure-base
  ([rho (box/c (listof (list/c symbol? (or/c val? closure-base?))))]
   [xs sym?]
   [b cps-cexp?])
  #:transparent)

(define (resolve v as)
  (cond
    [(symbol? v)
     (match (assoc v as)
       [`(,_ ,v) v]
       [#f (error (format "During CPS execution: unknown value ~a" v))])]
    [(list? v) (map (curryr resolve as) v)]
    [else v]))

(define (closure-check a)
  (when (closure? a)
    (error "During CPS execution: closures not allowed in primop args"))
  (when (list? a)
    (for-each closure-check a)))

(define (symbol-check rho)
  (map-car (lambda (x)
         (when (not (symbol? x))
           (error (format "During CPS execution: primop out variables ~v outnumber result value positions" x))))
       rho))

;; NOTE: this vm is only for debugging purposes.
(define/contract (run e)
  (-> cps-cexp? val?)
  (define/match (go e rho)
    [((cps-app (? cps-exit?) args) _)
     (resolve args rho)]
    [((cps-app f args) _)
     (let ((args (resolve args rho)))
       (match (resolve f rho)
         [(closure rho xs b)
          (go b (append (tree-preorder-zip xs args)
                        (unbox rho)))]))]
    [((cps-fix fs cont) _)
     (let* ((box-rho (box '()))
            (rho-fs (map (match-lambda
                           [`(,f ,xs ,b)
                            `(,f ,(closure box-rho xs b))])
                         fs))
            (rho (append rho-fs rho)))
       (set-box! box-rho rho)
       (go cont rho))]
    [((cps-primop (cps-op dir obj poly) args out cont) _)
     (let* ((argvals (resolve args rho))
            (_ (closure-check argvals))
            (obj (calculate-obj poly obj))
            (poly (fun-app poly argvals))
            (obj (fun-app-obj-hack (poly-ty poly) obj))
            (op (lp dir obj poly))
            (result (or (solve op) (error "While running, infeasible problem")))
            (rho-r (tree-preorder-zip out result)))
       (symbol-check rho-r)
       (go cont (append rho-r rho)))]
    [((cps-switch op v conts) _)
     (let* ((v (resolve v rho))
            (_ (closure-check v))
            (opts (build-list (length conts) (lambda (i) (vec-dense i))))
            (op (op (natty)))
            (op (lp-default (fun-app op (cons v opts))))
            (branch (vec-unpack (solve op))))
       (go (list-ref conts branch) rho))])
  (go e '()))
