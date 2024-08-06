#lang racket

(require "contract-utils.rkt")
(require (only-in "cps/util.rkt" fun-app-obj-hack))
(require (only-in "cps/basis.rkt" cps-exit? cps-error?))
(require (only-in "poly.rkt" poly-ty))
(require (only-in "round.rkt" round-scalar))
(require (only-in "ty.rkt" polyty-dom ty-dim))
(require (only-in "val.rkt" val? vec->val))
(require (only-in "vec.rkt" vec! vec-project vec->text))
(require "vm.rkt")
(require (only-in "prim/fun.ir.ast.rkt" fun-app))

(provide optomaton-vm)

(define/contract (optomaton-vm p [fail error])
  (->* ((list/c (listof (list/c number? symbol? lp?)) lp?)) (procedure?) val?)
  (define fs (first p))
  (define (go body)
    (match (solve body)
      [#f (fail "While running, infeasible problem")]
      [result
       (let* ((addr (round-scalar (vec! (car result) '())))
              (args (cadr result)))
         (match (assoc addr fs)
           [`(,_ ,(? cps-exit?) ,body)
            (let* ((ty-i (polyty-dom (poly-ty (lp-poly body))))
                   (args (vec-project args '(0) `(,(ty-dim ty-i)))))
              (vec->val ty-i args))]
           [`(,_ ,(? cps-error?) ,body)
            (let* ((ty-i (polyty-dom (poly-ty (lp-poly body))))
                   (args (vec-project args '(0) `(,(ty-dim ty-i)))))
              (error (vec->text args)))]
           [`(,_ ,f ,body)
            (let* ((ty-i (polyty-dom (poly-ty (lp-poly body))))
                   (args (vec-project args '(0) `(,(ty-dim ty-i))))
                   (args (vec->val ty-i args))
                   (poly (fun-app (lp-poly body) args))
                   (obj (fun-app-obj-hack (poly-ty poly) (lp-objective body))))
              (go (lp (lp-direction body) obj poly)))]
           [#f (error (format "While running, function ~a not found" addr))]))]))
  (go (second p)))
