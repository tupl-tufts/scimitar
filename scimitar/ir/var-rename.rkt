#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../util.rkt")
(require "error.rkt")
(require "grammar.rkt")

(provide
  var-env? env->fresh-var-env
  ir-dec-var-rename)

(define var-env?
  (envof symbol?))

(define/contract (env->fresh-var-env Gamma)
  (-> env? var-env?)
  (env-map (lambda (x _) `(,x ,(gensym x)))
           Gamma symbol?))

(define/contract (env-rename omicron Gamma)
  (-> env? env? env?)
  (env-map (lambda (k v)
             (match (env-assoc k omicron)
               [`(,_ ,x) `(,x ,v)]
               [#f `(,k ,v)]))
           Gamma))

(define/contract (ir-stmt-renamed st1 st2)
  (-> ir-stmt? ir-stmt? env?)
  (env (zip (env-dom (ir-subject-to-Gamma_l st1))
            (env-dom (ir-subject-to-Gamma_l st2)))
       symbol?))

(define/contract (ir-expr-var-rename e omicron)
  (-> ir-expr? var-env? ir-expr?)
  (match e
    [(? ir-val?) e]
    [(ir-var x)
     (match (env-assoc x omicron)
       [(list _ y) (ir-var y)]
       [#f e])]
    [(ir-add e1 e2)
     (ir-add (ir-expr-var-rename e1 omicron) (ir-expr-var-rename e2 omicron))]
    [(ir-scmul m e)
     (ir-scmul m (ir-expr-var-rename e omicron))]
    [(ir-var-mul e1 e2)
     (ir-var-mul (ir-expr-var-rename e1 omicron) (ir-expr-var-rename e2 omicron))]
    [(ir-tuple es)
     (ir-tuple (map (curryr ir-expr-var-rename omicron) es))]
    [(ir-vec-ix e ix)
     (ir-vec-ix (ir-expr-var-rename e omicron) (ir-expr-var-rename ix omicron))]
    [(ir-cast e ty)
     (ir-cast (ir-expr-var-rename e omicron) ty)]
    [(ir-poly-app f x)
     (ir-poly-app (ir-expr-var-rename f omicron) (ir-expr-var-rename  x omicron))]
    [(ir-thunk e xs f)
     (ir-thunk e (map (lambda (x) (second (or (env-assoc x omicron) `(#f ,x)))) xs)
               (lambda ys (ir-expr-var-rename (apply f ys) omicron)))]
    [else
      (ir-unexpected "var rename" e)]))

(define/contract (ir-block-var-rename ss bvs omicron)
  (-> (listof ir-stmt?) (listof symbol?) var-env? (list/c (listof symbol?) (listof ir-stmt?)))
  (foldl (match-lambda*
           [`(,s (,bvs ,ss))
            (match-let ([`(,bvs ,s) (ir-stmt-var-rename s bvs omicron)])
              `(,bvs ,(snoc ss s)))])
         `(,bvs ())
         ss))

(define/contract (ir-stmt-var-rename s bvs omicron)
  (-> ir-stmt? (listof symbol?) var-env? (list/c (listof symbol?) ir-stmt?))
  (match s
    [(ir-subject-to Gamma_l ss)
     (let* ((Gamma_b (env-filter-keys bvs Gamma_l))
            (omicron_b (env->fresh-var-env Gamma_b))
            (Gamma_l (env-rename omicron_b Gamma_l))
            (omicron (env-union-key omicron_b omicron))
            (bvs (append (env-dom Gamma_l) bvs))
            (bvs-ss (ir-block-var-rename ss bvs omicron)))
       `(,(car bvs-ss) ,(ir-subject-to Gamma_l (cadr bvs-ss))))]
    [(ir-constraint lhs ineq rhs)
     `(,bvs
       ,(ir-constraint
          (ir-expr-var-rename lhs omicron)
          ineq
          (ir-expr-var-rename rhs omicron)))]
    [else
      (ir-unexpected "var rename" s)]))

(define/contract (ir-dec-var-rename d)
  (-> ir-dec? ir-dec?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (let* ((bvs (env-dom (env-union-key Gamma_l Gamma_r Gamma_f)))
            (ss (cadr (ir-block-var-rename ss bvs (env-empty symbol?)))))
       (ir-poly-dec f Gamma_f Gamma_r Gamma_l ss))]
    [else
      (ir-unexpected "var rename" d)]))
