#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "../env.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/translate.rkt")
(require "../poly.rkt")
(require "../prim/op.ir.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../vec.rkt")

(provide
  (prefix-out scimitar- inline-solve-infeasible))

(define/contract (inline-solve-infeasible e)
  (-> scimitar-expr? scimitar-expr?)
  (define (go-map PC es #:vars [vars #f])
    (if (null? es)
      #f
      (let ((v  (go (if vars (cons (car vars) PC) PC) (car es)))
            (vs (go-map PC (cdr es) #:vars (and vars (cdr vars)))))
        (and (or v vs)
             (cons (or v  (car es))
                   (or vs (cdr es)))))))
  ;; create conditional infeasible constraints using path condition
  (define/match (go PC e)
    [('() (scimitar-typed (scimitar-infeasible) _))
     (error (format "During infeasibility elimination, found a constraint that will always be false"))]
    [(PC (scimitar-typed (scimitar-infeasible) bty))
     (let* ((PC-vars (scimitar-typed-list->scimitar-typed (map car PC)))
            (PC-ty (scimitar-typed-ty PC-vars)))
       (define-ir
         ;; This result looks convoluted but is needed to be type correct.
         (p-inf ((: vars PC-ty)) (: type-correct-result bty))
         (= 0 ((eval (op-eq PC-ty)) vars (eval (map (compose vec-scalar cdr) PC)))))
       (scimitar-typed
         (scimitar-app
           (scimitar-typed
             (scimitar-poly p-inf)
             (poly-ty p-inf))
           PC-vars)
         bty))]
    [(PC (scimitar-typed e ty))
     (let ((e (go PC e)))
       (and e (scimitar-typed e ty)))]
    [(PC (scimitar-tuple es))
     (let ((es (go-map PC es)))
       (and es (scimitar-tuple es)))]
    ; the prior lambda elimination makes the predicate always a variable
    [(PC (scimitar-switch op e es))
     (let ((es (go-map PC es #:vars (map (curry cons e)
                                         (range (length es))))))
       (and es (scimitar-switch op e es)))]
    [(PC (scimitar-switch _ _ _))
     (error "Found an illegal switch over an expression")]
    [(PC (scimitar-app f args))
     (let ((f2 (go PC f))
           (args2 (go PC args)))
       (and (or f2 args2)
            (scimitar-app (or f2 f) (or args2 args))))]
    [(PC (scimitar-begin es))
     (let ((es (go-map PC es)))
       (and es (scimitar-begin es)))]
    [(PC (scimitar-constraint lhs ineq rhs))
     (let ((lhs2 (go PC lhs))
           (rhs2 (go PC rhs)))
       (and (or lhs2 rhs2)
            (scimitar-constraint (or lhs2 lhs) ineq (or rhs2 rhs))))]
    [(_ _) #f])
  (scimitar-fmap
    scimitar-solve?
    (match-lambda
      [(scimitar-solve dir res obj locals ps)
       (let ((ps2 (go-map '() ps)))
         (scimitar-solve dir res obj locals (or ps2 ps)))])
    e))
