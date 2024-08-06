#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "../ir/translate.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/util.rkt")
(require "../env.rkt")
(require "../params.rkt")
(require "../poly.rkt")
(require "../prim/op.ir.rkt")
(require "../ty.rkt")
(require "../util.rkt")

(provide
  (prefix-out scimitar- inline-solve-switch))

(define (constraint-names ps)
  (map (compose scimitar-var-x scimitar-typed-e scimitar-constraint-rhs scimitar-typed-e) ps))

(define/contract (inline-solve-switch e)
  (-> scimitar-expr? scimitar-expr?)
  (define (elim-switch e)
    (define/match (go e)
      [((scimitar-typed e ty)) (scimitar-typed (go e) ty)]
      [((scimitar-tuple es)) (scimitar-tuple (map go es))]
      [((scimitar-switch _ e es))
       (let* ((e (go e))
              (pss (map (match-lambda [(scimitar-begin `(,@ps ,e)) (map go ps)]) es))
              (es  (map (match-lambda [(scimitar-begin `(,@ps ,e)) (go e)]) es))
              (result (gensym 'result))
              (branches (range (length es)))
              (ir-e (scimitar-expr->ir-expr e))
              (p-locals (map constraint-names pss))
              (Gamma-e (scimitar-expr->env e))
              (Gammas (map (lambda (e ps) (apply env-union-key (map scimitar-expr->env (cons e ps)))) es pss))
              (Gammas-ps (map env-filter-keys p-locals Gammas))
              (Gammas (map env-remove* p-locals Gammas))
              (Gamma-es (if (null? Gammas) (env-empty) (apply env-union-key Gammas)))
              (Gamma-i (env-union-key Gamma-e Gamma-es))
              (Gamma-o (env `((,result ,(last (cons (unitty) (map scimitar-typed-ty es)))))))
              (ty-r (scimitar-typed-ty e))
              (p-cmp (op-eq ty-r))
              (p (ir-compile
                   (ir-poly-dec
                     (gensym 'switch-)
                     Gamma-i
                     Gamma-o
                     (env-empty)
                     (map  (lambda (Gamma n e ps Gamma-ps)
                             ;; (p3 a b) becomes (p3 (if r==3 a fake-a3) (if r==3 b fake-b3))
                             ;; fake-[ab][01234]. But (p3 a b) (q3 a) use the same fakes.
                             (let* ((Gamma-real (env-union-key Gamma-o Gamma Gamma-ps))
                                    (Gamma-fake
                                      (env-map (lambda (k v) `(,(gensym k) ,v)) Gamma-real))
                                    (Gamma-used
                                      (env-map (lambda (k v) `(,(gensym k) ,v)) Gamma-real))
                                    (omicron-out (zip (env-dom Gamma-real) (map scimitar-var (env-dom Gamma-used))))
                                    (rename-out (curry subst omicron-out)))
                               (let ((=0 (gensym '=0)))
                                 (ir-subject-to
                                   (env-set =0 (bitty) (env-union-key Gamma-fake Gamma-used Gamma-ps))
                                   `(,(ir-constraint
                                        (ir-poly-app
                                          (ir-poly p-cmp)
                                          (ir-tuple `(,ir-e ,(ir-num n))))
                                        '=
                                        (ir-var =0))
                                     ,@(map (lambda (real fake used)
                                              (ir-constraint
                                                (ir-var used)
                                                '=
                                                (ir-add
                                                  (ir-var-mul
                                                    (ir-var =0)
                                                    (ir-var real))
                                                  (ir-var-mul
                                                    (ir-sub (ir-num 1) (ir-var =0))
                                                    (ir-var fake)))))
                                            (env-dom Gamma-real)
                                            (env-dom Gamma-fake)
                                            (env-dom Gamma-used))
                                     ,@(map (compose scimitar-expr->ir-stmt rename-out) ps)
                                     ,(ir-constraint
                                        (scimitar-expr->ir-expr (rename-out e))
                                        '= ;; Get the conditional variable for 'result
                                        (ir-var (car (env-dom Gamma-used)))))))))
                           Gammas branches es pss Gammas-ps))
                   (env-empty)
                   (env-empty number?)
                   (env-empty poly?))))
         (scimitar-app (scimitar-typed (scimitar-poly p) (poly-ty p))
                  (scimitar-typed-list->scimitar-typed
                    (map (lambda (x ty)
                           (scimitar-typed (scimitar-var x) ty))
                         (env-dom Gamma-i)
                         (env-range Gamma-i)))))]
      [((and (? scimitar-app?) (? scimitar-expr->ir-expr)))
       ;; Special case when poly is only applied to vars and vals, etc.
       e]
      [((scimitar-app (scimitar-typed (scimitar-poly p) (polyty tyi _ tyo)) args))
       (let* ((args (go args))
              (result (gensym 'result))
              (Gamma-i (scimitar-expr->env args))
              (Gamma-o (env `((,result ,tyo))))
              (Gamma-l (env-empty))
              (p (ir-compile
                   (ir-poly-dec
                     (gensym 'app-)
                     Gamma-i
                     Gamma-o
                     Gamma-l
                     `(,(ir-constraint
                          (ir-var result)
                          '=
                          (ir-poly-app (ir-poly p) (scimitar-expr->ir-expr args)))))
                   (env-empty)
                   (env-empty number?)
                   (env-empty poly?))))
         (scimitar-app (scimitar-typed (scimitar-poly p) (poly-ty p))
                  (scimitar-typed-list->scimitar-typed
                    (map (lambda (x ty)
                           (scimitar-typed (scimitar-var x) ty))
                         (env-dom Gamma-i)
                         (env-range Gamma-i)))))]
      [((scimitar-constraint e1 ineq e2)) (scimitar-constraint (go e1) ineq (go e2))]
      [(_) e])
    (match e
      [(scimitar-solve dir res obj locals ps)
       (scimitar-solve dir res obj locals (map go ps))]))
  (define (elim-check e)
    (match e
      [(scimitar-solve dir res obj locals ps)
       (for-each
         (match-lambda
           [(or (? scimitar-expr->ir-expr)
                (scimitar-typed (scimitar-constraint _ _ _) _))
            (void)]
           [(scimitar-typed (scimitar-app _ _) _) ;; See assumption in merge-solve
            (error "During scimitar solve inlining, applications must be of a poly over variables or values")]
           [_ (error "BUG in inline-solve")])
         ps)
       e]))
  (scimitar-fmap scimitar-solve? (compose elim-check elim-switch) e))
