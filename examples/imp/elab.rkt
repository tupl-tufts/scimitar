#lang racket

(require (for-syntax syntax/transformer))
(require scimitar/contract-utils)
(require "grammar.rkt")
(require "semantics/bv-helpers.rkt")
(require scimitar/elab)
(require scimitar/env)
(require scimitar/ty)
(require scimitar/util)

(provide
  naturalty
  boolty
  imp-expr-elab
  imp-block-elab)

(define-syntax naturalty (make-variable-like-transformer #'(bitty (bitwidth))))
(define boolty (bitty))

(define/contract (imp-expr-inf e)
  (-> imp-expr? (values (listof ~?) env? ty?))
  (match e
    [(or (imp-plus e1 e2) (imp-minus e1 e2))
     (let-values ([(c1s Gamma1 ty1) (imp-expr-inf e1)]
                  [(c2s Gamma2 ty2) (imp-expr-inf e2)])
       (values (append `(,(~ ty1 naturalty) ,(~ ty2 naturalty)) c1s c2s)
               (env-union-key Gamma1 Gamma2)
               naturalty))]
    [(imp-le e1 e2)
     (let-values ([(c1s Gamma1 ty1) (imp-expr-inf e1)]
                  [(c2s Gamma2 ty2) (imp-expr-inf e2)])
       (values (append `(,(~ ty1 naturalty) ,(~ ty2 naturalty)) c1s c2s)
               (env-union-key Gamma1 Gamma2)
               boolty))]
    [(imp-eq e1 e2)
     (let-values ([(c1s Gamma1 ty1) (imp-expr-inf e1)]
                  [(c2s Gamma2 ty2) (imp-expr-inf e2)])
       ;strict imp: (~ ty1 naturalty)
       (values (append `(,(~ ty1 ty2)) c1s c2s)
               (env-union-key Gamma1 Gamma2)
               boolty))]
    [(imp-bool v)
     (values '() (env-empty) boolty)]
    [(imp-not q)
     (let-values ([(cs Gamma ty) (imp-expr-inf q)])
       (values (cons (~ ty boolty) cs)
               Gamma
               boolty))]
    [(or (imp-and q1 q2) (imp-or q1 q2))
     (let-values ([(c1s Gamma1 ty1) (imp-expr-inf q1)]
                  [(c2s Gamma2 ty2) (imp-expr-inf q2)])
       (values (append `(,(~ ty1 boolty) ,(~ ty2 boolty)) c1s c2s)
               (env-union-key Gamma1 Gamma2)
               boolty))]
    [(imp-nat n)
     (values '() (env-empty) naturalty)]
    [(imp-var x)
     (values '() (env `((,x ,(tyvar x)))) (tyvar x))]))

(define/contract (imp-stmt-inf s)
  (-> imp-stmt? (values (listof ~?) env?))
  (match s
    [(imp-skip)
     (values '() (env-empty))]
    [(imp-assign x e)
     (let-values ([(cs Gamma ty) (imp-expr-inf e)])
       ;strict imp: (~ ty naturalty)
       (values (cons (~ (tyvar x) ty) cs)
               (env-add x (tyvar x) Gamma)))]
    [(imp-if q st sf)
     (let-values ([(cs Gamma ty) (imp-expr-inf q)]
                  [(cts Gammat) (imp-stmt-inf st)]
                  [(cfs Gammaf) (imp-stmt-inf sf)])
       (values (append `(,(~ ty boolty)) cs cts cfs)
               (env-union-key Gamma Gammat Gammaf)))]
    [(imp-while q bs)
     (let-values ([(cs Gamma ty) (imp-expr-inf q)]
                  [(cbs Gammab) (imp-block-inf bs)])
       (values (append `(,(~ ty boolty)) cs cbs)
               (env-union-key Gamma Gammab)))]))

(define/contract (imp-block-inf ss)
  (-> (listof imp-stmt?) (values (listof ~?) env?))
  (let-values ([(css Gammas) (map-values imp-stmt-inf ss)])
    (values (concat css) (apply env-union-key Gammas))))

(define/contract (imp-expr-elab e Gamma)
  (-> imp-expr? env? ty?)
  (let-values ([(cs _ ty) (imp-expr-inf e)]
               [(cts) (map (curry apply ~) (map-car tyvar (env-entries Gamma)))])
    (ty-subst ty (solve 'Imp (append cts cs)))))

(define/contract (imp-block-elab p)
  (-> (listof imp-stmt?) env?)
  (let-values ([(cs Gamma) (imp-block-inf p)])
    (env-subst Gamma (solve 'Imp cs))))
