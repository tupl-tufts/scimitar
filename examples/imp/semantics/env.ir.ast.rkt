#lang racket

(require scimitar/env)
(require scimitar/poly)
(require scimitar/ty)
(require scimitar/val)
(require scimitar/ir/compiler)
(require scimitar/ir/grammar)
(require scimitar/ir/util)

(provide
  env-set env-var)

(define-syntax-rule (env-set x Gamma)
  (ir-compile
    (let ((y (gensym x))
          (z (gensym x))
          (r (gensym 'r-set)))
      (ir-poly-dec
        'env-set
        (env `((,y ,(second (env-assoc x Gamma)))
               (,z ,(env->ty Gamma))))
        (env `((,r ,(env->ty Gamma))))
        Gamma
        (list
          (ir-constraint
            (ir-var r)
            '=
            (sym-val->ir-val (sym-val-list->sym-val (map (lambda (z) (if (eq? x z) y z)) (env-dom Gamma)))))
          (ir-constraint
            (sym-val->ir-val (sym-val-list->sym-val (env-dom Gamma)))
            '=
            (ir-var z)))))
    (env-empty)
    (env-empty number?)
    (env-empty poly?)))

(define-syntax-rule (env-var x Gamma)
  (ir-compile
    (let ((r (gensym 'r-var)))
      (ir-poly-dec
        'env-var
        Gamma
        (env `((,r ,(second (env-assoc x Gamma)))))
        (env-empty)
        (list
          (ir-constraint
            (ir-var r)
            '=
            (ir-var x)))))
    (env-empty)
    (env-empty number?)
    (env-empty poly?)))
