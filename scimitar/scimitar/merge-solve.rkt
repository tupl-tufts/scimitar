#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/util.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")

(provide
  (prefix-out scimitar- merge-solve))

(define constraint? (compose scimitar-constraint? scimitar-typed-e))

(define/contract (merge-solve e)
  (-> scimitar-expr? scimitar-expr?)
  (define/match (go e)
    [((scimitar-typed (scimitar-solve dir res obj locals ps) tyres))
     (let* ((Gamma (apply env-union-key (map scimitar-expr->env ps)))
            (cs (filter constraint? ps))
            (ps (filter (compose not constraint?) ps))
            (result (gensym 'result))
            (tyr (ty-list->ty (map scimitar-typed-ty ps)))
            (poly (ir-compile
                    (ir-poly-dec
                      (gensym 'poly-)
                      Gamma
                      (env `((,result ,tyr)))
                      (env-empty)
                      (cons
                        (ir-constraint
                          (ir-var result)
                          '=
                          (ir-val-list->ir-val (map scimitar-expr->ir-expr ps)))
                        (map scimitar-expr->ir-stmt cs)))
                    (env-empty)
                    (env-empty number?)
                    (env-empty poly?)))
            (p (scimitar-typed (scimitar-app (scimitar-typed (scimitar-poly poly) (poly-ty poly))
                                   (scimitar-typed-list->scimitar-typed
                                     (map (lambda (x ty)
                                         (scimitar-typed (scimitar-var x) ty))
                                       (env-dom Gamma)
                                       (env-range Gamma))))
                          tyr)))
       (scimitar-typed (scimitar-solve dir res obj locals `(,p)) tyres))]
    [(_) (error "BUG in merge-solve")])
  (scimitar-fmap (and/c scimitar-typed? (property/c scimitar-typed-e scimitar-solve?)) go e))
