#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../util.rkt")
(require "../ty.rkt")
(require "../vec.rkt")
(require "../val.rkt")
(require "../vm.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/lower.rkt")
(require "../ir/util.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- lower))

(define/contract (contty Gamma args)
  (-> env? sym-val? ty?)
  (tuplety
    (natty)
    (realty
      (ty-dim
          (sym-val-ty Gamma args)))))

(define (upper-bound-contty Gamma argss)
  (tuplety
    (natty)
    (realty
      (apply max
        (map (compose ty-dim (curry sym-val-ty Gamma)) argss)))))

(define (construct-cont addrs f args)
  (match (assoc f addrs)
    [`(,_ ,addr)
     (ir-tuple `(,(ir-num addr) ,(sym-val->ir-val args)))]
    [#f (error (format "While compiling, ~a not found" f))]))

(define (lower addrs Gamma e)
  (match e
    [(cps-app f args)
     (let* ((r (gensym 'r))
            (poly (ir-compile
                    (ir-poly-dec
                      (gensym f)
                      Gamma
                      (env `((,r ,(contty Gamma args))))
                      (env-empty)
                      (list
                        (ir-constraint
                          (ir-var r)
                          '=
                          (construct-cont addrs f args))))
                    (env-empty)
                    (env-empty number?)
                    (env-empty poly?))))
       (lp-default poly))]
    [(cps-primop (cps-op dir obj (and p (poly _ to _ _))) args out (cps-app f vs))
     (let* ((Gamma-l (env (map-cadr ty-tree->ty (tree-preorder-zip out (ty->ty-tree to)))))
            (r (gensym 'r))
            (poly (ir-compile
                    (ir-poly-dec
                      (gensym 'prim)
                      Gamma ; Technically wrong, as it doesn't account for xs' structure e.g. (x (y z))
                      (env `((,r ,(contty (env-union-key Gamma-l Gamma) vs))))
                      Gamma-l
                      (list
                        (ir-constraint
                          (ir-var r)
                          '=
                          (construct-cont addrs f vs))
                        (ir-constraint
                          (sym-val->ir-val out)
                          '=
                          (ir-poly-app (ir-poly p)
                            (sym-val->ir-val args)))))
                    (env-empty)
                    (env-empty number?)
                    (env-empty poly?)))
            ;; There is no way for obj to handle vars local to p, so hardcode it
            ;; We know that IR compilation puts polys at the end, so since there
            ;; is only one poly (p), we can merge from the end.
            (G-poly (poly-Gamma-col poly))
            (G-p    (poly-Gamma-col p   ))
            (omicron (zip (env-dom G-p) (take-right (env-dom G-poly) (length (env-dom G-p)))))
            (obj (map-car (lambda (x) (second (or (assoc x omicron) (error "BUG in cps-lower")))) obj)))
       (lp
         dir
         (calculate-obj poly obj)
         poly))]
    [(cps-switch op v conts)
     (let* ((ty-o (upper-bound-contty Gamma (map cps-app-args conts)))
            (cast-vars (map (compose gensym cps-app-f) conts))
            (op-final (op ty-o))
            (r (gensym 'r))
            (poly (ir-compile
                    (ir-poly-dec
                      (gensym 'switch)
                      Gamma
                      (env `((,r ,ty-o)))
                      (env (map (curryr list ty-o) cast-vars))
                      (cons
                        (ir-constraint
                          (ir-var r)
                          '=
                          (ir-poly-app (ir-poly op-final)
                            (ir-val-list->ir-val
                              (cons
                                (sym-val->ir-val v)
                                (map ir-var cast-vars)))))
                        (map (match-lambda* [`(,x ,(cps-app f args))
                                             (ir-constraint
                                               (ir-var x) '=
                                               (ir-cast (construct-cont addrs f args) ty-o))])
                          cast-vars
                          conts)))
                    (env-empty)
                    (env-empty number?)
                    (env-empty poly?))))
       (lp-default poly))]
    [e (error (format "BUG: ~a cannot be in flattened continuation" e))]))
