#lang racket

(require "../contract-utils.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- alpha))

(define/contract (alpha e)
  (-> cps-cexp? cps-cexp?)
  (define/match (go bvs e)
    [(_ (? cps-app?))
     `(,e ,bvs)]
    [(_ (cps-fix fs cont))
     (let* ((fs-gs (map (lambda (fxsb)
                          (let ((f (car fxsb)))
                            `(,f ,(if (member f bvs) (gensym f) f))))
                        fs))
            (bvs (append (map cadr fs-gs) bvs))
            (fs-gs-subst (filter (lambda (f-g)
                                   (not (equal? (car f-g) (cadr f-g))))
                                 fs-gs))
            (cont (if (null? fs-gs-subst) cont (apply foldl subst cont (unzip fs-gs-subst))))
            (fs-bvs (foldr (lambda (fxsb fs-bvs)
                             (let* ((f (car fxsb))
                                    (xs (cadr fxsb))
                                    (b (caddr fxsb))
                                    (fs (car fs-bvs))
                                    (bvs (cadr fs-bvs))
                                    (g (second (or (assoc f fs-gs) (error "Bug in CPS alpha"))))
                                    (ys (filter (curryr member bvs) (flatten xs)))
                                    (zs (map gensym ys))
                                    (ws (foldl (lambda (y z xs)
                                                 (tree-map (lambda (x)
                                                             (if (eq? y x) z x))
                                                           xs))
                                               xs ys zs))
                                    (fs-gs-scoped (filter (lambda (f-g)
                                                            (not (member (car f-g) (flatten xs))))
                                                          fs-gs))
                                    (b (if (null? fs-gs-scoped) b (apply foldl subst b (unzip fs-gs-scoped))))
                                    (b (foldl subst b ys zs))
                                    (b-bvs (go (append (flatten ws) bvs) b)))
                               `(,(cons `(,g ,ws ,(car b-bvs)) fs) ,(cadr b-bvs))))
                           `(() ,bvs)
                           fs))
            (cont-bvs (go (cadr fs-bvs) cont)))
       `(,(cps-fix (car fs-bvs) (car cont-bvs)) ,(cadr cont-bvs)))]
    [(_ (cps-primop op args out cont))
     (let* ((newout (tree-map (lambda (o) (if (member o bvs) (gensym o) o)) out))
            (omicron (tree-preorder-zip out newout))
            (cont-bvs (go (append (sym-val-free-in newout) bvs)
                          (if (or (null? out) (equal? out newout))
                            cont
                            (apply foldr subst cont (unzip omicron))))))
       `(,(cps-primop op args newout (car cont-bvs)) ,(cadr cont-bvs)))]
    [(_ (cps-switch op v conts))
     (let ((conts-bvs (foldr (lambda (c cs-bvs)
                               (let* ((cs (car cs-bvs))
                                      (bvs (cadr cs-bvs))
                                      (c-bvs (go bvs c)))
                                 `(,(cons (car c-bvs) cs) ,(cadr c-bvs))))
                             `(() ,bvs)
                             conts)))
      `(,(cps-switch op v (car conts-bvs)) ,(cadr conts-bvs)))])
  (car (go '() e)))
