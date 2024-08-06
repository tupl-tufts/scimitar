#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")

(provide
  (prefix-out scimitar- inline-solve-lambda))

(define (constraint-names ps)
  (map (compose scimitar-var-x scimitar-typed-e scimitar-constraint-rhs scimitar-typed-e) ps))

(define/contract (inline-solve-lambda e)
  (-> scimitar-expr? scimitar-expr?)
  (define/match (go e)
    [((? scimitar-var?)) (values '() e)]
    [((scimitar-typed e ty))
     (let-values ([(ps e) (go e)])
       (values ps (scimitar-typed e ty)))]
    [((? scimitar-val?)) (values '() e)]
    [((? scimitar-poly?)) (values '() e)]
    [((scimitar-tuple es))
     (let*-values ([(pss es) (map-values go es)])
       (values (concat pss)
               (scimitar-tuple es)))]
    [((scimitar-switch op eg es))
     (let*-values ([(ps eg) (go eg)]
                   [(pss es) (for/lists (pss es) ([e es]) (go e))]
                   [(branches) (range (length es))]
                   [(ty-r) (vecty (interval (first branches) (last branches)) '())]
                   [(x) (scimitar-typed (scimitar-var (gensym 'guard)) ty-r)]
                   ;; op chooses between branches, so for (op 3 0 1 2 3 4) -> r, r=3
                   [(op) (op ty-r)]
                   [(g) (scimitar-typed
                          (scimitar-app
                            (scimitar-typed
                              (scimitar-poly op)
                              (poly-ty op))
                            (scimitar-typed
                              (scimitar-tuple
                                `(,eg ,@(map (curryr scimitar-typed ty-r)
                                             (map scimitar-num branches))))
                              (polyty-dom (poly-ty op))))
                          (polyty-cod (poly-ty op)))]
                   [(p) (scimitar-typed (scimitar-constraint g '= x) (unitty))])
       (values
         (cons p ps)
         (scimitar-switch
           (lambda _ (error "switch op already discharged"))
           x (map (lambda (e ps) (scimitar-begin `(,@ps ,e))) es pss))))]
    [((scimitar-app (scimitar-typed (scimitar-lambda ps body) ty) args))
     (define (weird-expr-zip r e)
       (define/match (go t e)
         [((? list?) (scimitar-typed (scimitar-tuple es) _))
          #:when (= (length t) (length es))
          (concat (map go t es))]
         [((? list?) (scimitar-typed (scimitar-val (? list? vs)) ty))
          #:when (= (length t) (length vs))
          (concat (map (lambda (t v ty) (go t (scimitar-typed (scimitar-val v) ty)))
                       t vs (ty->ty-list ty)))]
         [((? list?) (scimitar-typed (scimitar-tuple _) _))
          (error (format "weird-expr-zip: tree nodes have unequal subtree counts~n  in: ~v~n      ~v" r e))]
         [(_ _) `((,t ,e))])
       (go r e))
     (let*-values ([(psa args) (go args)]
                   [(tys) (ty->ty-tree (scimitar-typed-ty args))]
                   [(ps2) (tree-map (lambda (p ty) (scimitar-typed (scimitar-var (gensym p)) (ty-tree->ty ty))) ps tys)]
                   [(pas) (weird-expr-zip ps2 args)]
                   [(_) (when (ormap (compose list? car) pas) (error "During scimitar solve inlining, an argument could not be applied"))]
                   [(pas) (map (lambda (p) (scimitar-typed (scimitar-constraint (cadr p) '= (car p)) (unitty))) pas)]
                   [(omicron) (tree-preorder-zip ps ps2)]
                   [(ps body) (go (subst omicron body))])
       (values (append pas ps psa)
               (scimitar-typed-e body)))]
    [((scimitar-app (scimitar-typed (scimitar-switch op e es) (polyty _ _ tyo)) args))
     (let*-values ([(psa args) (go args)]
                   [(es) (map (lambda (e) (scimitar-typed (scimitar-app e args) tyo)) es)]
                   [(ps e) (go (scimitar-switch op e es))])
       (values (apply append ps psa) e))]
    [((scimitar-app f args))
     (let-values ([(ps f) (go f)]
                  [(psa args) (go args)])
       (values (append ps psa) (scimitar-app f args)))]
    [((scimitar-let bs e))
     (let*-values ([(pss es) (for/lists (pss es) ([b bs]) (go (cadr b)))]
                   [(xs) (map car bs)]
                   [(tys) (map (compose scimitar-typed-ty cadr) bs)]
                   [(xs2) (map (lambda (x ty) (scimitar-typed (scimitar-var (gensym x)) ty)) xs tys)]
                   [(pas) (map (lambda (e x) (scimitar-typed (scimitar-constraint e '= x) (unitty))) es xs2)]
                   [(omicron) (zip xs xs2)]
                   [(ps e) (go (subst omicron e))])
       (values (apply append pas ps pss)
               e))]
    [((scimitar-let* bs e))
     (go (foldr scimitar-let e (map list bs)))]
    [((scimitar-begin es))
     (let*-values ([(pss es) (for/lists (pss es) ([e es]) (go e))]
                   [(ps) (if (null? es) '() (init es))]
                   [(e) (if (null? es) (scimitar-val '()) (last es))])
       (values (apply append ps pss)
               (scimitar-typed-e e)))]
    [((scimitar-lambda ps body)) (error "Unexpected lambda in inline-solve")]
    [((scimitar-infeasible)) (values '() e)]
    [(_) (error "BUG in inline-solve")])
  (scimitar-fmap
    scimitar-solve?
    (match-lambda
      [(scimitar-solve dir res obj locals ps)
       (let*-values ([(pss ps) (for/lists (pss ps) ([p ps]) (go p))]
                     [(ps2) (concat pss)]
                     [(ls) (constraint-names ps2)])
         (scimitar-solve dir res obj (append locals ls) (append ps ps2)))])
    e))
