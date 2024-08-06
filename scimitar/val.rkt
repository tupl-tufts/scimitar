#lang racket

(require "contract-utils.rkt")
(require "env.rkt")
(require "poly.rkt")
(require "topo.rkt")
(require "ty.rkt")
(require "util.rkt")
(require "vec.rkt")

(provide
  val?
  val-ty
  val-cast
  vec->val
  val->vec
  sym?
  sym-val?
  sym-val-ty
  sym-val-free-in
  sym-val-list->sym-val
  sym-val->sym-val-list
  sym-val-append)

(define val?
  (flat-rec-contract val
    vec? null? poly?
    (*list/c val val val)))

(define/contract (val-ty v)
  (-> val? ty?)
  (cond
    [(vec? v)  (vec-ty v)]
    [(null? v) (unitty)]
    [(pair? v) (apply tuplety (map val-ty v))]
    [(poly? v) (poly-ty v)]))

(define/contract (val-cast ty v)
  (->i ([ty (v) (curry ty-<: (val-ty v))]
        [v val?])
       [result val?])
  (cond
    [(vecty? ty)
     (or (vec-cast ty v)
         (error (format "val-cast failed: ~v not contained by desired type ~v" (vec-ty v) ty)))]
    [(unitty? ty)
     v]
    [(tuplety? ty)
     (map val-cast (tuplety-tys ty) v)]
    [(and (polyty? ty)
          ((measurement-exceeds (polyty-shape (poly-ty v)))
           (polyty-shape ty)))
     ;; polyty is unique in that it encodes the size and column types,
     ;; so we should keep the old size and inner types
     (poly (polyty-dom ty) (polyty-cod ty) (poly-Gamma-col v) (constraint-pad (poly-cs v) (polyty-shape ty)))]
    [else
      (error (format "Uncastable type ~v in val-cast" ty))]))

(define/contract (vec->val ty v)
  (->i ([ty ty?]
        [v (ty) (and/c (vec-dims/c (=/c 1))
                       (property/c vec-length (=/c (ty-dim ty))))])
       [result val?])
  (cond
    [(vecty? ty)
     (let* ((ty-flat (vecty (vecty-interval ty) `(,(ty-dim ty))))
            (v2 (vec-dynamic-cast ty-flat v)))
       (or (and v2 (vec-reshape v2 (vecty-shape ty)))
           (error (format "vec->val failed: ~v not contained by desired type ~v" (vec-ty v) ty))))]
    [(and (unitty? ty) (let ((v (vec-dynamic-cast (vecty zero `(,(ty-dim (unitty)))) v)))
                         (and v (= 0 (vec! v '(0)))))) '()]
    [(tuplety? ty)
     (car
       (foldr
         (lambda (ty acc)
           (let* ((t (car acc))
                  (v (cadr acc))
                  (s (- (vec-length v) (ty-dim ty)))
                  (u (vec->val ty (vec-project v `(,s))))
                  (w (vec-project v '(0) `(,s))))
             (list (cons u t) w)))
         `(() ,v)
         (tuplety-tys ty)))]
    [(polyty? ty)
     (error "(vec->val polyty? vec?) Not yet implemented")]
    [else
      (error (format "Uncastable type ~v in vec->val" ty))]))

(define/contract (val->vec v)
  (-> val? vec?)
  (let ((ty (val-ty v)))
    (cond
      [(vecty? ty) (vec-flatten v)]
      [(unitty? ty) (vec-0 '(1))]
      [(tuplety? ty) (apply vec-augment 0 (map val->vec v))]
      [(polyty? ty) (let ((c (poly-cs v)))
                      (vec-augment 0
                        (vec-flatten (constraint-A c))
                        (vec-dense (map (match-lambda ['<= -1] ['= 0] ['>= 1]) (constraint-ineq c)))
                        (vec-flatten (constraint-b c))))])))

(define sym?
  (flat-rec-contract sym
    symbol? null?
    (*list/c sym sym sym)))

(define sym-val?
  (flat-rec-contract sym-val
    vec? null? poly? symbol?
    (*list/c sym-val sym-val sym-val)))

(define/contract (sym-val-ty Gamma v0)
  (-> env? sym-val? ty?)
  (define (go v)
    (cond
      [(symbol? v)
       (second
         (or (env-assoc v Gamma)
           (error (format "Type for ~a not found in value ~v" v v0))))]
      [(vec? v)  (vec-ty v)]
      [(null? v) (unitty)]
      [(pair? v) (apply tuplety (map (curry sym-val-ty Gamma) v))]
      [(poly? v) (poly-ty v)]))
  (go v0))

(define/contract (sym-val-free-in v)
  (-> sym-val? (listof symbol?))
  (cond
    [(symbol? v) `(,v)]
    [(list? v) (concat (map sym-val-free-in v))]
    [else '()]))

(define/contract (sym-val-list->sym-val vs)
  (-> (listof sym-val?) sym-val?)
  (match vs
    [`(,v) v]
    [_ vs]))

(define/contract (sym-val->sym-val-list v)
  (-> sym-val? (listof sym-val?))
  (if (list? v)
    v
    `(,v)))

(define (sym-val-append v w)
  (sym-val-list->sym-val
    (append
      (sym-val->sym-val-list v)
      (sym-val->sym-val-list w))))
