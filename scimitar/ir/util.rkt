#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "grammar.rkt")

(provide
  sym-val->ir-val ir-val->val
  sym-val->ir-val-list ir-val-list->ir-val
  ir-free-in
  ir-subst
  ir-val-tree->ir-val ir-val->ir-val-list
  ir-fmap ir-foldr
  ir-ix? ir-ix->measurement)

(define/contract (sym-val->ir-val v)
  (-> sym-val? ir-expr?)
  (cond
    [(symbol? v) (ir-var v)]
    [(vec? v) (ir-vec v)]
    [(null? v) (ir-unit)]
    [(pair? v) (ir-tuple (map sym-val->ir-val v))]
    [(poly? v) (ir-poly v)]))

(define/contract (ir-val->val e)
  (-> ir-expr? (or/c val? #f))
  (match e
    [(ir-num n) (vec-dense n)]
    [(ir-vec v) v]
    [(? ir-unit?) '()]
    [(ir-tuple vs) (let ((vs (map ir-val->val vs)))
                     (and (andmap identity vs) vs))]
    [(ir-poly p) p]
    [_ #f]))

(define/contract (sym-val->ir-val-list v)
  (-> sym-val? (listof ir-expr?))
  (ir-val->ir-val-list (sym-val->ir-val v)))

(define/contract (ir-val-list->ir-val es)
  (-> (listof ir-expr?) ir-expr?)
  (match es
    ['() (ir-unit)]
    [`(,e) e]
    [es (ir-tuple es)]))

(define/contract (ir-val->ir-val-list e)
  (-> ir-expr? (listof ir-expr?))
  (match e
    [(ir-unit) '()]
    [(ir-tuple es) es]
    [e `(,e)]))

(define/match (ir-free-in e)
  [((? ir-val?)) '()]
  [((ir-var x)) `(,x)]
  [((or (ir-add e1 e2)
        (ir-scmul e1 e2)
        (ir-var-mul e1 e2)
        (ir-vec-ix e1 e2)
        (ir-poly-app e1 e2)))
   (append (ir-free-in e1) (ir-free-in e2))]
  [((ir-tuple es)) (concat (map ir-free-in es))]
  [((or (ir-vec-sum i start end body)
        (ir-forall i start end body)))
   (append (ir-free-in start) (ir-free-in end)
     (remove i (ir-free-in body)))]
  [((ir-cast e ty)) (ir-free-in e)]
  ; A thunk could produce a free variable when
  ; run, but there is no way to determine this
  ; except by running it with every possible
  ; value, which might be impossible.
  [((ir-thunk e xs force)) xs]
  [((ir-subject-to G ss))
   (remove* (env-dom G) (concat (map ir-free-in ss)))]
  [((ir-constraint lhs ineq rhs))
   (append (ir-free-in lhs) (ir-free-in rhs))])

(define/contract (ir-subst yzs e)
  (-> (listof (list/c symbol? ir-expr?)) (or/c ir-expr? ir-stmt?) (or/c ir-expr? ir-stmt?))
  (define/match (go e)
    [((? ir-val?)) e]
    [((ir-var x)) (match (assoc x yzs) [`(,y ,z) z] [_ e])]
    [((ir-add e1 e2)) (ir-add (go e1) (go e2))]
    [((ir-scmul n e)) (ir-scmul (go n) (go e))]
    [((ir-var-mul e1 e2)) (ir-var-mul (go e1) (go e2))]
    [((ir-tuple es)) (ir-tuple (map go es))]
    [((ir-vec-ix v ix)) (ir-vec-ix (go v) (go ix))]
    [((ir-vec-sum i start end body))
     (ir-vec-sum i (go start) (go end)
       (let ((yzs (remove*-by-car `(,i) yzs)))
         (ir-subst yzs body)))]
    [((ir-cast e ty)) (ir-cast (go e) ty)]
    [((ir-poly-app f a)) (ir-poly-app (go f) (go a))]
    [((ir-thunk e xs force))
     ;; subst for e is practically impossible
     ;; y can only be masked by sum, but also
     ;; literally anywhere in (eval ...)
     (ir-thunk e xs (lambda ys (go (apply force ys))))]
    [((ir-forall i start end body))
     (ir-forall i (go start) (go end)
       (let ((yzs (remove*-by-car `(,i) yzs)))
         (ir-subst yzs body)))]
    [((ir-subject-to G ss))
     (ir-subject-to G (map go ss))]
    [((ir-constraint lhs ineq rhs))
     (ir-constraint (go lhs) ineq (go rhs))])
  (go e))

(define ir-val-tree?
  (flat-rec-contract ir-val-tree
    ir-val? null?
    (*list/c ir-val-tree ir-val-tree ir-val-tree)))

(define/contract (ir-val-tree->ir-val t)
  (-> ir-val-tree? ir-expr?)
  (match t
    ['() (ir-unit)]
    [(? list?) (ir-tuple (map ir-val-tree->ir-val t))]
    [t t]))

(define/contract (ir-fmap p? f e)
  (-> procedure? procedure? (or/c ir-expr? ir-stmt?) (or/c ir-expr? ir-stmt?))
  (define (test e)
    (find (if (p? e) (f e) e)))
  (define/match (find e)
    [((? ir-val?)) e]
    [((ir-var x)) e]
    [((ir-add e1 e2)) (ir-add (test e1) (test e2))]
    [((ir-scmul n e)) (ir-scmul (test n) (test e))]
    [((ir-var-mul e1 e2)) (ir-var-mul (test e1) (test e2))]
    [((ir-tuple es)) (ir-tuple (map test es))]
    [((ir-vec-ix v ix)) (ir-vec-ix (test v) (test ix))]
    [((ir-vec-sum id start end body)) (ir-vec-sum id (test start) (test end) (test body))]
    [((ir-cast e ty)) (ir-cast (test e) ty)]
    [((ir-poly-app g x)) (ir-poly-app (test g) (test x))]
    [((ir-thunk e xs force)) (ir-thunk e xs (lambda ys (test (apply force ys))))]
    [((ir-forall id start end body)) (ir-forall id (test start) (test end) (test body))]
    [((ir-subject-to G ss)) (ir-subject-to G (map test ss))]
    [((ir-constraint lhs ineq rhs)) (ir-constraint (test lhs) ineq (test rhs))])
  (test e))

(define/contract (ir-foldr p? f acc e)
  (-> procedure? procedure? any/c (or/c ir-expr? ir-stmt?) any/c)
  (define (test e acc)
    (find (if (p? e) (f e acc) acc) e))
  (define/match (find acc e)
    [(_ (? ir-val?)) acc]
    [(_ (ir-var x)) acc]
    [(_ (or (ir-add e1 e2)
            (ir-scmul e1 e2)
            (ir-var-mul e1 e2)
            (ir-vec-ix e1 e2)
            (ir-poly-app e1 e2)
            (ir-constraint e1 _ e2)))
     (test e1 (test e2 acc))]
    [(_ (or (ir-tuple es)
           (ir-subject-to _ es)))
     (foldr test acc es)]
    [(_ (or (ir-vec-sum id start end body)
            (ir-forall id start end body)))
     (test start (test end (test body acc)))]
    [(_ (ir-cast e ty)) (test e acc)]
    [(_ (ir-thunk e xs force)) (error "Cannot foldr over a thunk")])
  (test e acc))

(define ir-ix?
  (or/c ir-unit? ir-num?
        (and/c ir-vec? (property/c ir-vec-vec (vec-dims/c (=/c 0))))
        (and/c ir-tuple? (property/c ir-tuple-es (listof (or/c ir-num? (and/c ir-vec? (property/c ir-vec-vec (vec-dims/c (=/c 0))))))))))

(define/contract (ir-ix->measurement e)
  (-> ir-ix? measurement?)
  (match e
    [(ir-unit) '()]
    [(ir-num ix) `(,ix)]
    [(ir-vec (vec-dense ix)) `(,ix)]
    [(ir-tuple ixs) (map (match-lambda [(ir-num ix) ix] [(ir-vec (vec-dense ix)) ix]) ixs)]))
