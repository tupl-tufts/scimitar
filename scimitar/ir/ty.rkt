#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "../ty-def.rkt")
(require "../util.rkt")
(require "../vec.rkt")
(require "error.rkt")
(require "grammar.rkt")

(provide
  ir-val-check
  ir-expr-check
  ir-dec-check)

(define/contract (ir-val-check v)
  (-> (or/c ir-val? ir-tuple?) ty?)
  (match v
    [(ir-num n)
     (vecty (interval n n) '())]
    [(ir-vec v)
     (vec-ty v)]
    [(ir-unit)
     (unitty)]
    [(ir-poly p)
     (poly-ty p)]
    [(ir-tuple vs)
     (apply tuplety (map ir-val-check vs))]
    [else
      (ir-unexpected "type checking" v)]))

(define/contract (ir-expr-check e Gamma)
  (-> ir-expr? env? ty?)
  (define (go e)
    (match e
      [(? ir-val?) (ir-val-check e)]
      [(ir-var x)
       (match (env-assoc x Gamma)
         [(list _ ty) ty]
         [#f (error (format "During type checking: Variable ~v not defined" x))])]
      [(ir-add e1 e2)
       (letrec
         ((ty1 (go e1))
          (ty2 (go e2))
          (ty-sum
            (match-lambda*
              [`(,(? unitty? ty)    ,(? unitty?))       ty]
              [`(,(? vecty? ty1)    ,(? scalarty? ty2)) (vecty-fmap (curry interval-msum (scalarty-interval ty2)) identity ty1)]
              [`(,(? scalarty? ty1) ,(? vecty? ty2))    (vecty-fmap (curry interval-msum (scalarty-interval ty1)) identity ty2)]
              [`(,(? vecty? ty1)    ,(? vecty? ty2))    (vecty-msum ty1 ty2)]
              [`(,(tuplety ty1 ...) ,(tuplety ty2 ...))
               #:when (and (= (length ty1) (length ty2)) (andmap ty-shape-equiv? ty1 ty2))
               (apply tuplety (map ty-sum ty1 ty2))]
              [`(,ty1 ,ty2) (error (format "Expected vectors, units, numbers, or compatible tuples in addition.~nInstead got ~v~n        and ~v~nOriginal expression: ~v" ty1 ty2 (ir-expr->string e)))])))
         (ty-sum ty1 ty2))]
      [(ir-scmul n e)
       (let ((tyn (go n))
             (tye (go e)))
         (define (scmul-check tye)
           (if (or (unitty? tye) (vecty? tye) (tuplety? tye))
             (ty-scale (scalarty-interval tyn) tye)
             (error (format "Expected vector, unit, tuple, or number in scalar multiplication.~nInstead got ~v~n        and ~v~nOriginal expression: ~v" tyn tye (ir-expr->string (ir-scmul n e))))))
         (if (unitty? tyn)
           tyn
           (if (scalarty? tyn)
             (scmul-check tye)
             (error (format "Expected number in scalar multiplication.~nInstead got ~v~nOriginal expression: ~v" tyn (ir-expr->string (ir-scmul n e)))))))
       ]
      [(ir-var-mul e1 e2)
       (let ((ty1 (go e1))
             (ty2 (go e2)))
         (cond
           [(indicatorty? ty1) (ty-scale (scalarty-interval ty1) ty2)]
           [(indicatorty? ty2) (ty-scale (scalarty-interval ty2) ty1)]
           [else
             (with-handlers
               ([exn:fail? (lambda (f1)
                  (with-handlers
                    ([exn:fail? (lambda (f2)
                       (let* ((m0 (format "Expected indicator variable in variable multiplication.~nInstead got ~v~n        and ~v~nOriginal expression: ~v" ty1 ty2 (ir-expr->string e)))
                              (m1 (exn-message f1))
                              (m2 (exn-message f2)))
                         (error (format "~a~nWhen that failed, attempted to type the expression as a constant multiplication:~n~a~n~a" m0 m1 m2))))])
                    (go (ir-scmul e2 e1))))])
               (go (ir-scmul e1 e2)))]))]
      [(ir-tuple es)
       (apply tuplety (map go es))]
      [(ir-vec-ix v ix)
       (let ((tyv (go v))
             (tyi (go ix)))
         (if (vecty? tyv)
           (if (or (unitty? tyi) (and (vecty? tyi) (null? (vecty-shape tyi))) (tuplety? tyi))
             (let ((tyi-rank (match tyi [(unitty) 0] [(vecty _ '())  1] [(tuplety tys ...) (length tys)] [_ (error "BUG in IR type checking")]))
                   (tyv-rank (measurement-dims (vecty-shape tyv))))
               (if (= tyi-rank tyv-rank)
                 (vecty (vecty-interval tyv) '())
                 (error (format "Expected matching rank in vector indexing.~nInstead got vector of rank ~v and index of rank ~v~nOriginal expression: ~v" tyv-rank tyi-rank (ir-expr->string e)))))
             (error (format "Expected an index in vector indexing.~nInstead got ~v~nOriginal expression: ~v" tyi (ir-expr->string e))))
           (error (format "Expected a vector in vector indexing.~nInstead got ~v~nOriginal expression: ~v" tyv (ir-expr->string e)))))]
      [(ir-vec-sum id start end body)
       (let ((tys (go start))
             (tye (go end)))
         (if (and (scalarty? tys) (scalarty? tye))
           (let* ((ty (vecty-hull (scalarty->vecty tys) (scalarty->vecty tye)))
                  (tyb (ir-expr-check body (env-add id ty Gamma))))
             (ty-scale (vecty-interval ty) tyb))
           (error (format "Expected units or numbers in sum bounds.~nInstead got ~v~n        and ~v~nOriginal expression: ~v" tys tye (ir-expr->string e)))))]
      [(ir-cast e ty)
       (define (compatible-cast? tye ty)
         (or (ty-<: tye ty)
             (and (tuplety? tye) (vecty? ty)
                  (equal? 1 (measurement-dims (vecty-shape ty)))
                  (<= (ty-dim tye) (ty-dim ty)))
             (and (tuplety? tye) (tuplety? ty)
                  (andmap compatible-cast? (tuplety-tys tye) (tuplety-tys ty)))))
       (let ((tye (go e)))
         (if (compatible-cast? tye ty)
           ty
           (error (format "Expected cast to compatible type.~nInstead got ~v~n        and ~v~nOriginal expression: ~v" ty tye (ir-expr->string e)))))]
      [(ir-poly-app f x)
       (let ((tyf (go f))
             (tys (go x)))
         (if (and (polyty? tyf)
                  (ty-<:
                    tys
                    (polyty-dom tyf)))
           (polyty-cod tyf)
           (error (format "Expected a poly and matching arguments in poly application.~nInstead got ~v~n        and ~v~nOriginal expression: ~v" tyf tys (ir-expr->string e)))))]
      [(ir-thunk _ xs force)
       (let* ((tys (map (lambda (x)
                          (match (env-assoc x Gamma)
                            [(list _ ty) ty]
                            [#f (error (format "During type checking: Variable ~v not defined" x))]))
                        xs))
              (infs (tree-map (lambda (v) (if (vec-scalar? v) (vec-scalar-value v) v)) (map ty-inf tys)))
              (tyi (ir-val-check (apply force infs))))
         tyi)]
      [else
        (ir-unexpected "type checking" e)]))
    (go e))

(define/contract (ir-stmt-check s Gamma)
  (-> ir-stmt? env? env?)
  (match s
    [(ir-forall id start end body)
     (let* ((tys (ir-expr-check start Gamma))
            (tye (ir-expr-check end Gamma)))
       (if (and (scalarty? tys) (scalarty? tye))
         (let ((ty (vecty-hull (scalarty->vecty tys) (scalarty->vecty tye))))
           (when (< 0 (interval-measure (vecty-interval ty)))
             (ir-stmt-check body (env-add id ty Gamma)))
           (env-empty))
         (error (format "Expected units or numbers in forall bounds.~nInstead got ~v~n        and ~v" tys tye))))]
    [(ir-subject-to Gamma_l ss)
     (let* ((Gamma_2 (env-union-key Gamma_l Gamma)))
       (map (curryr ir-stmt-check Gamma_2) ss)
       Gamma_l)]
    [(ir-constraint lhs _ rhs)
     (let ((tyl (ir-expr-check lhs Gamma))
           (tyr (ir-expr-check rhs Gamma)))
       (if (and ((or/c vecty? tuplety? unitty?) tyl) ((or/c vecty? tuplety? unitty?) tyr))
         (if (and (or (and (vecty? tyl) (not (scalarty? tyl)) (vecty? tyr) (not (scalarty? tyr)))
                      (and (tuplety? tyl) (tuplety? tyr)))
                  (not (ty-shape-equiv? tyl tyr)))
           (error (format "Expected types with compatible shapes in constraint.~nInstead got ~v~n        and ~v" tyl tyr))
           (env-empty))
         (error (format "Expected vectors, units, numbers, or compatible tuples in constraint.~nInstead got ~v~n        and ~v" tyl tyr))))]
    [else
      (ir-unexpected "type checking" s)]))

(define/contract (ir-dec-check d Gamma)
  (-> ir-dec? env? void?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (let* ((Gamma2 (env-union-key Gamma_l Gamma_r Gamma_f Gamma)))
       (map (curryr ir-stmt-check Gamma2) ss))])
  (void))
