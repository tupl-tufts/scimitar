#lang racket

(require "grammar.rkt")
(require "util.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "../ty-def.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")

(provide (prefix-out scimitar- const-reduce))

(define const? (or/c scimitar-num? scimitar-val? scimitar-poly?))

(define/match (get-num v)
  [((scimitar-num n)) n]
  [((scimitar-val (? vec-scalar? v))) (vec-scalar-value v)]
  [(_) #f])

(define (make-zero ty)
  (if (scalarty? ty)
    (scimitar-num 0)
    (scimitar-val (ty-inf (ty-scale zero ty)))))

(define (const-reduce e)
  (define/match (go e rho)
    [((scimitar-solve dir res obj locals ps) _)
     (let ((rho (env-remove* locals rho)))
       (scimitar-solve dir res (go obj rho) locals (map (curryr go rho) ps)))]
    [((scimitar-var x) _)
     (match (env-assoc x rho)
       [(list _ v) v]
       [#f e])]
    [((scimitar-typed e ty) _)
     (scimitar-typed (go e rho) ty)]
    [((scimitar-val _) _)
     e]
    [((scimitar-poly _) _)
     e]
    [((scimitar-tuple es) _)
     (let ((es (map (curryr go rho) es)))
       (if (andmap (or/c scimitar-val? scimitar-num?
                         (and/c scimitar-typed? (property/c scimitar-typed-e scimitar-val?))
                         (and/c scimitar-typed? (property/c scimitar-typed-e scimitar-num?))) es)
         (scimitar-val
           (map (match-lambda
                  [(scimitar-typed (scimitar-num n) ty) (vec-dense n ty)]
                  [(scimitar-typed (scimitar-val v) ty) (val-cast ty v)])
                es))
         (scimitar-tuple es)))]
    [((scimitar-switch op e es) _)
     (scimitar-switch op (go e rho) (map (curryr go rho) es))]
    [((scimitar-app g args) _)
     (match* ((go g rho) (go args rho))
       [((scimitar-typed (scimitar-lambda ps body) _) (scimitar-typed args _))
        #:when (andmap const? (scimitar-expr->scimitar-expr-list args))
        (let ((rho (env-union-key
                     (env (tree-preorder-zip
                            (sym-val->sym-val-list ps)
                            (scimitar-expr->scimitar-expr-list args))
                          const?)
                     rho)))
          (go body rho))]
       [(g args)
        (scimitar-app g args)])]
    [((scimitar-lambda ps body) _)
     (scimitar-lambda ps (go body (env-remove* (flatten ps) rho)))]
    [((scimitar-fix fs e) _)
     (scimitar-fix (map-cadr (curryr go rho) fs) (go e rho))]
    [((scimitar-let bs e) _)
     (let*-values ([(bs) (map-cadr (curryr go rho) bs)]
                   [(cs bs) (partition (compose const? scimitar-typed-e cadr) bs)]
                   [(rho) (env-remove* (map car bs) (env-union-key (env (map-cadr scimitar-typed-e cs) const?) rho))]
                   [(e) (go e rho)])
       (if (null? bs)
         (scimitar-typed-e e)
         (scimitar-let bs e)))]
    [((scimitar-let* bs e) _)
     (let* ((rho-bs (foldl
                      (lambda (b rho-bs)
                        (let* ((rho (car rho-bs))
                               (bs (cadr rho-bs))
                               (x (car b))
                               (e (go (cadr b) rho)))
                          (if (const? (scimitar-typed-e e))
                            `(,(env-set x (scimitar-typed-e e) rho) ,bs)
                            `(,(env-remove x rho) ,(snoc bs `(,x ,e))))))
                      `(,rho ())
                      bs))
            (rho (car rho-bs))
            (bs (cadr rho-bs))
            (e (go e rho)))
       (if (null? bs)
         (scimitar-typed-e e)
         (scimitar-let* bs e)))]
    [((scimitar-begin es) _)
     (define (concat-begin es)
       (concat (map (match-lambda
                      [(scimitar-typed (scimitar-begin es) _) es]
                      [e `(,e)])
                    es)))
     (match (concat-begin (map (curryr go rho) es))
       ['() (scimitar-val '())]
       [`(,es ... ,e)
        (let ((es (filter (compose not const? scimitar-typed-e) es)))
          (if (null? es) ;; none of the initial constants can possibly matter
            (scimitar-typed-e e)
            (scimitar-begin (snoc es e))))])]
    [((scimitar-num _) _)
     e]
    [((scimitar-plus e1 e2) _)
     (match
       (match* ((go e1 rho) (go e2 rho))
         [((scimitar-typed (scimitar-tuple ts1) _)
           (scimitar-typed (scimitar-tuple ts2) _))
          (go (scimitar-tuple
                (map (match-lambda*
                       [`(,(and e1 (scimitar-typed _ ty1)) ,(and e2 (scimitar-typed _ ty2)))
                        (scimitar-typed
                          (scimitar-plus e1 e2)
                          (ty-msum ty1 ty2))])
                     ts1 ts2))
              rho)]
         [((scimitar-typed (scimitar-val v1) _)
           (scimitar-typed (scimitar-val v2) _))
          (define/match (add-val v1 v2)
            [((? vec?) (? vec?)) (vec+ v1 v2)]
            [((? list?) (? list?)) (map add-val v1 v2)]
            [(_ _) (error (format "Addition is ill defined for ~v and ~v" v1 v2))])
          (scimitar-val (add-val v1 v2))]
         [((scimitar-typed (? get-num n) _)
           (scimitar-typed (? get-num m) _))
          (scimitar-num (+ (get-num n) (get-num m)))]
         [((scimitar-typed (? const? e1) ty1)
           (scimitar-typed (scimitar-plus (scimitar-typed (? const? e2) ty2) e3) _))
          (go (scimitar-plus
                (scimitar-typed
                  (scimitar-plus (scimitar-typed e1 ty1) (scimitar-typed e2 ty2))
                  (ty-msum ty1 ty2))
                e3)
              rho)]
         [((scimitar-typed (scimitar-plus (scimitar-typed (? const? e1) ty1) e2) _)
           (scimitar-typed (? const? e3) ty3))
          (go (scimitar-plus
                (scimitar-typed
                  (scimitar-plus (scimitar-typed e1 ty1) (scimitar-typed e3 ty3))
                  (ty-msum ty1 ty3))
                e2)
              rho)]
         [((scimitar-typed (scimitar-plus (scimitar-typed (? const? e1) ty1)
                                          (scimitar-typed e2 ty2)) _)
           (scimitar-typed (scimitar-plus (scimitar-typed (? const? e3) ty3)
                                          (scimitar-typed e4 ty4)) _))
          (go (scimitar-plus
                (scimitar-typed
                  (scimitar-plus (scimitar-typed e1 ty1) (scimitar-typed e3 ty3))
                  (ty-msum ty1 ty3))
                (scimitar-typed
                  (scimitar-plus (scimitar-typed e2 ty2) (scimitar-typed e4 ty4))
                  (ty-msum ty2 ty4)))
              rho)]
         [((scimitar-typed (scimitar-plus (scimitar-typed (? const? e1) ty1)
                                          (scimitar-typed e2 ty2)) _)
           (scimitar-typed e3 ty3))
          (go (scimitar-plus
                (scimitar-typed e1 ty1)
                (scimitar-typed
                  (scimitar-plus
                    (scimitar-typed e2 ty2)
                    (scimitar-typed e3 ty3))
                  (ty-msum ty2 ty3)))
              rho)]
         [((scimitar-typed e1 ty1)
           (scimitar-typed (scimitar-plus (scimitar-typed (? const? e2) ty2)
                                          (scimitar-typed e3 ty3)) _))
          (go (scimitar-plus
                (scimitar-typed e2 ty2)
                (scimitar-typed
                  (scimitar-plus
                    (scimitar-typed e1 ty1)
                    (scimitar-typed e3 ty3))
                  (ty-msum ty2 ty3)))
              rho)]
         [(e1 (scimitar-typed (? const? e2) ty2))
          (scimitar-plus (scimitar-typed e2 ty2) e1)]
         [(e1 e2)
          (scimitar-plus e1 e2)])
       [(scimitar-plus (scimitar-typed (app get-num 0) _) (scimitar-typed e _)) e]
       [e e])]
    [((scimitar-minus e1 e2) _)
     (match* ((go e1 rho) (go e2 rho))
       [(e1 (scimitar-typed e2 ty2))
        (go (scimitar-plus
              e1
              (scimitar-typed
                (scimitar-mul
                  (scimitar-typed
                    (scimitar-num -1)
                    (vecty (interval -1 -1) '()))
                  (scimitar-typed e2 ty2))
                (ty-scale (interval -1 -1) ty2)))
            rho)])]
    [((scimitar-mul e1 e2) _)
     (match
       (match* ((go e1 rho) (go e2 rho))
         [((scimitar-typed (? get-num e1) ty1) e2)
          (match* ((get-num e1) e2)
            [(n (scimitar-typed (scimitar-tuple es) _))
             (go (scimitar-tuple
                   (map (lambda (e)
                          (scimitar-typed
                            (scimitar-mul
                              (scimitar-typed
                                (scimitar-num n)
                                (vecty (interval n n) '()))
                              e)
                            (ty-scale (interval n n) (scimitar-typed-ty e))))
                        es))
                 rho)]
            [(n (scimitar-typed (? get-num m) _))
             (scimitar-num (* n (get-num m)))]
            [(n (scimitar-typed (scimitar-val v) _))
             (define/match (scale-val v)
               [((? vec?)) (vec.* n v)]
               [((? list?)) (map scale-val v)]
               [(_) (error (format "Multiplication is ill defined for ~v" v))])
             (scimitar-val (scale-val v))]
            [(n (scimitar-typed (scimitar-mul (scimitar-typed (? const? e2) ty2) e3) _))
             (go (scimitar-mul
                   (scimitar-typed
                     (scimitar-mul
                       (scimitar-typed (scimitar-num n) (vecty (interval n n) '()))
                       (scimitar-typed e2 ty2))
                     (ty-scale (interval n n) ty2))
                   e3)
                 rho)]
            [(n (scimitar-typed (scimitar-plus (scimitar-typed e2 ty2) (scimitar-typed e3 ty3)) _))
             (go (scimitar-plus
                   (scimitar-typed
                     (scimitar-mul
                       (scimitar-typed
                         (scimitar-num n)
                         (vecty (interval n n) '()))
                       (scimitar-typed e2 ty2))
                     (ty-scale (interval n n) ty2))
                   (scimitar-typed
                     (scimitar-mul
                       (scimitar-typed
                         (scimitar-num n)
                         (vecty (interval n n) '()))
                       (scimitar-typed e3 ty3))
                     (ty-scale (interval n n) ty3)))
                 rho)]
            [(n e2)
             (scimitar-mul
               (scimitar-typed
                 (scimitar-num n)
                 (vecty (interval n n) '()))
               e2)])]
         [(e1 (scimitar-typed (? get-num e2) ty2))
          (go (scimitar-mul (scimitar-typed e2 ty2) e1) rho)]
         [(e1 e2) (scimitar-mul e1 e2)])
       [(or (scimitar-mul (scimitar-typed (app get-num 0) _) (scimitar-typed _ ty))
            (scimitar-mul (scimitar-typed _ ty) (scimitar-typed (app get-num 0) _)))
        (make-zero ty)]
       [(or (scimitar-mul (scimitar-typed (app get-num 1) _) (scimitar-typed e _))
            (scimitar-mul (scimitar-typed e _) (scimitar-typed (app get-num 1) _)))
        e]
       [e e])]
    [((or (scimitar-lt e1 e2)
          (scimitar-eq e1 e2)
          (scimitar-gt e1 e2)) _)
     (let-values ([(op vop sop)
                   (cond [(scimitar-lt? e) (values < vec< scimitar-lt)]
                         [(scimitar-eq? e) (values = vec= scimitar-eq)]
                         [(scimitar-gt? e) (values > vec> scimitar-gt)])])
       (match* ((go e1 rho) (go e2 rho))
         [((scimitar-typed (? get-num n) _) (scimitar-typed (? get-num m) _))
          (scimitar-num (if (op (get-num n) (get-num m)) 1 0))]
         [((scimitar-typed (scimitar-val v1) _)
           (scimitar-typed (scimitar-val v2) _))
          (define/match (cmp-val v1 v2)
            [((? vec?) (? vec?)) (vop v1 v2)]
            [((? list?) (? list?)) (andmap cmp-val v1 v2)]
            [(_ _) (error (format "Addition is ill defined for ~v and ~v" v1 v2))])
          (scimitar-num (if (cmp-val v1 v2) 1 0))]
         [(e1 e2)
          (sop e1 e2)]))]
    [((scimitar-ref vec ix) _)
     (match* ((go vec rho) (go ix rho))
       [((scimitar-typed (scimitar-val vec) _)
         (scimitar-typed (? scimitar-ix? ix) _))
        (let ((ix (scimitar-ix->measurement ix))
              (s (vec-shape vec)))
          (if ((measurement-strictly-exceeds ix) s)
            (scimitar-num (vec! vec ix))
            (error (format "During const reduce: Index ~v lies outside vector of shape ~v" ix s))))]
       [((scimitar-typed (scimitar-num n) _)
         (scimitar-typed (scimitar-val '()) _))
        (scimitar-num n)]
       [(vec ix) (scimitar-ref vec ix)])]
    [((scimitar-if eg et ef) _)
     (match* ((go eg rho) (go et rho) (go ef rho))
       [((scimitar-typed (? get-num g) _) (scimitar-typed et _) (scimitar-typed ef _))
        (if (zero? (get-num g)) ef et)]
       [(eg et ef) (scimitar-if eg et ef)])]
    [((scimitar-for i en em eb) _)
     (match* ((go en rho) (go em rho))
       [((scimitar-typed (app get-num (? values n)) _)
         (scimitar-typed (app get-num (? values m)) _))
        (let ((tyi (second (or (env-assoc i (scimitar-expr->env eb)) (list #f (unitty))))))
          (when (> 0 (- m n))
            (error "Upper bound of for is less than lower bound"))
          (define (subst-i j)
            (subst `((,i ,(scimitar-typed (scimitar-num j) tyi))) eb))
          (go (scimitar-begin
                (build-list (- m n)
                  (lambda (i)
                    (subst-i (+ i n)))))
              rho))]
       [(en em)
        (scimitar-for i en em (go eb (env-remove i rho)))])]
    [((scimitar-sum i en em eb) _)
     (match* ((go en rho) (go em rho))
       [((scimitar-typed (app get-num (? values n)) tyn)
         (scimitar-typed (app get-num (? values m)) tym))
        (let ((tyb (scimitar-typed-ty eb))
              (tyi (second (or (env-assoc i (scimitar-expr->env eb)) (list #f (unitty))))))
          (when (> 0 (- m n))
            (error "Upper bound of sum is less than lower bound"))
          (define (subst-i j)
            (subst `((,i ,(scimitar-typed (scimitar-num j) tyi))) eb))
          (cond
            [(= 0 (- m n)) (make-zero tyb)]
            [(= 1 (- m n)) (scimitar-typed-e (go eb (env-add i (scimitar-num n) rho)))]
            [(const? (scimitar-typed-e (go eb (env-add i (scimitar-num n) rho))))
              (go
                (scimitar-typed-e
                  (foldl1
                    (lambda (e a)
                      (let* ((tye (scimitar-typed-ty e))
                             (tya (scimitar-typed-ty a))
                             (ty (ty-msum tye tya)))
                        (scimitar-typed (scimitar-plus e a) ty)))
                    (build-list (- m n)
                      (lambda (i)
                        (subst-i (+ i n))))))
                rho)]
            [else
              (scimitar-sum i (scimitar-typed (scimitar-num n) tyn)
                              (scimitar-typed (scimitar-num m) tym)
                              (go eb (env-remove i rho)))]))]
       [(en em)
        (scimitar-sum i en em (go eb (env-remove i rho)))])]
    [((scimitar-constraint lhs ineq rhs) _)
     (match* ((go lhs rho) ineq (go rhs rho))
       [((scimitar-typed (? get-num el) _) _ (scimitar-typed (? get-num er) _))
        (if ((ineq->cmp ineq) (get-num el) (get-num er))
          (scimitar-val '()) ;; this constraint is trivially satisfied
          (scimitar-infeasible))] ;; this constraint is trivially unsatisfiable
       [((scimitar-typed (? const? el) _) '= (scimitar-typed (? const? er) _))
        (if (equal? el er)
          (scimitar-val '()) ;; this constraint is trivially satisfied
          (scimitar-infeasible))] ;; this constraint is trivially unsatisfiable
       [(lhs ineq rhs)
        (scimitar-constraint lhs ineq rhs)])]
    [((scimitar-symbolic) _)
     e]
    [((scimitar-infeasible) _)
     e])
  (go e (env-empty const?)))
