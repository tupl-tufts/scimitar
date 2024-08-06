#lang racket

(require "contract-utils.rkt")
(require "env.rkt")
(require "ty.rkt")
(require "util.rkt")

(provide
  (struct-out tyvar)
  (struct-out ty-plus)
  ty-minus
  (struct-out ty-mul)
  constraint?
  (struct-out ~)
  (struct-out <:)
  fresh-tyvar
  concrete-ty?
  (rename-out [ty-subst-concrete ty-subst])
  (rename-out [env-subst-concrete env-subst])
  solve)

(struct/contract
  tyvar ty
  ([name symbol?])
  #:transparent)

(struct/contract
  ty-op ty
  ([ty1 ty?]
   [ty2 ty?])
  #:transparent)

(struct
  ty-plus ty-op ()
  #:transparent)

(define (ty-minus t1 t2)
  (ty-plus t1 (ty-mul (vecty (interval -1 -1) '()) t2)))

(struct
  ty-mul ty-op ()
  #:transparent)

(struct/contract
  constraint
  ([ty1 ty?]
   [ty2 ty?])
  #:transparent)

(struct
  ~ constraint ()
  #:transparent)

(struct
  <: constraint ()
  #:transparent)

(define (fresh-tyvar)
  (tyvar (gensym "alpha")))

(define (constraint-ctor c)
  (match c [(~ _ _) ~] [(<: _ _) <:]))

(define/contract (concrete-ty? ty)
  (-> ty? boolean?)
  (match ty
    [(? tyvar?) #f]
    [(? ty-op?) #f]
    [(? vecty?) #t]
    [(? unitty?) #t]
    [(tuplety tys ...) (andmap concrete-ty? tys)]
    [(polyty tyi _ tyo) (and (concrete-ty? tyi) (concrete-ty? tyo))]))

(define/contract (ty-subst ty theta)
  (-> ty? env? ty?)
  (define (go ty)
    (match ty
      [(tyvar alpha)
       (match (env-assoc alpha theta)
         [`(,_ ,ty) ty]
         [#f (tyvar alpha)])]
      [(ty-mul t1 t2)
       (define (subst-op t1 t2)
         (match (list (go t1) (go t2))
           [(or (list (vecty i '()) (? concrete-ty? t))
                (list (? concrete-ty? t) (vecty i '())))
            (ty-scale i t)]
           [(or (list t (polyty d s c))
                (list (polyty d s c) t))
            (polyty (subst-op t d) s (subst-op t c))]
           [(or (list t (tuplety ts ...))
                (list (tuplety ts ...) t))
            (apply tuplety (map (curryr subst-op t) ts))]
           [(list t1 t2) (ty-mul t1 t2)]))
       (subst-op t1 t2)]
      [(ty-plus t1 t2)
       (define (subst-op t1 t2)
         (match (list (go t1) (go t2))
           [(list (? concrete-ty? t1) (? concrete-ty? t2))
            (ty-msum t1 t2)]
           [(list (polyty d1 s1 c1) (polyty d2 s2 c2))
            (polyty (subst-op d1 d2) (map + s1 s2) (subst-op c1 c2))]
           [(list (tuplety t1s ...) (tuplety t2s ...))
            #:when (= (length t1s) (length t2s))
            (apply tuplety (map subst-op t1s t2s))]
           [(list t1 t2) (ty-plus t1 t2)]))
       (subst-op t1 t2)]
      [(polyty d s c) (polyty (go d) s (go c))]
      [(tuplety ts ...) (apply tuplety (map go ts))]
      [_ ty]))
  (go ty))

(define (ty-subst-concrete ty theta)
  (let ((ty (ty-subst ty theta)))
    (if (concrete-ty? ty)
      ty
      (error (format "During type inference, type ~v contains unknown types" ty)))))

(define/contract (subst c theta)
  (-> constraint? env? constraint?)
  (match c
    [(constraint ty1 ty2)
     ((constraint-ctor c)
      (ty-subst ty1 theta)
      (ty-subst ty2 theta))]))

(define/contract (freein ty)
  (-> ty? (listof symbol?))
  (match ty
    [(tyvar alpha) `(,alpha)]
    [(ty-op t1 t2) (append (freein t1) (freein t2))]
    [(polyty d s c) (append (freein d) (freein c))]
    [(tuplety ts ...) (concat (map freein ts))]
    [_ '()]))

(define/contract (env-subst Gamma theta)
  (-> env? env? env?)
  (env-map-vals
    (curryr ty-subst theta)
    Gamma))

(define (env-subst-concrete Gamma theta)
  (let* ((Gamma (env-subst Gamma theta))
         (tyvars (map car (filter (compose not concrete-ty? cadr) (env-entries Gamma)))))
    (if (null? tyvars)
      Gamma
      (error (format "During type inference, variables ~v have unknown types" tyvars)))))

(define/contract (solve phase cs #:allow-unsolvable [allow-unsolvable #f])
  (-> symbol? (listof constraint?) env?)
  (define (unify c theta)
    (match (subst c theta)
      [(~ (tyvar alpha) t2)
       #:when (equal? t2 (tyvar alpha))
       (values theta '())]
      [(or (~ (tyvar alpha) t2)
           (~ t2 (tyvar alpha)))
       #:when (member alpha (freein t2))
       (error (format "During ~a type inference, unsatisfiable constraint ~v ~~ ~v" phase alpha t2))]
      [(or (~ (tyvar alpha) t2)
           (~ t2 (tyvar alpha)))
       (let* ((theta2 (env-subst theta (env `((,alpha ,t2)))))
              (theta3 (env-add alpha t2 theta2)))
         (values theta3 '()))]
      [(or (~ (? ty-op? t1) t2)
           (~ t1 (? ty-op? t2)))
       (let* ((alpha (fresh-tyvar))
              (beta (fresh-tyvar))
              (theta2 (env-add alpha t1 theta))
              (theta3 (env-add beta  t2 theta2)))
         (values theta3 `((,alpha ,beta))))]
      [(~ (polyty d1 _ c1) (polyty d2 _ c2))
       (let*-values ([(theta2 ab-equiv2) (unify (~ d2 d1) theta)]
                     [(theta3 ab-equiv3) (unify (~ c1 c2) theta2)])
         (values theta3 (append ab-equiv2 ab-equiv3)))]
      [(~ (tuplety t1s ...) (tuplety t2s ...))
       #:when (= (length t1s) (length t2s))
       (unify-list (map ~ t1s t2s) theta)]
      [(~ t1 t2)
       (if (and (ty-<: t1 t2) (ty-<: t2 t1))
         (values theta '())
         (error (format "During ~a type inference, unsatisfiable constraint ~v ~~ ~v" phase t1 t2)))]))
  (define (unify-list cs theta0)
    (for/fold ([theta theta0] [ab-equiv '()])
              ([c cs])
      (let-values ([(theta2 ab-equiv2) (unify c theta)])
        (values theta2 (append ab-equiv ab-equiv2)))))
  ;; a separate pass is required for subtyping
  ;; because substitution is done differently.
  ;; these are very similar, but subtly different
  ;; enough that it merits its own function.
  (define (preprocess c)
    (match c
      [(<: (tyvar alpha) t2)
       #:when (equal? t2 (tyvar alpha))
       (values '() '())]
      [(or (<: (tyvar alpha) t2)
           (<: t2 (tyvar alpha)))
       #:when (member alpha (freein t2))
       (error (format "During ~a type inference, unsatisfiable constraint ~v <: ~v" phase alpha t2))]
      [(<: _ (tyvar alpha))
       (values `(,c) '())]
      [(<: (tyvar alpha) _)
       (values '() `(,c))]
      [(or (<: (? ty-op? t1) t2)
           (<: t1 (? ty-op? t2)))
       (let ((alpha (fresh-tyvar)))
         (values `(,(<: t1 alpha)) `(,(<: alpha t2))))]
      [(<: (polyty d1 _ c1) (polyty d2 _ c2))
       (let-values ([(subs-d sups-d) (preprocess (<: d2 d1))]
                    [(subs-c sups-c) (preprocess (<: c1 c2))])
         (values (append subs-c subs-d) (append sups-c sups-d)))]
      [(<: (tuplety t1s ...) (tuplety t2s ...))
       #:when (= (length t1s) (length t2s))
       (let-values ([(subss supss) (for/lists (subss supss) ([st (map <: t1s t2s)]) (preprocess st))])
         (values (concat subss) (concat supss)))]
      [(<: t1 t2)
       (if (ty-<: t1 t2)
         (values '() '())
         (error (format "During ~a type inference, unsatisfiable constraint ~v <: ~v" phase t1 t2)))]))
  (define (subty-subst sigma)
    (define (go alphas sigma)
      (if (null? alphas)
        sigma
        (match (assoc (car alphas) sigma)
          [`(,alpha ,ty1s)
           (go (cdr alphas)
               (map-cadr
                 (lambda (ty2s)
                   (concat
                     (map (lambda (ty2)
                            (if (member alpha (freein ty2))
                              (map (lambda (ty1)
                                     (ty-subst ty2 (env `((,alpha ,ty1)))))
                                   ty1s)
                              `(,ty2)))
                          ty2s)))
                 sigma))])))
    (go (map car sigma) sigma))
  (define (biunify cs)
    (define (group-by-tyvar group-ty grouped-ty cs)
      (let* ((gs (group-by group-ty cs))
             (sigma (map (lambda (stg)
                           `(,(tyvar-name (group-ty (car stg)))
                              ,(map grouped-ty stg)))
                         gs)))
        sigma))
    (define (concrete? s)
      (and (= 1 (length s))
           (concrete-ty? (only s))))
    (define (update lattice-op sigma theta)
      (define (compact tys)
        (define/match (go tys)
          [(`(,ty))
           (if (concrete-ty? ty)
             (values ty '())
             (values #f `(,ty)))]
          [(`(,ty . ,tys))
           (let-values ([(ty-prime tys) (go tys)])
             (if (concrete-ty? ty)
               (if ty-prime
                 (values (lattice-op ty ty-prime) tys)
                 (values ty tys))
               (values ty-prime (cons ty tys))))])
        (let-values ([(ty tys) (go tys)])
          (if ty
            (cons ty tys)
            tys)))
      (let* ((sigma (map-cadr compact sigma))
             (theta-prime (env (map-cadr only (filter-cadr concrete? sigma))))
             (theta (env-union-key theta-prime theta))
             (sigma (filter-cadr (compose not concrete?) sigma)))
        (values theta-prime theta sigma)))
    (define (subseteq xs ys)
      (andmap (curryr member ys) xs))
    (define (freein-sigma sigma)
      (flatten (map (compose (curry map freein) cadr) sigma)))
    (define (concretize sigma theta)
      (define (all-in-theta? x-tys)
        (subseteq (concat (map freein (cadr x-tys))) (env-dom theta)))
      (define ((any-in-ftvs? ftvs) c)
        (ormap (curryr member ftvs) (concat (map freein (cadr c)))))
      (let*-values ([(concrete abstract) (partition all-in-theta? sigma)]
                    [(ftvs) (freein-sigma abstract)]
                    [(also-abstract actually-concrete) (partition (any-in-ftvs? ftvs) concrete)])
        (append (map-cadr (curry map (curryr ty-subst theta)) actually-concrete) also-abstract abstract)))
    (define (isolates sigma sigma-bad)
      (define (collect-sigma sigmas)
        (map (lambda (ss)
               (list (caar ss) (concat (map cadr ss))))
             (group-by car sigmas)))
      (define (go tys)
        (let*-values ([(tvs tys) (partition tyvar? tys)]
                      [(ty-cs ty-as) (partition concrete-ty? tys)]
                      [(sigma) (if (null? ty-cs) '() (map (curryr list ty-cs) (map tyvar-name tvs)))])
          (cond
            [(null? ty-as) sigma]
            [(andmap tuplety? tys)
             (let ((sigmas (map go (unzip (map tuplety-tys tys)))))
               (collect-sigma (apply append sigma sigmas)))]
            [(andmap polyty? tys)
             (let* ((sigma-dom (go (map polyty-dom tys)))
                    (sigma-cod (go (map polyty-cod tys))))
               (collect-sigma (append sigma sigma-dom sigma-cod)))]
            [else sigma])))
      (let* ((ftvs (freein-sigma sigma))
             (ftvs-bad (append (map car sigma-bad) (freein-sigma sigma-bad)))
             (ftvs-iso (remove* ftvs-bad ftvs))
             (sigma-iso (filter (lambda (s) (andmap (lambda (ty) (subseteq (freein ty) ftvs-iso)) (cadr s))) sigma)))
        (collect-sigma (concat (map (compose go cadr) sigma-iso)))))
    (define (check-conflicts theta theta-prime mode)
      (let* ((order (if (equal? mode 'sub) (lambda (f a b) (f a b)) (lambda (f a b) (f b a))))
             (conflicts (filter (lambda (xty) (not (order ty-<: (cadr (env-assoc (car xty) theta)) (cadr xty))))
                                (env-entries (env-filter-keys (env-dom theta) theta-prime))))
             (conflict-strs (map (lambda (c) (order (curry format "~v <: ~v") (car c) (cadr c))) conflicts)))
        (when (not (null? conflicts))
          (error (format "During ~a type inference, unsatisfiable constraints: ~a" phase
                         (apply string-append (intersperse ", " conflict-strs)))))))
    (define (go sigma-sub sigma-sup theta-sup theta-sub)
      (let*-values (;; Propagate all tyvars in sigma-sup that only occur on the right side and aren't in sigma-sub
                    [(sigma-sup) (subty-subst (append sigma-sup (isolates sigma-sup sigma-sub)))]
                    ;; Propagate all tyvars in sigma-sub that only occur on the right side and aren't in sigma-sup
                    [(sigma-sub) (subty-subst (append sigma-sub (isolates sigma-sub sigma-sup)))]
                    [(theta-sup-prime theta-sup sigma-sup) (update ty-least-upper-bound    sigma-sup theta-sup)]
                    [(theta-sub-prime theta-sub sigma-sub) (update ty-greatest-lower-bound sigma-sub theta-sub)]
                    ;; Substitute into sigma-sub all alpha <: ty s.t. ftvs(ty) subseteq theta-sup
                    ;; and no member of ftvs(ty) is in a constraint that contains tyvars not in theta-sup
                    [(sigma-sub) (concretize sigma-sub theta-sup)]
                    ;; Likewise for sigma-sup and theta-sub
                    [(sigma-sup) (concretize sigma-sup theta-sub)])
        ;; tyvars whose least upper bound is above its greatest lower bound
        (check-conflicts theta-sub theta-sup-prime 'sup)
        (check-conflicts theta-sup theta-sub-prime 'sub)
        (if (and (null? sigma-sup) (null? sigma-sub))
          ;; there are no more constraints with tyvars in them
          (values theta-sub theta-sup)
          (if (and (env-empty? theta-sup-prime) (env-empty? theta-sub-prime))
            ;; we didn't make any progress this round
            (if allow-unsolvable
              (values theta-sub theta-sup)
              (error (format "During ~a type inference, cannot solve for ~v" phase
                             (append (map car sigma-sup) (map car sigma-sub)))))
            (go sigma-sub sigma-sup theta-sup theta-sub)))))
    (let*-values ([(sts eqs) (partition <:? cs)]
                  [(theta ab-equiv) (unify-list eqs (env-empty))]
                  [(sts) (append sts (map (lambda (ab) (<: (cadr ab) (car ab))) ab-equiv)
                                     (map (lambda (ab) (<: (car ab) (cadr ab))) ab-equiv))]
                  [(sts) (map (curryr subst theta) sts)]
                  [(supss subss) (for/lists (supss subss) ([st sts]) (preprocess st))]
                  [(sups subs) (values (concat supss) (concat subss))]
                  [(sigma-sup) (subty-subst (group-by-tyvar constraint-ty2 constraint-ty1 sups))]
                  [(sigma-sub) (subty-subst (group-by-tyvar constraint-ty1 constraint-ty2 subs))]
                  [(theta-sub theta-sup) (go sigma-sub sigma-sup (env-empty) (env-empty))]
                  [(theta) (env-subst (env-subst theta theta-sup) theta-sub)])
      (check-conflicts theta theta-sup 'sup)
      (check-conflicts theta theta-sub 'sub)
      (env-union-key theta theta-sup theta-sub)))
  (biunify cs))

(module+ test
  (require rackunit)
  (define sum-to-n-cs
    (list (<: (tyvar 'gamma) (tyvar 'delta))
          (<: (vecty (interval 100 100) '()) (tyvar 'delta))
          (<: (vecty (interval 0 0) '()) (tyvar 'beta))
          (<: (tyvar 'alpha) (tyvar 'beta))
          (<: (tyvar 'beta) (vecty (interval -10000 10000) '()))
          (<: (tyvar 'beta) (tyvar 'gamma))
          (<: (vecty (interval 0 20) '()) (tyvar 'alpha))
          (<: (vecty (interval 1 1) '()) (tyvar 'alpha))
          (<: (tyvar 'epsilon) (vecty (interval 0 20) '()))))
  (define sum-to-n-G
    (env `((beta    ,(vecty (interval 0 20) '()))
           (epsilon ,(vecty (interval 0 20) '()))
           (delta   ,(vecty (interval 0 100) '()))
           (gamma   ,(vecty (interval 0 20) '()))
           (alpha   ,(vecty (interval 0 20) '())))))
  (check env-equiv? (solve 'sum-to-n-test sum-to-n-cs) sum-to-n-G)
  )
