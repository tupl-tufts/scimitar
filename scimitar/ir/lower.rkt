#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "../ty-def.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "error.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  poly-env?
  poly-env->env
  ir-dec-lower)

(define val-env?
  (envof natural?))

(define poly-env?
  (envof poly?))

(define/contract (poly-env->env phi)
  (-> poly-env? env?)
  (env-map-vals poly-ty phi ty?))

(define (ty-width ty)
  (match ty
    [(polyty tyi (list _ w) tyo) (max w (+ (ty-dim tyi) (ty-dim tyo)))]
    [_ (ty-dim ty)]))

(define (env->val-env Gamma)
  (let ((es (env-entries Gamma)))
    (env
      (zip
        (map car es)
        (init
          (scanl + 0 (map (compose ty-width cadr) es))))
      natural?)))

(define/contract (ir-var-lower e sigma Gamma)
  (-> ir-var? val-env? env? vec?)
  (match e
    [(ir-var x)
     (match* ((env-assoc x sigma)
              (env-assoc x Gamma))
       [((list _ k) (list _ ty))
        (vec-left-pad k (I (ty-dim ty)))]
       [(_ _) (error (format "During lowering: Variable ~v not found" x))])]))

(define/contract (ir-expr-lower e sigma Gamma)
  (-> ir-expr? val-env? env? vec?)
  (match e
    [(? ir-val?) (error "bug in ir-expr-lower")]
    [(ir-var x)
     (ir-var-lower e sigma Gamma)]
    [(ir-add e1 e2)
     (let* ((v1 (ir-expr-lower e1 sigma Gamma))
            (v2 (ir-expr-lower e2 sigma Gamma))
            (shape (shape-max (vec-shape v1) (vec-shape v2))))
       (vec+ (vec-pad v1 shape) (vec-pad v2 shape)))]
    [(ir-scmul (ir-num n) e)
     (let* ((v (ir-expr-lower e sigma Gamma)))
       (vec.* n v))]
    [(? ir-var-mul?) (error "bug in ir-expr-lower")]
    [(ir-tuple es)
     (let* ((vs (map (curryr ir-expr-lower sigma Gamma) es))
            (max-width (cadr (apply shape-max (map vec-shape vs))))
            (vs (map (lambda (v) (vec-pad v `(,(vec-length v) ,max-width))) vs)))
       (apply vec-augment 0 vs))]
    [(ir-vec-ix e (? ir-ix? ixs))
     (let* ((v (ir-expr-lower e sigma Gamma))
            (flat-ix (measurement-flatten
                       (ir-ix->measurement ixs)
                       (vecty-shape (second (env-assoc (ir-var-name e) Gamma))))))
       (vec-project v
         `(,flat-ix 0)
         `(,(+ 1 flat-ix) ,(vec-width v))))]
    [(ir-cast e ty)
     (ir-expr-lower e sigma Gamma)]
    [(ir-poly-app (ir-var f) _)
     ;; NOTE: This ONLY puts in the call's result marshalling.
     ;;       The argument marshalling is in ir-stmt-lower.
     ;;       The call itself is inlined in ir-dec-lower.
     (match (env-assoc f Gamma)
       [`(,_ ,(polyty _ _ tyo))
        (let ((Gamma (env-set f tyo Gamma)))
          (ir-expr-lower (ir-var f) sigma Gamma))])]
    [else
      (ir-unexpected "lowering" e)]))

(define/contract (ir-stmt-lower s sigma Gamma)
  (-> ir-stmt? val-env? env? constraint?)
  (match s
    [(ir-constraint (? ir-var? x) '= (ir-vec v))
     (let* ((ve (ir-var-lower x sigma Gamma)))
       (constraint ve '= (vec-flatten v)))]
    [(ir-constraint (? ir-var? x) ineq (or (ir-num n) (and (ir-unit) (app (lambda _ 0) n))))
     (let* ((v (ir-var-lower x sigma Gamma))
            (l (vec-length v)))
       (constraint v ineq (vec-n `(,l) n)))]
    [(ir-constraint lhs ineq (ir-num n))
     (let* ((v (ir-expr-lower lhs sigma Gamma))
            (l (vec-length v))
            (c (constraint v ineq (vec-n `(,l) n)))
            (cs (ir-foldr ir-poly-app?
                  (match-lambda*
                    [`(,(ir-poly-app (ir-var f) args) ,acc)
                     ;; NOTE: This ONLY puts in the call's argument marshalling.
                     ;;       The result marshalling is in ir-expr-lower.
                     ;;       The call itself is inlined in ir-dec-lower.
                     (match* ((env-assoc f sigma) (env-assoc f Gamma))
                       [((list _ k) (list _ (polyty tyi _ tyo)))
                        (let* ((tmp (gensym f))
                               (sigma (env-add tmp (+ k (ty-dim tyo)) sigma))
                               (Gamma (env-add tmp tyi Gamma)))
                          ; linearization leaves vals directly in call position.
                          (if (ir-val? args)
                            (cons (ir-stmt-lower (ir-constraint (ir-var tmp) '= args) sigma Gamma) acc)
                            (let* ((v (ir-expr-lower (ir-sub (ir-var tmp) args) sigma Gamma))
                                   (l (vec-length v)))
                              (cons (constraint v '= (vec-0 `(,l))) acc))))]
                       [(_ _) (error (format "During lowering: Poly ~v not found" f))])])
                  `(,c)
                  lhs)))
       (constraint-concat cs))]
    [(ir-constraint (? ir-var? x) '= (ir-var-mul e1 e2))
     (let* ((v  (ir-expr-lower x  sigma Gamma))
            (v1 (ir-expr-lower e1 sigma Gamma))
            (v2 (ir-expr-lower e2 sigma Gamma))
            (vb v)
            (v1b v1)
            (v2b v2)
            (i1 (scalarty-interval (cadr (env-assoc (ir-var-name e1) Gamma))))
            (i2 (scalarty-interval (cadr (env-assoc (ir-var-name e2) Gamma))))
            (i1inf (interval-inf i1))
            (i1sup (interval-sup i1))
            (i2inf (interval-inf i2))
            (i2sup (interval-sup i2))
            (shape (shape-max (vec-shape v) (vec-shape v1) (vec-shape v2)))
            (v  (vec-negate (vec-pad v  shape)))
            (v1 (vec-pad v1 shape))
            (v2 (vec-pad v2 shape))
            (i2inf.*v1 (vec.* i2inf v1))
            (i2sup.*v1 (vec.* i2sup v1))
            (i1inf.*v2 (vec.* i1inf v2))
            (i1sup.*v2 (vec.* i1sup v2))
            (lhs1 (vec+ i1inf.*v2 i2inf.*v1 v))
            (lhs2 (vec+ i1sup.*v2 i2sup.*v1 v))
            (lhs3 (vec+ i1sup.*v2 i2inf.*v1 v))
            (lhs4 (vec+ i1inf.*v2 i2sup.*v1 v))
            (rhs1 (vec-scalar (* i1inf i2inf) 1))
            (rhs2 (vec-scalar (* i1sup i2sup) 1))
            (rhs3 (vec-scalar (* i1sup i2inf) 1))
            (rhs4 (vec-scalar (* i1inf i2sup) 1))
            (eq14 (and (equal? lhs1 lhs4) (equal? rhs1 rhs4)))
            (eq23 (and (equal? lhs2 lhs3) (equal? rhs2 rhs3))))
       ;; take the convex relaxation using McCormick envelopes
       (match* (eq14 eq23)
         [(#t #t)
          (constraint-append
            (constraint lhs1 '= rhs1)
            (constraint lhs2 '= rhs2))]
         [(#t #f)
          (constraint-append
            (constraint lhs1 '= rhs1)
            (constraint lhs2 '<= rhs2)
            (constraint lhs3 '>= rhs3))]
         [(#f #t)
          (constraint-append
            (constraint lhs1 '<= rhs1)
            (constraint lhs2 '= rhs2)
            (constraint lhs4 '>= rhs4))]
         [(#f #f)
          (constraint-append
            (constraint lhs1 '<= rhs1)
            (constraint lhs2 '<= rhs2)
            (constraint lhs3 '>= rhs3)
            (constraint lhs4 '>= rhs4))]))]
    [else
      (ir-unexpected "lowering" s)]))

(define/contract (ir-dec-lower d phi)
  (-> ir-dec? poly-env? poly?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (let*-values ([(ss_phi ss)
                    (partition
                      (match-lambda
                        [(ir-constraint (? ir-var?) '= (? ir-poly?)) #t] [_ #f]) ss)])
       (let* ((phi (env-union-key
                     (env (map (match-lambda
                                 [(ir-constraint (ir-var x) _ (ir-poly p)) `(,x ,p)])
                               ss_phi)
                          poly?)
                     phi))
              ;; Poly arguments/results require unique variables in sigma.
              ;; Otherwise, all applications would use the same inlined code,
              ;; but each application requires a fresh inlining from phi.  To
              ;; achieve that, rip through and give every applied poly a fresh
              ;; name mapped to the original.
              (omicron (box '()))
              (ss (map (match-lambda
                         [(ir-constraint lhs ineq rhs)
                          (define/match (rename-calls e)
                            [((ir-poly-app (ir-var f) a))
                             (let ((g (gensym f)))
                               (set-box! omicron
                                 (cons `(,g ,f) (unbox omicron)))
                               (ir-poly-app (ir-var g) a))])
                          (ir-constraint
                            (ir-fmap ir-poly-app? rename-calls lhs)
                            ineq
                            (ir-fmap ir-poly-app? rename-calls rhs))])
                       ss))
              (omicron (unbox omicron))
              (phi (env (map (lambda (f_s f_p)
                               (match (env-assoc f_p phi)
                                 [(list _ p) (list f_s p)]
                                 [#f (error (format "During lowering: Poly ~v not found" f_p))]))
                             (map car omicron)
                             (map cadr omicron))
                        poly?))
              (Gamma_l (env-remove* (map cadr omicron) Gamma_l))
              (Gamma_l (env-union-key Gamma_l (poly-env->env phi)))
              (Gamma_r (if (env-empty? Gamma_r) (env `((,(gensym 'unit-arg) ,(unitty)))) Gamma_r))
              (Gamma_f (if (env-empty? Gamma_f) (env `((,(gensym 'unit-arg) ,(unitty)))) Gamma_f))
              (Gamma (env-union-key Gamma_r Gamma_f Gamma_l))
              (sigma (env->val-env Gamma))
              (Gamma_col (apply env-union-key
                           (map (lambda (fty)
                                  (let ((f (car fty))
                                        (ty (cadr fty)))
                                    (match (env-assoc f phi)
                                      [`(,_ ,p)
                                         (let ((w (box (ty-width (poly-ty p)))))
                                           (env-map (lambda (x ty)
                                                      (when (and (< (unbox w) (ty-dim ty)) (> (unbox w) 0))
                                                        (error "During lowering: A variable is split down the middle"))
                                                      (set-box! w (- (unbox w) (ty-dim ty)))
                                                      `(,(extend-symbol f x)
                                                         ,(if (< (unbox w) 0) (vecty zero '(0)) ty)))
                                                    (poly-Gamma-col p)))]
                                      [#f (env `(,fty))])))
                                (env-entries Gamma))))
              (cs (map (curryr ir-stmt-lower sigma Gamma) ss))
              (cs_phi (map (lambda (f)
                             (match* ((env-assoc f sigma)
                                      (env-assoc f phi))
                               [((list _ k) (list _ p))
                                (let* ((cs (poly-cs p))
                                       (h (vec-length (constraint-A cs))))
                                  (constraint-augment (vec-0 `(,h ,k)) cs))]
                               [(_ _) (error (format "During lowering: Poly ~v not found" f))]))
                           (map car omicron)))
              (tyi (env->ty Gamma_f))
              (tyo (env->ty Gamma_r)))
         (apply poly tyi tyo Gamma_col (append cs cs_phi))))]
    [else
      (ir-unexpected "lowering" d)]))
