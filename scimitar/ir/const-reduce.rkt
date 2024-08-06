#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "error.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "ty.rkt")

(provide
  const-env?
  ir-dec-const-reduce)

(define const-env?
  (envof number?))

(define/contract (const-expr? e)
  (-> ir-expr? boolean?)
  (match e
    [(ir-tuple es) (andmap const-expr? es)]
    [(? ir-val?) #t]
    [_ #f]))

(define/contract (ir-expr-const-reduce e Gamma rho)
  (-> ir-expr? env? const-env? ir-expr?)
  (define/match (go e)
    [((? ir-val?)) e]
    [((ir-var x))
     (match (env-assoc x rho)
       [(list _ n) (ir-num n)]
       [#f e])]
    [((ir-add e1 e2))
     ;; The following cases not only reduce, but normalize
     ;; addition so that nums are collected at the left.
     ;; There should only be one num per addition chain.
     (match
       (match* ((go e1) (go e2))
         [((? ir-unit?) e) e]
         [(e (? ir-unit?)) e]
         [((ir-tuple ts1) (ir-tuple ts2))
          (go (ir-tuple (map ir-add ts1 ts2)))]
         [((ir-vec v) (ir-vec u))
          (ir-vec (vec+ v u))]
         [((ir-vec v) (ir-add (ir-vec u) e))
          (ir-add (ir-vec (vec+ v u)) e)]
         [((ir-add (ir-vec v) e) (ir-vec u))
          (ir-add (ir-vec (vec+ v u)) e)]
         [((ir-add (ir-vec v) e1) (ir-add (ir-vec u) e2))
          (ir-add (ir-vec (vec+ v u)) (ir-add e1 e2))]
         [((ir-num m) (ir-num n))
          (ir-num (+ m n))]
         [((ir-num m) (ir-add (ir-num n) e))
          (ir-add (ir-num (+ m n)) e)]
         [((ir-add (ir-num m) e) (ir-num n))
          (ir-add (ir-num (+ m n)) e)]
         [((ir-add (ir-num m) e1) (ir-add (ir-num n) e2))
          (ir-add (ir-num (+ m n)) (ir-add e1 e2))]
         [((ir-add (? (or/c ir-num? ir-vec?) e) e1) e2)
          (ir-add e (ir-add e1 e2))]
         [(e1 (ir-add (? (or/c ir-num? ir-vec?) e) e2))
          (ir-add e (ir-add e1 e2))]
         [(e1 (? (or/c ir-num? ir-vec?) e2))
          (ir-add e2 e1)]
         [(e1 e2)
          (ir-add e1 e2)])
       [(ir-add (ir-num 0) e) e]
       [e e])]
    [((ir-scmul m e))
     (match
       (match* ((go m) (go e))
         ;; The following cases reduce and distribute
         ;; multiplication over addition and re-reduce.
         [((? ir-unit? u) _) u]
         [(_ (? ir-unit? u)) u]
         [((ir-num m) (ir-num n))
          (ir-num (* m n))]
         [((ir-num m) (ir-vec v))
          (ir-vec (vec.* m v))]
         [((ir-num m) (ir-scmul (ir-num n) e))
          (ir-scmul (ir-num (* m n)) e)]
         [((? ir-num? m) (ir-add e1 e2))
          (go (ir-add (ir-scmul m e1) (ir-scmul m e2)))]
         [((? ir-num? m) e)
          (ir-scmul m e)]
         [((ir-vec m) e)
           #:when (eq? (vec-shape m) '())
          (go (ir-scmul (ir-num (vec! m '())) e))]
         [(me _) (error (format "During const reduce: Expected ~a to evaluate to constant expression.~nInstead, it evaluated to ~v" (ir-expr->string m) me))])
       [(ir-scmul (ir-num 0) e) (ir-num 0)]
       [(ir-scmul (ir-num 1) e) e]
       [e e])]
    [((ir-var-mul e1 e2))
     (match* ((go e1) (go e2))
       [((? const-expr? e1) e2) (go (ir-scmul e1 e2))]
       [(e1 (? const-expr? e2)) (go (ir-scmul e2 e1))]
       [(e1 e2)
        (let ((ty1 (ir-expr-check e1 Gamma)))
          (if (indicatorty? ty1)
            (ir-var-mul e1 e2)
            (let ((ty2 (ir-expr-check e2 Gamma)))
              (if (indicatorty? ty2)
                (ir-var-mul e1 e2)
                (error (format "Expected indicator variable in variable multiplication.~nInstead got ~v and ~v~nOriginal expression: ~v" ty1 ty2 (ir-expr->string e)))))))])]
    [((ir-tuple es))
     (ir-tuple (map go es))]
    [((ir-vec-ix e ix))
     (define (get-ix v ix)
       (if ((measurement-strictly-exceeds ix) (vec-shape v))
         (ir-num (vec! v ix))
         (error (format "During const reduce: Index ~v lies outside vector of shape ~v" ix (vec-shape v)))))
     (match* ((go e) (go ix))
       [((ir-vec v) (? ir-ix? ix)) (get-ix v (ir-ix->measurement ix))]
       [(e ix) (ir-vec-ix e ix)])]
    [((ir-vec-sum id start end body))
     (match* ((go start) (go end))
       [((ir-num start) (ir-num end))
        (go
          (foldl
            ir-add
            (ir-num 0)
            (build-list (- end start)
              (lambda (i)
                (ir-expr-const-reduce
                  body
                  (env-add id (vecty (interval start end) '()) Gamma)
                  (env-add id (+ i start) rho))))))]
       [(se ee) (error (format "During const reduce: Expected ~a and ~a to evaluate to constant expressions.~nInstead, they respectively evaluated to ~v and ~v"
                               (ir-expr->string start) (ir-expr->string end) se ee))])]
    [((ir-cast e ty))
     (match* ((go e) ty)
       [((? ir-unit?) (unitty))
        (ir-unit)]
       [((? ir-unit?) (vecty i s))
        (ir-vec (vec-0 s i))]
       [((ir-num 0) (unitty))
        (ir-unit)]
       [((ir-num n) (vecty _ s))
        (ir-vec (vec-sparse `((,(map (lambda (d) 0) s) ,n)) ty))]
       [((ir-vec v) (? vecty?))
        (ir-vec (or (vec-dynamic-cast ty v)
                    (error (format "During const reduce: cast failed from ~v to ~v" (vec-ty v) ty))))]
       [((ir-tuple es) (tuplety ts ...))
        (go (ir-tuple (map ir-cast es ts)))]
       [((? ir-tuple? e) (? vecty?))
        #:when (const-expr? e)
        (let ((t (ir-val->val e)))
          (ir-vec
            (or (vec-dynamic-cast ty (val->vec t))
                (error (format "During const reduce: cast failed of ~v to ~v" t ty)))))]
       [(e ty)
        (ir-cast e ty)])]
    [((ir-poly-app f x))
     (match* ((go f) (go x))
       ; TODO: in theory, here we could simplify by directly solving const f with const xs
       [(f x) (ir-poly-app f x)])]
    [((ir-thunk e xs force))
     (let ((xs (map (lambda (x)
                      (match (env-assoc x rho)
                        [(list _ n) n]
                        [#f (error (format "When forcing the expression ~a,~n   the variable ~v was not defined" e x))]))
                    xs)))
       (apply force xs))]
    [(_)
      (ir-unexpected "const reduce" e)])
  (go e))

;; Normalizes statements into a list of constraints and subject-tos
(define/contract (ir-stmt-const-reduce s Gamma rho)
  (-> ir-stmt? env? const-env? (listof ir-stmt?))
  (match s
    [(ir-forall id start end body)
     (match* ((ir-expr-const-reduce start Gamma rho)
              (ir-expr-const-reduce end Gamma rho))
       [((ir-num start) (ir-num end))
        (concat
          (build-list (- end start)
            (lambda (i)
              (ir-stmt-const-reduce
                body
                (env-add id (vecty (interval start end) '()) Gamma)
                (env-add id (+ i start) rho)))))]
       [(se ee) (error (format "During const reduce: Expected ~a and ~a to evaluate to constant expressions.~nInstead, they respectively evaluated to ~v and ~v"
                               (ir-expr->string start) (ir-expr->string end) se ee))])]
    [(ir-subject-to Gamma_l ss)
     `(,(ir-subject-to Gamma_l (concat (map (curryr ir-stmt-const-reduce (env-union-key Gamma_l Gamma) rho) ss))))]
    [(ir-constraint lhs ineq rhs)
     (match (ir-expr-const-reduce (ir-sub lhs rhs) Gamma rho)
      [(? ir-unit?) '()] ;; () = 0
      [(ir-num n)
       (if ((ineq->cmp ineq) n 0)
         '()
         (error (format "During const reduce: expression ~a ~v ~a reduces to ~v ~v 0, which is false"
                        (ir-expr->string lhs) ineq (ir-expr->string rhs) n ineq)))]
      [(ir-add (ir-num n) e) `(,(ir-constraint e ineq (ir-num (- n))))]
      [(ir-scmul _ e) `(,(ir-constraint e ineq (ir-num 0)))]
      [e `(,(ir-constraint e ineq (ir-num 0)))])]
    [else
      (ir-unexpected "const reduce" s)]))

(define/contract (ir-dec-const-reduce d rho)
  (-> ir-dec? const-env? ir-dec?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (let* ((Gamma (env-union-key Gamma_l Gamma_r Gamma_f #;Gamma))
            (ss (concat (map (curryr ir-stmt-const-reduce Gamma rho) ss))))
       (ir-poly-dec f Gamma_f Gamma_r Gamma_l ss))]
    [else
      (ir-unexpected "const reduce" d)]))
