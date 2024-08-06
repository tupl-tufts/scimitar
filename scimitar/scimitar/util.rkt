#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "string.rkt")
(require "../ir/grammar.rkt")
(require "../ir/util.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../vec.rkt")

(provide
  scimitar-free-in
  subst
  scimitar-expr->env
  scimitar-expr->ir-expr
  scimitar-expr->ir-stmt
  scimitar-typed-list->scimitar-typed
  scimitar-expr-list->scimitar-expr
  scimitar-expr->scimitar-expr-list
  scimitar-fmap
  scimitar-lower-objective
  scimitar-ix? scimitar-ix->measurement)

(define/contract (scimitar-free-in e)
  (-> scimitar-expr? (listof symbol?))
  (match e
    [(scimitar-solve dir res obj locals problems)
     (remove* locals (concat (map scimitar-free-in problems)))]
    [(scimitar-typed e _)
     (scimitar-free-in e)]
    [(scimitar-var x)
     `(,x)]
    [(scimitar-val _)
     '()]
    [(scimitar-poly _)
     '()]
    [(scimitar-tuple es)
     (concat (map scimitar-free-in es))]
    [(scimitar-switch _ e es)
     (concat (cons (scimitar-free-in e) (map scimitar-free-in es)))]
    [(scimitar-app f args)
     (append (scimitar-free-in f) (scimitar-free-in args))]
    [(scimitar-lambda ps body)
     (remove* (flatten ps) (scimitar-free-in body))]
    [(scimitar-fix fs e)
     (concat (remove* (map car fs) (cons (scimitar-free-in e) (map (compose scimitar-free-in cadr) fs))))]
    [(scimitar-let bs e)
     (concat (cons (remove* (map car bs) (scimitar-free-in e))
                   (map (compose scimitar-free-in cadr) bs)))]
    [(scimitar-let* bs e)
     (foldr (lambda (b fvs)
              (append (scimitar-free-in (cadr b)) (remove (car b) fvs)))
            (scimitar-free-in e) bs)]
    [(scimitar-begin es)
     (concat (map scimitar-free-in es))]
    [(scimitar-num _)
     '()]
    [(scimitar-plus e1 e2)
     (append (scimitar-free-in e1) (scimitar-free-in e2))]
    [(scimitar-minus e1 e2)
     (append (scimitar-free-in e1) (scimitar-free-in e2))]
    [(scimitar-mul e1 e2)
     (append (scimitar-free-in e1) (scimitar-free-in e2))]
    [(scimitar-lt e1 e2)
     (append (scimitar-free-in e1) (scimitar-free-in e2))]
    [(scimitar-eq e1 e2)
     (append (scimitar-free-in e1) (scimitar-free-in e2))]
    [(scimitar-gt e1 e2)
     (append (scimitar-free-in e1) (scimitar-free-in e2))]
    [(scimitar-ref vec ix)
     (append (scimitar-free-in vec) (scimitar-free-in ix))]
    [(scimitar-if eg et ef)
     (append (scimitar-free-in eg) (scimitar-free-in et) (scimitar-free-in ef))]
    [(scimitar-for i en em eb)
     (append (scimitar-free-in en) (scimitar-free-in em) (remove i (scimitar-free-in eb)))]
    [(scimitar-sum i en em eb)
     (append (scimitar-free-in en) (scimitar-free-in em) (remove i (scimitar-free-in eb)))]
    [(scimitar-constraint lhs ineq rhs)
     (append (scimitar-free-in lhs) (scimitar-free-in rhs))]
    [(scimitar-symbolic)
     '()]
    [(scimitar-infeasible)
     '()]))

(define/contract (subst yzs e)
  (-> (listof (list/c symbol? scimitar-expr?)) scimitar-expr? scimitar-expr?)
  (define/match (go e)
    [((scimitar-solve dir res obj locals ps)) (scimitar-solve dir res obj locals (map go ps))]
    [((scimitar-var _)) (error "BUG in inline-solve")]
    [((scimitar-typed (scimitar-var x) _)) (match (assoc x yzs) [`(,y ,z) z] [_ e])]
    [((scimitar-typed e ty)) (scimitar-typed (go e) ty) ]
    [((scimitar-val _)) e]
    [((scimitar-poly _)) e]
    [((scimitar-tuple es)) (scimitar-tuple (map go es))]
    [((scimitar-switch op e es)) (scimitar-switch op (go e) (map go es))]
    [((scimitar-app f args)) (scimitar-app (go f) (go args))]
    [((scimitar-lambda ps body)) (scimitar-lambda ps (subst (remove*-by-car (flatten ps) yzs) body))]
    [((scimitar-fix fs b)) (let ((yzs (remove*-by-car (map car fs) yzs)))
                        (scimitar-fix (map-cadr (curry subst yzs) fs) (subst yzs b)))]
    [((scimitar-let bs e)) (scimitar-let (map-cadr go bs) (subst (remove*-by-car (map car bs) yzs) e))]
    [((scimitar-let* bs e)) (let ((bs-yzs (foldl
                                            (lambda (b bs-yzs)
                                              (let ((bs (car bs-yzs))
                                                    (yzs (cadr bs-yzs))
                                                    (x (car b))
                                                    (e (cadr b)))
                                                `(,(snoc bs `(,x ,(subst yzs e)))
                                                  ,(remove*-by-car `(,x) yzs))))
                                            `(() ,yzs)
                                            bs)))
                              (scimitar-let* (car bs-yzs) (subst (cadr bs-yzs) e)))]
    [((scimitar-begin es)) (scimitar-begin (map go es))]
    [((scimitar-num _)) e]
    [((scimitar-plus e1 e2)) (scimitar-plus (go e1) (go e2))]
    [((scimitar-minus e1 e2)) (scimitar-minus (go e1) (go e2))]
    [((scimitar-mul e1 e2)) (scimitar-mul (go e1) (go e2))]
    [((scimitar-lt e1 e2)) (scimitar-lt (go e1) (go e2))]
    [((scimitar-eq e1 e2)) (scimitar-eq (go e1) (go e2))]
    [((scimitar-gt e1 e2)) (scimitar-gt (go e1) (go e2))]
    [((scimitar-ref vec ix)) (scimitar-ref (go vec) (go ix))]
    [((scimitar-if eg et ef)) (scimitar-if (go eg) (go et) (go ef))]
    [((scimitar-for i en em eb)) (scimitar-for i (go en) (go em) (subst (remove*-by-car `(,i) yzs) eb))]
    [((scimitar-sum i en em eb)) (scimitar-sum i (go en) (go em) (subst (remove*-by-car `(,i) yzs) eb))]
    [((scimitar-constraint lhs ineq rhs)) (scimitar-constraint (go lhs) ineq (go rhs))]
    [((scimitar-symbolic)) e]
    [((scimitar-infeasible)) e])
  (go e))

(define/contract (scimitar-expr->env e)
  (-> scimitar-expr? env?) ;; NOTE: only works on a typed expr
  (match e
    [(scimitar-solve _ _ _ locals problems)
     (env-remove* locals (apply env-union-key (map scimitar-expr->env problems)))]
    [(scimitar-var _)
     (error "Bug in scimitar-expr->env")]
    [(scimitar-typed (scimitar-var x) ty)
     (env `((,x ,ty)))]
    [(scimitar-typed e _)
     (scimitar-expr->env e)]
    [(or (scimitar-val _)
         (scimitar-num _)
         (scimitar-poly _))
     (env-empty)]
    [(scimitar-tuple es)
     (apply env-union-key (map scimitar-expr->env es))]
    [(scimitar-switch _ e es)
     (apply env-union-key (scimitar-expr->env e) (map scimitar-expr->env es))]
    [(scimitar-app f args)
     (env-union-key (scimitar-expr->env f) (scimitar-expr->env args))]
    [(scimitar-lambda ps body)
     (env-remove* (flatten ps) (scimitar-expr->env body))]
    [(scimitar-fix fs e)
     (env-remove* (map car fs) (apply env-union-key (scimitar-expr->env e) (map (compose scimitar-expr->env cadr) fs)))]
    [(scimitar-let bs e)
     (apply env-union-key (env-remove* (map car bs) (scimitar-expr->env e)) (map (compose scimitar-expr->env cadr) bs))]
    [(scimitar-let* bs e)
     (foldr (lambda (b G) (env-union-key (scimitar-expr->env (cadr b)) (env-remove (car b) G))) (scimitar-expr->env e) bs)]
    [(scimitar-begin es)
     (apply env-union-key (map scimitar-expr->env es))]
    [(or (scimitar-plus e1 e2)
         (scimitar-minus e1 e2)
         (scimitar-mul e1 e2)
         (scimitar-lt e1 e2)
         (scimitar-eq e1 e2)
         (scimitar-gt e1 e2)
         (scimitar-ref e1 e2)
         (scimitar-constraint e1 _ e2))
     (env-union-key (scimitar-expr->env e1) (scimitar-expr->env e2))]
    [(scimitar-if eg et ef)
     (env-union-key (scimitar-expr->env eg) (scimitar-expr->env et) (scimitar-expr->env ef))]
    [(or (scimitar-for i en em eb)
         (scimitar-sum i en em eb))
     (env-union-key (scimitar-expr->env en) (scimitar-expr->env em) (env-remove i (scimitar-expr->env eb)))]
    [(scimitar-symbolic)
     (env-empty)]
    [(scimitar-infeasible)
     (env-empty)]))

(define/contract (scimitar-expr->ir-expr e)
  (-> scimitar-expr? (or/c ir-expr? #f))
  (define/match (do-val v)
    [((and (? vec?) v))  (ir-vec v)]
    [((? null?))         (ir-unit)]
    [((and (? poly?) v)) (ir-poly v)]
    [((and (? list?) v)) (ir-tuple (map do-val v))])
  (define/match (go e)
    [((scimitar-val v)) (do-val v)]
    [((scimitar-typed e _)) (go e)]
    [((scimitar-var x)) (ir-var x)]
    [((scimitar-poly p)) (ir-poly p)]
    [((scimitar-num n)) (ir-num n)]
    [((scimitar-tuple es))
     (let ((es (map go es)))
       (and (andmap identity es) (ir-tuple es)))]
    [((or (scimitar-app e1 e2)
          (scimitar-plus e1 e2)
          (scimitar-minus e1 e2)
          (scimitar-mul e1 e2)
          (scimitar-ref e1 e2)))
     (let ((e1 (go e1))
           (e2 (and e1 (go e2)))
           (op (cond [(scimitar-app? e) ir-poly-app]
                     [(scimitar-plus? e) ir-add]
                     [(scimitar-minus? e) (lambda (e1 e2) (ir-sub e1 e2))]
                     [(scimitar-mul? e) ir-var-mul]
                     [(scimitar-ref? e) ir-vec-ix])))
       (and e1 e2 (op e1 e2)))]
    [(_) #f])
  (go e))

(define/contract (scimitar-expr->ir-stmt e)
  (-> scimitar-expr? ir-stmt?)
  (define/match (go e)
    [((scimitar-typed e _)) (go e)]
    [((scimitar-constraint e1 ineq e2))
     (ir-constraint (scimitar-expr->ir-expr e1) ineq (scimitar-expr->ir-expr e2))])
  (go e))

(define/contract (scimitar-typed-list->scimitar-typed es)
  (-> (listof scimitar-typed?) scimitar-typed?)
  (let ((ty (ty-list->ty (map scimitar-typed-ty es)))
        (e (scimitar-expr-list->scimitar-expr es)))
    (if (= 1 (length es)) e (scimitar-typed e ty))))

(define/contract (scimitar-expr-list->scimitar-expr es)
  (-> (listof scimitar-expr?) scimitar-expr?)
  (match es
    ['() (scimitar-val '())]
    [`(,e) e]
    [es (scimitar-tuple es)]))

(define/contract (scimitar-expr->scimitar-expr-list e)
  (-> scimitar-expr? (listof scimitar-expr?))
  (match e
    [(scimitar-val (? list? vs)) (map scimitar-val vs)]
    [(scimitar-tuple es) es]
    [e `(,e)]))

(define/contract (scimitar-fmap p? f e)
  (-> procedure? procedure? scimitar-expr? scimitar-expr?)
  (define (test e)
    (find (if (p? e) (f e) e)))
  (define/match (find e)
    [((scimitar-solve dir res obj locals ps)) (scimitar-solve dir res obj locals (map test ps))]
    [((scimitar-var _)) e]
    [((scimitar-typed e ty)) (scimitar-typed (test e) ty)]
    [((scimitar-val _)) e]
    [((scimitar-poly _)) e]
    [((scimitar-tuple es)) (scimitar-tuple (map test es))]
    [((scimitar-switch op e es)) (scimitar-switch op (test e) (map test es))]
    [((scimitar-app g args)) (scimitar-app (test g) (test args))]
    [((scimitar-lambda ps body)) (scimitar-lambda ps (test body))]
    [((scimitar-fix fs e)) (scimitar-fix (map-cadr test fs) (test e))]
    [((scimitar-let bs e)) (scimitar-let (map-cadr test bs) (test e))]
    [((scimitar-let* bs e)) (scimitar-let* (map-cadr test bs) (test e))]
    [((scimitar-begin es)) (scimitar-begin (map test es))]
    [((scimitar-num _)) e]
    [((scimitar-plus e1 e2)) (scimitar-plus (test e1) (test e2))]
    [((scimitar-minus e1 e2)) (scimitar-minus (test e1) (test e2))]
    [((scimitar-mul e1 e2)) (scimitar-mul (test e1) (test e2))]
    [((scimitar-lt e1 e2)) (scimitar-lt (test e1) (test e2))]
    [((scimitar-eq e1 e2)) (scimitar-eq (test e1) (test e2))]
    [((scimitar-gt e1 e2)) (scimitar-gt (test e1) (test e2))]
    [((scimitar-ref vec ix)) (scimitar-ref (test vec) (test ix))]
    [((scimitar-if eg et ef)) (scimitar-if (test eg) (test et) (test ef))]
    [((scimitar-for i en em eb)) (scimitar-for i (test en) (test em) (test eb))]
    [((scimitar-sum i en em eb)) (scimitar-sum i (test en) (test em) (test eb))]
    [((scimitar-constraint lhs ineq rhs)) (scimitar-constraint (test lhs) ineq (test rhs))]
    [((scimitar-symbolic)) e]
    [((scimitar-infeasible)) e])
  (test e))

(define/contract (scimitar-lower-objective obj)
  (-> scimitar-expr? (listof (list/c symbol? number?)))
  ;; lowers to the form ((x a) ...)
  (define/match (go e)
    [((scimitar-typed e _)) (go e)]
    [((scimitar-mul (scimitar-typed (scimitar-num n) _)
                    (scimitar-typed (scimitar-var x) _)))
     `((,x ,n))]
    [((scimitar-plus e1 e2))
     (append (go e1) (go e2))]
    [((scimitar-var x)) `((,x 1))]
    [((scimitar-num _)) `()]
    [(_) (error (format "improperly formatted objective ~a" (scimitar-expr->string obj)))])
  (go obj))

(define scimitar-ix?
  (let* ((v-elem? (vec-dims/c (=/c 0)))
         (e-elem? (or/c scimitar-num? (and/c scimitar-val? (property/c scimitar-val-v v-elem?))))
         (t-elem? (lambda (p?) (or/c p? (and/c scimitar-typed? (property/c scimitar-typed-e p?)))))
         (elem? (t-elem? e-elem?)))
    (or/c elem?
          (t-elem? (and/c scimitar-val? (property/c scimitar-val-v null?)))
          (t-elem? (and/c scimitar-val? (property/c scimitar-val-v (*list/c v-elem? v-elem? v-elem?))))
          (t-elem? (and/c scimitar-tuple? (property/c scimitar-tuple-es (*list/c elem? elem? elem?)))))))

(define/contract (scimitar-ix->measurement e)
  (-> scimitar-ix? measurement?)
  (match e
    [(scimitar-typed e _) (scimitar-ix->measurement e)]
    [(scimitar-val '()) '()]
    [(scimitar-num ix) `(,ix)]
    [(scimitar-val (vec-dense ix)) `(,ix)]
    [(scimitar-val `(,i1 ,i2 ,ixs ...))
     (map (match-lambda [(vec-dense ix) ix]) (cons i1 (cons i2 ixs)))]
    [(scimitar-tuple ixs)
     (map (match-lambda [(scimitar-num ix) ix] [(scimitar-val (vec-dense ix)) ix]) ixs)]))
