#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "../cps/basis.rkt")
(require "../elab.rkt")
(require "../env.rkt")
(require (except-in "../poly.rkt" constraint?))
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")

(provide
  scimitar-elab)

(define/contract (scimitar-inf e Gamma)
  (-> scimitar-expr? env? (values (listof constraint?) scimitar-typed?))
  (match e
    [(scimitar-solve dir res obj locals problems)
     (let*-values ([(Gamma) (foldr (lambda (x G) (env-add x (fresh-tyvar) G)) Gamma locals)]
                   [(ocs obj) (scimitar-inf obj Gamma)]
                   [(css problems) (map-values (curryr scimitar-inf Gamma) problems)]
                   [(fvas) (concat (map scimitar-free-in problems))]
                   [(fvos) (scimitar-free-in obj)]
                   [(fvrs) (sym-val-free-in res)]
                   [(bad-locals) (remove* fvas locals)]
                   [(bad-obj) (remove* (append fvas locals) fvos)]
                   [(bad-res) (remove* (append fvas locals) fvrs)])
       (when (not (null? bad-locals))
         (error (format "During scimitar type inference, some locals were unused: ~v" bad-locals)))
       (when (not (null? bad-obj))
         (error (format "During scimitar type inference, some objective vars are not declared: ~v" bad-obj)))
       (when (not (null? bad-res))
         (error (format "During scimitar type inference, some result vars are not declared: ~v" bad-res)))
       (values
         (append ocs (concat css))
         (scimitar-typed
           (scimitar-solve dir res obj locals problems)
           (ty-tree->ty (tree-map (lambda (x) (second (env-assoc x Gamma))) res)))))]
    [(scimitar-typed e ty)
     (let-values ([(cs e) (scimitar-inf e Gamma)])
       (values (cons (<: (scimitar-typed-ty e) ty) cs)
               (scimitar-typed (scimitar-typed-e e) ty)))]
    [(scimitar-var x)
     (match (env-assoc x Gamma)
       [`(,_ ,ty) (values '() (scimitar-typed e ty))]
       [#f (error (format "During scimitar type inference, variable ~a not found" x))])]
    [(scimitar-val v)
     (values '() (scimitar-typed e (val-ty v)))]
    [(scimitar-poly poly)
     (values '() (scimitar-typed e (poly-ty poly)))]
    [(scimitar-tuple es)
     (let-values ([(css es) (map-values (curryr scimitar-inf Gamma) es)])
       (values (concat css)
               (scimitar-typed (scimitar-tuple es)
                               (apply tuplety (map scimitar-typed-ty es)))))]
    [(scimitar-switch op e es)
     (let-values ([(cse e) (scimitar-inf e Gamma)]
                  [(css es) (for/lists (css es) ([e es]) (scimitar-inf e Gamma))]
                  [(alpha) (fresh-tyvar)])
       (match* ((poly-ty (op (unitty))) (map scimitar-typed-ty es))
         [((polyty (tuplety ti _ ...) _ _) ts)
          (values (apply append `(,(~ ti
                                      (scimitar-typed-ty e))
                             ,@(map (curryr <: alpha) ts))
                         cse css)
                  (scimitar-typed (scimitar-switch op e es) alpha))]))]
    [(scimitar-app f args)
     (let-values ([(csf f) (scimitar-inf f Gamma)]
                  [(csa args) (scimitar-inf args Gamma)]
                  [(tyi) (fresh-tyvar)]
                  [(tyo) (fresh-tyvar)])
       (values (append `(,(~ (scimitar-typed-ty f)
                             (polyty tyi '() tyo))
                         ,(~ tyi
                             (scimitar-typed-ty args)))
                       csf csa)
               (scimitar-typed (scimitar-app f args) tyo)))]
    [(scimitar-lambda ps body)
     (when (not (null? (duplicates (flatten ps))))
       (error (format "During scimitar type inference, found duplicate parameters: ~a" (duplicates (flatten ps)))))
     (let*-values ([(tys) (tree-map (lambda (_) (fresh-tyvar)) ps)]
                   [(Gamma) (env-union-key
                              (env (tree-preorder-zip ps tys))
                              Gamma)]
                   [(cs body) (scimitar-inf body Gamma)])
       (values cs
               (scimitar-typed (scimitar-lambda ps body)
                          (polyty (ty-tree->ty tys) '() (scimitar-typed-ty body)))))]
    [(scimitar-fix fs e)
     (when (not (null? (duplicates (map car fs))))
       (error (format "During scimitar type inference, found duplicate functions: ~a" (duplicates (map car fs)))))
     (let*-values ([(Gamma-f) (env (map (lambda (fxsb)
                                          `(,(first fxsb)
                                            ,(fresh-tyvar)))
                                        fs))]
                   [(Gamma) (env-union-key Gamma-f Gamma)]
                   [(css fs) (for/lists (css fs) ([f fs])
                               (match f
                                 [`(,f ,e)
                                  (let*-values ([(tyf) (second (env-assoc f Gamma-f))]
                                                [(cs e) (scimitar-inf e Gamma)])
                                    (values (append `(,(~ tyf (scimitar-typed-ty e))
                                                      ,(~ tyf (polyty (fresh-tyvar) '() (fresh-tyvar))))
                                                    cs)
                                            `(,f ,e)))]))]
                   [(cse e) (scimitar-inf e Gamma)])
       (values (apply append cse css)
               (scimitar-typed (scimitar-fix fs e) (scimitar-typed-ty e))))]
    [(scimitar-let bs e)
     (when (not (null? (duplicates (map car bs))))
       (error (format "During scimitar type inference, found duplicate let bindings: ~a" (duplicates (map car bs)))))
     (let*-values ([(css bs) (for/lists (css bs) ([b bs])
                               (match b
                                 [`(,b ,e)
                                  (let*-values ([(cs e) (scimitar-inf e Gamma)])
                                    (values cs `(,b ,e)))]))]
                   [(Gamma-b) (env (map-cadr scimitar-typed-ty bs))]
                   [(Gamma) (env-union-key Gamma-b Gamma)]
                   [(cse e) (scimitar-inf e Gamma)])
       (values (apply append cse css)
               (scimitar-typed (scimitar-let bs e) (scimitar-typed-ty e))))]
    [(scimitar-let* bs e)
     (let*-values ([(cs-bs-Gamma) (foldl (match-lambda*
                                           [`((,b ,e) (,css ,bs ,Gamma))
                                            (let-values ([(cs e) (scimitar-inf e Gamma)])
                                                 `(,(append css cs)
                                                   ,(snoc bs `(,b ,e))
                                                   ,(env-set b (scimitar-typed-ty e) Gamma)))])
                                         `(() () ,Gamma)
                                         bs)]
                   [(cs) (car cs-bs-Gamma)]
                   [(bs) (cadr cs-bs-Gamma)]
                   [(Gamma) (caddr cs-bs-Gamma)]
                   [(cs-e e) (scimitar-inf e Gamma)])
       (values (append cs-e cs)
               (scimitar-typed (scimitar-let* bs e) (scimitar-typed-ty e))))]
    [(scimitar-begin es)
     (let-values ([(css es) (for/lists (css es) ([e es]) (scimitar-inf e Gamma))])
       (values (concat css)
               (scimitar-typed (scimitar-begin es)
                               (last (cons (unitty) (map scimitar-typed-ty es))))))]
    [(scimitar-num n)
     (let ((alpha (fresh-tyvar))
           (ty (vecty (interval n n) '())))
       (values `(,(<: ty alpha)) (scimitar-typed e alpha)))]
    [(or (scimitar-plus e1 e2)
         (scimitar-minus e1 e2)
         (scimitar-mul e1 e2))
     (let*-values ([(op ty-arith-op)
                    (cond [(scimitar-plus? e) (values scimitar-plus ty-plus)]
                          [(scimitar-minus? e) (values scimitar-minus ty-minus)]
                          [(scimitar-mul? e) (values scimitar-mul ty-mul)])]
                   [(cs1 e1) (scimitar-inf e1 Gamma)]
                   [(cs2 e2) (scimitar-inf e2 Gamma)]
                   [(ty1) (scimitar-typed-ty e1)]
                   [(ty2) (scimitar-typed-ty e2)]
                   [(ty) (ty-arith-op ty1 ty2)])
       (values (append cs1 cs2)
               (scimitar-typed (op e1 e2) ty)))]
    [(or (scimitar-lt e1 e2)
         (scimitar-eq e1 e2)
         (scimitar-gt e1 e2))
     (let*-values ([(op) (cond [(scimitar-lt? e) scimitar-lt]
                               [(scimitar-eq? e) scimitar-eq]
                               [(scimitar-gt? e) scimitar-gt])]
                   [(cs1 e1) (scimitar-inf e1 Gamma)]
                   [(cs2 e2) (scimitar-inf e2 Gamma)]
                   [(ty) (fresh-tyvar)]
                   [(cs) `(,(<: (scimitar-typed-ty e1) ty)
                           ,(<: (scimitar-typed-ty e2) ty))])
       (values (append cs cs1 cs2)
               (scimitar-typed (op e1 e2) (bitty))))]
    [(scimitar-ref vec ix)
     (let-values ([(cs-v vec) (scimitar-inf vec Gamma)]
                  [(cs-x ix)  (scimitar-inf ix  Gamma)])
       (when (not (vecty? (scimitar-typed-ty vec)))
         (error "Unfortunately, vectors for indexing cannot yet be inferred."))
       (let ((ty-v (scimitar-typed-ty vec))
             (ty-ix (scimitar-typed-ty ix))
             (make-iv (lambda (n) (interval 0 n))))
         (values (append
                   `(,(match ty-v
                        [(vecty _ '())
                         (~ ty-ix (unitty))]
                        [(vecty _ `(,n))
                         (<: ty-ix (vecty (make-iv n) '()))]
                        [(vecty _ `(,ns ...))
                         (<: ty-ix (apply tuplety (map (compose (curryr vecty '()) make-iv) ns)))]))
                   cs-v cs-x)
                 (scimitar-typed (scimitar-ref vec ix) (vecty (vecty-interval ty-v) '())))))]
    [(scimitar-if eg et ef)
     (let*-values ([(csg eg) (scimitar-inf eg Gamma)]
                   [(cst et) (scimitar-inf et Gamma)]
                   [(csf ef) (scimitar-inf ef Gamma)]
                   [(tyg) (scimitar-typed-ty eg)]
                   [(tyt) (scimitar-typed-ty et)]
                   [(tyf) (scimitar-typed-ty ef)]
                   [(alpha) (fresh-tyvar)]
                   [(cs) `(,(<: tyg (bitty)) ,(<: tyt alpha) ,(<: tyf alpha))])
       (values (append cs csg cst csf)
               (scimitar-typed (scimitar-if eg et ef) alpha)))]
    [(or (scimitar-for i en em eb)
         (scimitar-sum i en em eb))
     (let*-values ([(op) (cond [(scimitar-for? e) scimitar-for]
                               [(scimitar-sum? e) scimitar-sum])]
                   [(csn en) (scimitar-inf en Gamma)]
                   [(csm em) (scimitar-inf em Gamma)]
                   [(alpha) (fresh-tyvar)]
                   [(beta) (fresh-tyvar)]
                   [(Gamma) (env-set i alpha Gamma)]
                   [(csb eb) (scimitar-inf eb Gamma)]
                   [(tyn) (scimitar-typed-ty en)]
                   [(tym) (scimitar-typed-ty em)]
                   [(tyb) (scimitar-typed-ty eb)]
                   [(tyo) (if (scimitar-sum? e) (ty-mul alpha tyb) tyb)]
                   [(cs) `(,(<: tyn alpha) ,(<: tym alpha) ,(<: alpha (intty)))])
       (values (append cs csn csm csb)
               (scimitar-typed (op i en em eb) tyo)))]
    [(scimitar-constraint lhs ineq rhs)
     (let*-values ([(cs-l lhs) (scimitar-inf lhs Gamma)]
                   [(cs-r rhs) (scimitar-inf rhs  Gamma)]
                   [(ty) (fresh-tyvar)]
                   [(cs) `(,(<: (scimitar-typed-ty lhs) ty)
                           ,(<: (scimitar-typed-ty rhs) ty))])
       (values (append cs cs-l cs-r)
               (scimitar-typed (scimitar-constraint lhs ineq rhs) (unitty))))]
    [(or (scimitar-symbolic)
         (scimitar-infeasible))
     (values '() (scimitar-typed e (fresh-tyvar)))]))

(define/contract (scimitar-subst e theta)
  (-> scimitar-typed? env? scimitar-typed?)
  (define (go e)
    (match e
      [(scimitar-solve dir res obj locals problem)
       (scimitar-solve dir res (retype obj) locals (map retype problem))]
      [(? scimitar-var?) e]
      [(? scimitar-poly?) e]
      [(? scimitar-val?) e]
      [(scimitar-tuple es) (scimitar-tuple (map retype es))]
      [(scimitar-switch op e es) (scimitar-switch op (retype e) (map retype es))]
      [(scimitar-app f args) (scimitar-app (retype f) (retype args))]
      [(scimitar-lambda ps body) (scimitar-lambda ps (retype body))]
      [(scimitar-fix fs e) (scimitar-fix (map-cadr retype fs) (retype e))]
      [(scimitar-let bs e) (scimitar-let (map-cadr retype bs) (retype e))]
      [(scimitar-let* bs e) (scimitar-let* (map-cadr retype bs) (retype e))]
      [(scimitar-begin es) (scimitar-begin (map retype es))]
      [(scimitar-num _) e]
      [(scimitar-plus e1 e2) (scimitar-plus (retype e1) (retype e2))]
      [(scimitar-minus e1 e2) (scimitar-minus (retype e1) (retype e2))]
      [(scimitar-mul e1 e2) (scimitar-mul (retype e1) (retype e2))]
      [(scimitar-lt e1 e2) (scimitar-lt (retype e1) (retype e2))]
      [(scimitar-eq e1 e2) (scimitar-eq (retype e1) (retype e2))]
      [(scimitar-gt e1 e2) (scimitar-gt (retype e1) (retype e2))]
      [(scimitar-ref vec ix) (scimitar-ref (retype vec) (retype ix))]
      [(scimitar-if eg et ef) (scimitar-if (retype eg) (retype et) (retype ef))]
      [(scimitar-for i en em eb) (scimitar-for i (retype en) (retype em) (retype eb))]
      [(scimitar-sum i en em eb) (scimitar-sum i (retype en) (retype em) (retype eb))]
      [(scimitar-constraint lhs ineq rhs) (scimitar-constraint (retype lhs) ineq (retype rhs))]
      [(scimitar-symbolic) e]
      [(scimitar-infeasible) e]
      [_ (error "bug in scimitar type inference")]))
  (define (retype e)
    (match e
      [(scimitar-typed e ty) (scimitar-typed (go e) (ty-subst ty theta))]
      [_ (error "bug in scimitar type inference")]))
  (retype e))

(define/contract (scimitar-elab e)
  (-> scimitar-expr? scimitar-typed?)
  (let*-values ([(Gamma) cps-basis-types]
                [(cs e) (scimitar-inf e Gamma)])
    (scimitar-subst e (solve 'scimitar cs))))
