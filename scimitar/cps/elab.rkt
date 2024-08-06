#lang racket

(require "../contract-utils.rkt")
(require (except-in "../elab.rkt" constraint?))
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "basis.rkt")
(require "grammar.rkt")

(provide
  cps-elab)

(define/contract (cps-inf Gamma e)
  (-> env? cps-cexp? (values (listof ~?) ty? env?))
  (match e
    [(cps-app f args)
     (match* ((env-assoc f Gamma)
              (sym-val-ty Gamma args))
       [(`(,_ ,ty) ts)
        (let ((ti (fresh-tyvar))
              (to (fresh-tyvar)))
          (values
            `(,(~ ty (polyty ti '() to))
              ,(~ ti ts))
            to
            (env-empty)))]
       [(#f _) (error (format "During CPS type inference, function ~v not found" f))])]
    [(cps-fix fs cont)
     (when (not (null? (duplicates (map car fs))))
       (error (format "During CPS type inference, found duplicate functions: ~a" (duplicates (map car fs)))))
     (when (ormap (lambda (fxsb) (not (null? (duplicates (flatten (second fxsb)))))) fs)
       (error (format "During CPS type inference, found duplicate parameters: ~a" (filter (compose not null?) (map (lambda (fxsb) (duplicates (flatten (second fxsb)))) fs)))))
     (let* ((Gammaf (env (map (lambda (fxsc)
                                `(,(first fxsc)
                                  ,(polyty (ty-tree->ty
                                             (tree-map (lambda (_) (fresh-tyvar))
                                                       (second fxsc)))
                                           '()
                                           (fresh-tyvar))))
                              fs)))
            (cs0 (env-range (env-inter-key Gamma Gammaf #:combine ~ #:range? ~?)))
            (Gamma (env-union-key Gamma Gammaf)))
       (let-values ([(cs t Gammag) (cps-inf Gamma cont)])
         (match-let ([`(,css ,Gammahs)
                      (unzip #:default `(() (,(env-empty)))
                        (map (match-lambda
                               [`(,f ,xs ,c)
                                (match (second (env-assoc f Gamma))
                                  [(polyty td _ tc)
                                   (let* ((Gamma (env-union-key (env (map-cadr ty-tree->ty (tree-preorder-zip xs (ty->ty-tree td)))) Gamma)))
                                     (let-values ([(cs t Gamma) (cps-inf Gamma c)])
                                       `(,(cons (~ t tc) cs) ,Gamma)))])])
                             fs))])
           (values (apply append cs0 cs css)
                   t
                   (apply env-union-key Gammaf Gammag Gammahs)))))]
    [(cps-primop (cps-op _ _ (poly ti to _ _)) args out cont)
     (match (sym-val-ty Gamma args)
       [tas
        (let-values ([(cs tc Gamma)
                      (cps-inf
                        (if (null? out)
                          Gamma
                          (apply foldr env-add
                                 Gamma (unzip (map-cadr ty-tree->ty
                                                (tree-preorder-zip out (ty->ty-tree to))))))
                        cont)])
          (values (cons (~ ti tas)
                        cs)
                  tc
                  Gamma))])]
    [(cps-switch op v conts)
     (let-values ([(css ts Gammas) (map-values (curry cps-inf Gamma) conts)])
       (match* ((poly-ty (op (unitty)))
                (sym-val-ty Gamma v)
                ts)
         [((polyty (tuplety ti _ ...) _ _) tv `(,t1 ,ts ...))
          (values (apply append `(,(~ ti tv)
                             ,@(map (curry ~ t1) ts))
                           css)
                  t1
                  (apply env-union-key Gammas))]))]))

(define/contract (cps-elab e)
  (-> cps-cexp? (list/c ty? env?))
  (let*-values ([(Gamma) cps-basis-types]
                [(cs ty Gamma) (cps-inf Gamma e)]
                [(theta) (solve 'CPS cs)])
    `(,(ty-subst ty theta) ,(env-subst Gamma theta))))
