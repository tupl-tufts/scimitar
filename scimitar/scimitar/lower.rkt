#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "../cps/grammar.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/util.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")

(provide
  scimitar-lower)

(define/match (tree-types xs e)
  [(`(,y . ,ys) (scimitar-typed (scimitar-tuple es) _)) (concat (map tree-types xs es))]
  [('() (scimitar-typed _ (unitty))) '()]
  [((? sym-val?) _) `((,xs ,(scimitar-typed-ty e)))]
  [(_ _) (println e) (error (format "Invalid key ~v in tree-types" xs))])

(define/contract (scimitar-lower e cont)
  (-> scimitar-expr?
      (-> sym-val? cps-cexp?)
      cps-cexp?)
  (match e
    [(scimitar-typed (scimitar-solve dir res obj locals `(,(scimitar-typed (scimitar-app (scimitar-typed (scimitar-poly poly) _) args) ty))) tyr)
     (let* ((out (tree-map gensym res))
            (obj (scimitar-lower-objective obj)))
       (scimitar-lower args
          (lambda (in)
            (let* ((result  (gensym 'result))
                   (locals  (cons result locals))
                   (Gamma-a (env-add result ty (env (filter (compose symbol? car) (tree-types in args)))))
                   (Gamma-l (env-filter-keys locals Gamma-a))
                   (Gamma-i (env-diff-key Gamma-a Gamma-l))
                   (r (gensym 'r))
                   (poly (ir-compile
                           (ir-poly-dec
                             (gensym 'primop)
                             Gamma-i
                             (env `((,r ,tyr)))
                             Gamma-l
                             (list
                               (ir-constraint
                                 (ir-var r)
                                 '=
                                 (sym-val->ir-val res))
                               (ir-constraint
                                 (ir-var result)
                                 '=
                                 (ir-poly-app (ir-poly poly) (sym-val->ir-val in)))))
                           (env-empty)
                           (env-empty number?)
                           (env-empty poly?)))
                   (in (sym-val-list->sym-val (env-dom Gamma-i))))
              (cps-primop
                (cps-op dir obj poly)
                in out (cont out))))))]
    [(scimitar-var x) (cont x)]
    [(scimitar-typed e ty) (scimitar-lower e cont)]
    [(scimitar-val v) (cont v)]
    [(scimitar-poly poly) (cont poly)]
    [(scimitar-tuple es)
     ((foldr (lambda (e cont)
               (lambda (ins)
                 (scimitar-lower e
                   (lambda (in)
                     (cont (sym-val-append ins in))))))
        cont
        es) '())]
    [(scimitar-switch op e es)
     (let* ((r (gensym 'r))
            (in-r (gensym 'x)))
       (cps-fix
         `((,r ,in-r ,(cont in-r)))
         (scimitar-lower e
           (lambda (out)
             (cps-switch op out
               (map (lambda (e) (scimitar-lower e (curry cps-app r))) es))))))]
    [(scimitar-app f args)
     (let* ((r (gensym 'r))
            (in-r (gensym 'x)))
       (cps-fix
         `((,r ,in-r ,(cont in-r)))
         (scimitar-lower f
           (lambda (f)
             (scimitar-lower args
               (lambda (args)
                 (cps-app f (sym-val-list->sym-val (list args r)))))))))] ; should be (cps-app f (sym-val-append args r))
    [(scimitar-lambda ps body)
     (let ((l (gensym 'λ))
           (k (gensym 'k)))
       (cps-fix
         `((,l
            ,(sym-val-list->sym-val (list ps k)) ; should be (sym-val-append ps k)
            ,(scimitar-lower body (curry cps-app k))))
         (cont l)))]
    [(scimitar-fix fs e)
     (cps-fix
       (map (match-lambda
              [`(,f ,(scimitar-typed e ty))
               (let ((ps (map (lambda (_) (gensym 'p)) (ty->ty-list (polyty-dom ty))))
                     (p (gensym 'p)))
                 `(,f
                   ,(sym-val-list->sym-val (list (sym-val-list->sym-val ps) p)) ; should be (sym-val-append ps p)
                   ,(scimitar-lower e (curryr cps-app (sym-val-list->sym-val (list (sym-val-list->sym-val ps) p))))))]) ; should be (scimitar-lower e (curryr cps-app (sym-val-append ps p)))
            fs)
       (scimitar-lower e cont))]))
