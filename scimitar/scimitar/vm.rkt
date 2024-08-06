#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require (except-in "util.rkt" subst))
(require "../cps/util.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/util.rkt")
(require "../prim/fun.ir.ast.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "../vm.rkt")

(provide
  (prefix-out scimitar- vm))

(struct/contract
  closure
  ([rho (listof (list/c symbol? number?))]
   [xs sym-val?]
   [body scimitar-expr?])
  #:transparent)

(define (closure-check a)
  (when (closure? a)
    (error "Closures not allowed as VM arguments"))
  (when (list? a)
    (for-each closure-check a)))

;; NOTE: this vm is only for debugging purposes.
(define/contract (vm e)
  (-> scimitar-expr? val?)
  (define sigma (box '()))
  (define (update-sigma k v)
    (set-box! sigma
      (match (assoc k (unbox sigma))
        [`(,_ ,old) (map (lambda (xv) (if (equal? k (car xv)) `(,k ,v) xv)) (unbox sigma))]
        [#f (cons `(,k ,v) (unbox sigma))])))
  (define top (box 0))
  (define null (string->unreadable-symbol "null"))
  (define (malloc)
    (set-box! top (+ 1 (unbox top)))
    (update-sigma (unbox top) null)
    (unbox top))
  (define (make-rho xs [args #f])
    (let* ((addrs (tree-map (lambda (_) (malloc)) xs))
           (rho (tree-preorder-zip xs addrs)))
      (when args
        (tree-map update-sigma addrs args))
      rho))
  (define (lookup x rho)
    (match (assoc x rho)
       [`(,_ ,addr)
         (match (assoc addr (unbox sigma))
           [#f (error (format "Address ~v of variable ~v not found" addr x))]
           [`(,_ ,v) #:when (equal? v null)
            (error (format "Address ~v of variable ~v is null" addr x))]
           [`(,_ ,v) v])]
       [#f (error (format "Variable ~v not found" x))]))
  (define/match (go e rho)
    [((scimitar-solve _ _ _ _ _) _)
     (error "Unexpected solve expression; running before type checking?")]
    [((scimitar-typed (scimitar-solve dir res obj locals ps) ty) rho)
     (scimitar-fmap (or/c scimitar-let? scimitar-let*? scimitar-begin?
                          scimitar-num? scimitar-plus? scimitar-minus?
                          scimitar-mul? scimitar-ref?
                          scimitar-lt?  scimitar-eq?   scimitar-gt?
                          scimitar-if?  scimitar-for?  scimitar-sum?
                          scimitar-constraint?  scimitar-infeasible?
                          scimitar-symbolic?)
       (lambda (e)
         (error "Unexpected sugar term; running before desugar?"))
       e)
     (scimitar-fmap scimitar-fix?
       (lambda (e)
         (error "Unexpected fix; running before inline-solve-fix?"))
       e)
     (scimitar-fmap scimitar-infeasible?
       (lambda (e)
         (error "Unexpected infeasible token; running before inline-solve-infeasible?"))
       e)
     (scimitar-fmap scimitar-lambda?
       (lambda (e)
         (error "Unexpected lambda; running before inline-solve-lambda?"))
       e)
     (scimitar-fmap scimitar-switch?
       (lambda (e)
         (error "Unexpected switch; running before inline-switch?"))
       e)
     (let* ((ty-l (ty-list->ty (map scimitar-typed-ty ps)))
            (result (gensym 'result))
            (Gamma-ps (scimitar-expr->env e))
            (Gamma-ls (env-remove* (env-dom Gamma-ps) (env-add result ty-l (apply env-union-key (map scimitar-expr->env ps)))))
            (r (gensym 'r))
            (Gamma-r (env `((,r ,ty))))
            (poly (ir-compile
                    (ir-poly-dec
                      (gensym 'solve-)
                      Gamma-ps
                      Gamma-r
                      Gamma-ls
                      (list
                        (ir-constraint
                          (ir-var r)
                          `=
                          (sym-val->ir-val res))
                        (ir-constraint
                          (ir-var result)
                          `=
                          (ir-val-list->ir-val (map scimitar-expr->ir-expr ps)))))
                    (env-empty)
                    (env-empty number?)
                    (env-empty poly?)))
            (argvals (sym-val-list->sym-val (map (curryr lookup rho) (env-dom Gamma-ps))))
            (_ (closure-check argvals))
            (obj (calculate-obj poly (scimitar-lower-objective obj)))
            (poly (fun-app poly argvals))
            (obj (fun-app-obj-hack (poly-ty poly) obj))
            (op (lp dir obj poly))
            (result (or (solve op) (error "While running, infeasible problem"))))
       result)]
    [((scimitar-typed e ty) rho)
     (let ((v (go e rho)))
       (if (closure? v)
         v
         (val-cast ty v)))]
    [((scimitar-var x) rho)
     (lookup x rho)]
    [((scimitar-val v) rho) v]
    [((scimitar-poly p) rho) p]
    [((scimitar-tuple es) rho) (map (curryr go rho) es)]
    [((scimitar-switch op e es) rho)
     (let* ((v (go e rho))
            (_ (closure-check v))
            (opts (build-list (length es) (lambda (i) (vec-dense i))))
            (op (op (natty)))
            (op (lp-default (fun-app op (cons v opts))))
            (result (or (solve op) (error "While running, infeasible problem")))
            (branch (vec-unpack result)))
       (when (>= branch (length es))
         (error (format "Branch number ~v is beyond the number of branches ~v" branch (length es))))
       (go (list-ref es branch) rho))]
    [((scimitar-app f args) rho)
     (let ((f (go f rho))
           (args (go args rho)))
       (match f
         [(closure rho xs body)
          (go body (append (make-rho xs args) rho))]
         [_ (error "Non-function in application")]))]
    [((scimitar-lambda xs body) rho)
     (closure rho xs body)]
    [((scimitar-fix fs e) rho)
     (let* ((rho-f (make-rho (map car fs)))
            (rho (append rho-f rho))
            (fs (map-cadr (curryr go rho) fs))
            (fvs (map (lambda (fa fl) `(,(cadr fa) ,(cadr fl))) rho-f fs)))
       (map (curry apply update-sigma) fvs)
       (go e rho))]
    [((scimitar-let bs e) rho)
     (let ((xs (map car bs))
           (vs (map (lambda (b) (go (cadr b) rho)) bs)))
       (go e (append (make-rho xs vs) rho)))]
    [((scimitar-let* bs e) rho)
     (let ((rho (foldl
                  (lambda (b rho)
                    (let ((x (car b))
                          (v (go (cadr b) rho)))
                      (append (make-rho `(,x) `(,v)) rho)))
                  rho
                  bs)))
       (go e rho))]
    [((scimitar-begin es) rho)
     (last (cons '() (map (curryr go rho) es)))]
    [((scimitar-infeasible) _)
     (error "While running, infeasible problem")])
  (let ((result (go e '())))
    (closure-check result)
    result))
