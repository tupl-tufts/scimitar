#lang racket

(require (for-syntax syntax/parse))
(require "../env.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "../poly.rkt")
(require "compiler.rkt")
(require "grammar.rkt")
(require "lower.rkt")
(require "util.rkt")

(provide
  (for-syntax ir-dec-name)
  ir-define-syntax
  ir-define
  define-ir)

(define-syntax (go-literal stx)
  (syntax-parse stx
    [(_ (e1 e2 es ...)) #'(ir-tuple (list (go-literal e1)
                                          (go-literal e2)
                                          (go-literal es) ...))]
    [(_ ()) #'(ir-unit)]
    [(_ v)
     #'(match (syntax->datum #'v)
         [(? number? n) (ir-num n)]
         [_ (error (format "During compilation: Unexpected value or expression ~v" (syntax->datum #'v)))])]))

(define-syntax (go-quasiquote stx)
  (syntax-parse stx
    [(_ vars ((~datum unquote) e)) #'(go-expr vars e)]
    [(_ vars (e1 e2 es ...)) #'(ir-tuple (list (go-quasiquote vars e1)
                                               (go-quasiquote vars e2)
                                               (go-quasiquote vars es) ...))]
    [(_    _ v) #'(go-literal v)]))

(define base-ns (make-base-namespace))

(define-syntax-rule (with-base-ns e)
  (parameterize ([current-namespace base-ns]) e))

(define-syntax (ir-expr-translate stx)
  (syntax-parse stx
    [(_ expr) #'(go-expr () expr)]))

(define-syntax (go-expr stx)
  (syntax-parse stx #:datum-literals (: + - .* * ! sum eval)
    [(_ vars (+ e1 es ...))
     #'(let-values ([(eis el)
                     (split-at-right
                       (list (go-expr vars e1)
                             (go-expr vars es) ...)
                       1)])
         (foldr ir-add (car el) eis))]
    [(_ vars (- e)) #'(ir-scmul (ir-num -1) (go-expr vars e))]
    [(_ vars (- e1 es ...)) #'(go-expr vars (+ e1 (- es) ...))]
    [(_ vars (.* e1 e2)) #'(ir-scmul (go-expr vars e1) (go-expr vars e2))]
    [(_ vars (* e1 e2)) #'(ir-var-mul (go-expr vars e1) (go-expr vars e2))]
    [(_ vars (! vec ix)) #'(ir-vec-ix (go-expr vars vec) (go-expr vars ix))]
    [(_ (var ...) (sum id start end body))
     #'(ir-vec-sum (syntax->datum #'id)
         (go-expr (var ...) start)
         (go-expr (var ...) end)
         (go-expr (var ... id) body))]
    [(_ vars (: e ty)) #'(ir-cast (go-expr vars e) (with-base-ns ty))]
    [(_    _ ((~datum quote) e)) #'(go-literal e)]
    [(_ vars ((~datum quasiquote) e)) #'(go-quasiquote vars e)]
    [(_ (var ...) (eval e))
     #'(ir-thunk 'e '(var ...)
         (lambda (var ...)
           (letrec ((go (lambda (v)
                          (match v
                            [(? number?)  (ir-num v)]
                            [(? sym-val?) (sym-val->ir-val v)]
                            [_ (error (format "During evaluation of racket code: Expression ~a yields unknown value ~v" 'e v))]))))
             (go (with-base-ns e)))))]
    [(_ vars (f xs ...)) #'(ir-poly-app (go-expr vars f) (ir-val-list->ir-val (list (go-expr vars xs) ...)))]
    [(_    _ v)
     #'(match (syntax->datum #'v)
         [(? symbol? x) (ir-var x)]
         [(? number? n) (ir-num n)]
         [v (error (format "During compilation: Unknown value or expression ~v" v))])]))

(define-syntax (ir-locals-translate stx)
  (syntax-parse stx #:datum-literals (: locals)
    [(_ (locals (: x ty) ...) ss ...)
     #'(env (list (list (syntax->datum #'x) (with-base-ns ty)) ...))]
    [(_ ss ...)
     #'(env-empty)]))

(define-syntax (ir-block-translate stx)
  (syntax-parse stx #:datum-literals (: locals)
    [(_ vars (locals (: x ty) ...) ss ...)
     #'(list (go-stmt vars ss) ...)]
    [(_ vars ss ...)
     #'(list (go-stmt vars ss) ...)]))

(define-syntax (ir-stmt-translate stx)
  (syntax-parse stx
    [(_ stmt) #'(go-stmt () expr)]))

(define-syntax (go-stmt stx)
  (syntax-parse stx #:datum-literals (= <= >= forall subject-to)
    [(_ (var ...) (forall id start end body))
     #'(ir-forall (syntax->datum #'id)
         (go-expr (var ...) start)
         (go-expr (var ...) end)
         (go-stmt (var ... id) body))]
    [(_ vars (subject-to ss ...))
     #'(ir-subject-to
         (ir-locals-translate ss ...)
         (ir-block-translate vars ss ...))]
    [(_ vars ((~and ineq (~or* = <= >=)) e1 e2))
     #'(ir-constraint
         (go-expr vars e1)
         (syntax->datum #'ineq)
         (go-expr vars e2))]
    [(_    _ s) #'(error (format "During compilation: Unknown syntax ~a" (syntax->datum #'s)))]))

(define-syntax (ir-dec-translate stx)
  (syntax-parse stx #:datum-literals (: define)
    [(_ (define (f ((: x tyi) ...) (: y tyo)) rest ...))
     #'(ir-poly-dec
         (syntax->datum #'f)
         (env (list (list (syntax->datum #'x) (with-base-ns tyi)) ...))
         (env (list (list (syntax->datum #'y) (with-base-ns tyo))))
         (ir-locals-translate rest ...)
         (ir-block-translate () rest ...))]
    [(_ d) #'(error (format "During compilation: Unknown syntax ~a" (syntax->datum #'d)))]))

(begin-for-syntax
  (define (ir-dec-name stx)
    (syntax-parse stx #:datum-literals (: define)
      [(define (f (_ ...) _) _ ...) #'f]
      [d (error (format "Malformed IR declaration:~n ~a" (syntax->datum #'d)))])))

(define-syntax-rule (ir-dec-compile phi d)
  (ir-compile
    (ir-dec-translate d)
    (poly-env->env phi)
    (env-empty number?)
    phi))

;; for the purposes of forall
(define (eval v) v)

(define-syntax (ir-define-syntax stx)
  (syntax-parse stx #:datum-literals (forall)
    [(_ phi (forall (ks:id ... [os:id ds] ...) d))
     (with-syntax ([f (ir-dec-name #'d)]
                   [proxy (gensym)])
       #`(begin
           (define-syntax (proxy stx)
             (syntax-parse stx
               [(_ ks ... os ...)
                #'(ir-dec-compile phi d)]))
           (define-syntax (f stx)
             (syntax-parse stx
               [(_ ks ... (~optional os #:defaults ([os #'ds])) ...)
                #'(proxy (eval ks) ... (eval os) ...)]))))]))

(define-syntax (ir-define stx)
  (syntax-parse stx
    [(_ phi d)
     (with-syntax ([f (ir-dec-name #'d)])
       #`(define f
           (ir-dec-compile phi d)))]))

(define-syntax-rule (define-ir f rest ...)
  (ir-define (env-empty poly?) (define f rest ...)))
