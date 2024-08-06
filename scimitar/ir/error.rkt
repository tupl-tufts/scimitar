#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  ir-expr->string
  ir-dec->string
  ir-unexpected)

(define (ir-var-dec->string x ty)
  (format "(: ~a ~v)" (symbol->string x) ty))

(define/contract (ir-env->string Gamma)
  (-> env? string?)
  (string-join (map (curry apply ir-var-dec->string) (env-entries Gamma)) " "))

(define (ir-locals->maybe-string Gamma)
  (let ((ls (ir-env->string Gamma)))
    (if (non-empty-string? ls)
      `(,(format "(locals ~a)" ls))
      '())))

(define/contract (ir-tuple->string t)
  (-> ir-tuple? string?)
  (define (tick-for es)
    (if (andmap ir-num? es) "'" "`"))
  (define (go e)
    (format "~a~a" (if (ir-num? e) "" ",") (ir-expr->string e)))
  (match t
    [(ir-tuple es) (format "~a(~a)" (tick-for es) (string-join (map go es) " "))]))

(define/contract (ir-expr->string e)
  (-> ir-expr? string?)
  (match e
    [(ir-num n)         (number->string n)]
    [(ir-vec v)         (format "(eval ~v)" v)]
    [(ir-unit)          "'()"]
    [(ir-poly p)        (format "(eval ~v)" p)]
    [(ir-var x)         (symbol->string x)]
    [(ir-cast e ty)     (format "(: ~a ~v)" (ir-expr->string e) ty)]
    [(ir-add e1 e2)     (format "(+ ~a ~a)" (ir-expr->string e1) (ir-expr->string e2))]
    [(ir-scmul n e)     (format "(.* ~a ~a)" (ir-expr->string n) (ir-expr->string e))]
    [(ir-var-mul e1 e2) (format "(* ~a ~a)" (ir-expr->string e1) (ir-expr->string e2))]
    [(ir-tuple es)      (ir-tuple->string e)]
    [(ir-vec-ix v ix)   (format "(! ~a ~a)" (ir-expr->string v) (ir-expr->string ix))]
    [(ir-vec-sum id start end body) (format "(sum ~a ~a ~a ~a)" (symbol->string id) (ir-expr->string start) (ir-expr->string end) (ir-expr->string body))]
    [(ir-poly-app f x)  (format "(~a ~a)" (ir-expr->string f) (string-join (map ir-expr->string (ir-val->ir-val-list x)) " "))]
    [(ir-thunk body _ _)  (format "(eval ~a)" body)]))

(define/contract (ir-stmt->string s)
  (-> ir-stmt? string?)
  (match s
    [(ir-forall id start end body)(format "(forall ~a ~a ~a ~a)" (symbol->string id) (ir-expr->string start) (ir-expr->string end) (ir-stmt->string body))]
    [(ir-subject-to Gamma_l ss)   (format "(subject-to ~a)" (append (ir-locals->maybe-string Gamma_l) (string-join (map ir-stmt->string ss) " ")))]
    [(ir-constraint lhs ineq rhs) (format "(~a ~a ~a)"    (symbol->string ineq) (ir-expr->string lhs) (ir-expr->string rhs))]))

(define/contract (ir-dec->string d)
  (-> ir-dec? string?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (format "(define (~a (~a)~a) ~a)"
             (symbol->string f)
             (ir-env->string Gamma_f)
             (if (env-empty? Gamma_r)
               ""
               (string-append " " (apply ir-var-dec->string (car (env-entries Gamma_r)))))
             (string-join
                 (append (ir-locals->maybe-string Gamma_l)
                         (map ir-stmt->string ss)) " "))]))

(define (ir-unexpected phase v)
  (cond
    [(ir-dec? v)  (error (format "During ~a: Unexpected declaration ~a" phase (ir-dec->string  v)))]
    [(ir-stmt? v) (error (format "During ~a: Unexpected statement ~a"   phase (ir-stmt->string v)))]
    [(ir-val? v)  (error (format "During ~a: Unexpected value ~a"       phase (ir-expr->string v)))]
    [(ir-expr? v) (error (format "During ~a: Unexpected expression ~a"  phase (ir-expr->string v)))]
    [else         (error (format "During ~a: Unexpected non-IR ~v "     phase v))]))
