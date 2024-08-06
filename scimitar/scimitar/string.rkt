#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")

(provide scimitar-expr->string)

(define (expr-list->string es)
  (apply string-append (intersperse " " es)))

(define/contract (poly->string p)
  (-> poly? string?)
  ; impl
  (format "~v" p))

(define/contract (ty->string ty)
  (-> ty? string?)
  ; impl
  (format "~v" ty))

(define/contract (sym-val->string v [include-tick #f])
  (->* (sym-val?) (boolean?) string?)
  (define/match (go v)
    [((vec-dense es))
     (define/match (do-vec e)
       [((? list?)) (string-append "#(" (expr-list->string (map do-vec e)) ")")]
       [((? number?)) (number->string e)])
     (do-vec es)]
    [((? null?)) "()"]
    [((? poly?)) (poly->string v)]
    [((? symbol?)) (symbol->string v)]
    [((? list?)) (string-append "(" (expr-list->string (map go v)) ")")])
  (if (and include-tick (list? v))
    (string-append "'" (go v))
    (go v)))

(define/contract (sym-val-list->string v [include-tick #f])
  (->* ((listof sym-val?)) (boolean?) string?)
  (define (go v)
    (string-append "(" (expr-list->string (map sym-val->string v)) ")"))
  (if (and include-tick (list? v))
    (string-append "'" (go v))
    (go v)))

(define/contract (scimitar-expr->string e #:strip-typed [strip-typed #f])
  (->i ([e scimitar-expr?])
       (#:strip-typed [strip-typed boolean?])
       [result string?])
  (define/match (go e)
    [((scimitar-solve dir res obj locals problems))
     (let ((problems (map go problems)))
       (string-append "(optimum-ref " (sym-val->string res)
                      " (" (symbol->string dir)
                      " " (go obj)
                      " " (sym-val-list->string locals)
                      " " (expr-list->string problems) "))"))]
    [((scimitar-typed e ty))
     (if strip-typed
       (go e)
       (string-append "(: " (go e) " " (ty->string ty) ")"))]
    [((scimitar-var x))
     (symbol->string x)]
    [((scimitar-val v))
     (sym-val->string v #t)]
    [((scimitar-poly p))
     (poly->string p)]
    [((? scimitar-tuple?))
     (define/match (do-tup e)
       [((scimitar-tuple es))
        (let-values ([(es qs) (map-values do-tup es)])
          (values (string-append "(" (expr-list->string es) ")") (ormap identity qs)))]
       [((scimitar-val v))
        (values (sym-val->string v) #f)]
       [((scimitar-num n))
        (values (number->string n) #f)]
       [(_) (values (string-append "," (go e)) #t)])
     (let-values ([(e q) (do-tup e)])
       (string-append (if q "`" "'") e))]
    [((scimitar-switch op e es))
     (let* ((es (map go es))
            (sep (if (null? es) "" " "))
            (cs (map (lambda (i e) (string-append "[(" (number->string i) ") " e "]"))
                     (range (length es)) es)))
       ;; unfortunately, op can't be printed, but it's important to include it
       (string-append "(case " (format "~v" op) " " (go e) sep (expr-list->string cs) ")"))]
    [((scimitar-app f args))
     (define/match (do-args a)
       [((or (scimitar-typed (scimitar-val '()) _) (scimitar-val '()))) ""]
       [((scimitar-typed (scimitar-val (? list? es)) (tuplety tys ...)))
        (string-append " " (expr-list->string (map (compose go scimitar-typed) (map scimitar-val es) tys)))]
       [((scimitar-val (? list? es)))
        (string-append " " (expr-list->string (map (curryr sym-val->string #t) es)))]
       [((or (scimitar-typed (scimitar-tuple es) _) (scimitar-tuple es)))
         (string-append " " (expr-list->string (map go es)))]
       [(_) (string-append " " (go a))])
     (string-append "(" (go f) (do-args args) ")")]
    [((scimitar-lambda ps body))
     (string-append "(lambda " (sym-val->string ps) " " (go body) ")")]
    [((or (scimitar-fix bs b)
          (scimitar-let bs b)
          (scimitar-let* bs b)))
     (let ((op (match e
                 [(? scimitar-fix?) "letrec"]
                 [(? scimitar-let?) "let"]
                 [(? scimitar-let*?) "let*"]))
           (bs (map (lambda (b) (string-append "(" (symbol->string (car b)) " " (go (cadr b)) ")")) bs)))
       (string-append "(" op " (" (expr-list->string bs) ") " (go b) ")"))]
    [((scimitar-begin es))
     (let ((es (cons "begin" (map go es))))
       (string-append "(" (expr-list->string es) ")"))]
    [((scimitar-num n))
     (number->string n)]
    [((or (scimitar-plus e1 e2)
          (scimitar-minus e1 e2)
          (scimitar-mul e1 e2)
          (scimitar-lt e1 e2)
          (scimitar-eq e1 e2)
          (scimitar-gt e1 e2)
          (scimitar-ref e1 e2)))
     (let ((op (match e
                 [(? scimitar-plus?) "+"]
                 [(? scimitar-minus?) "-"]
                 [(? scimitar-mul?) "*"]
                 [(? scimitar-lt?) "<"]
                 [(? scimitar-eq?) "="]
                 [(? scimitar-gt?) ">"]
                 [(? scimitar-ref?) "vec-ref"])))
       (string-append "(" op " " (go e1) " " (go e2) ")"))]
    [((scimitar-if eg et ef))
     (string-append "(if " (go eg) " " (go et) " " (go ef) ")")]
    [((scimitar-for i en em eb))
     (string-append "(for ([" (symbol->string i) " (range " (go en) " " (go em) ")]) " (go eb) ")")]
    [((scimitar-sum i en em eb))
     (string-append "(sum ([" (symbol->string i) " (range " (go en) " " (go em) ")]) " (go eb) ")")]
    [((scimitar-constraint lhs ineq rhs))
     (string-append "(assert (" (symbol->string ineq) " " (go lhs) " " (go rhs) "))")]
    [((scimitar-symbolic))
     "(symbolic)"]
    [((scimitar-infeasible))
     "(assert #f)"])
  (go e))
