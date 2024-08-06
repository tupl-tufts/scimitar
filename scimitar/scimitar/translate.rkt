#lang racket

(require (for-syntax syntax/parse))
(require (for-syntax racket/function))
(require "grammar.rkt")
(require "util.rkt")
(require "../ir/translate.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "../ty.rkt")

(provide
  (rename-out [expr-translate scimitar])
  optimum-ref)

(define-syntax (syn-error stx)
  (syntax-parse stx
    [(_ v)
     #`(error (format "during scimitar compilation: ~a:~a~n  unexpected ~a"
                      #,(syntax-line stx) #,(syntax-column stx) (syntax->datum #'v)))]))

(define-syntax (go-quasiquote stx)
  (syntax-parse stx
    [(_ ((~datum unquote) e)) #'(expr-translate e)]
    [(_ (e es ...))
     #'(scimitar-tuple
         (list (go-quasiquote e)
               (go-quasiquote es) ...))]
    [(_ v) #'(scimitar-val 'v)]))

(define base-ns (make-base-namespace))

(define-syntax-rule (with-base-ns e)
  (parameterize ([current-namespace base-ns]) e))

(define-syntax (expr-translate stx)
  (syntax-parse stx
    #:datum-literals (+ - * : < = > vec-ref tuple-ref
                        case if letrec let let* begin
                        for for* sum sum* range
                        maximize minimize optimum-ref
                        lambda define-ir
                        assert <= >= symbolic
                        racket)
    [(_ (+)) #'(scimitar-num 0)]
    [(_ (+ es ... e))
     #'(foldr scimitar-plus
              (expr-translate e)
              (list (expr-translate es) ...))]
    [(_ (-)) #'(scimitar-num 0)]
    [(_ (- e))
     #'(expr-translate (* -1 e))]
    [(_ (- e es ...))
     #'(scimitar-minus
         (expr-translate e)
         (expr-translate (+ es ...)))]
    [(_ (*)) #'(scimitar-num 1)]
    [(_ (* es ... e))
     #'(foldr scimitar-mul
              (expr-translate e)
              (list (expr-translate es) ...))]
    [(_ (: e ty)) #'(scimitar-typed (expr-translate e) (with-base-ns ty))]
    [(_ (< e1 e2)) #'(scimitar-lt (expr-translate e1) (expr-translate e2))]
    [(_ (= e1 e2)) #'(scimitar-eq (expr-translate e1) (expr-translate e2))]
    [(_ (> e1 e2)) #'(scimitar-gt (expr-translate e1) (expr-translate e2))]
    [(_ (case ep cs ...))
     (with-syntax ([(ds ...)
                    (syntax->list
                      (syntax-parse #'(cs ...) #:datum-literals (else)
                        [([(d) _] ...) ; [else _]
                         #'(d ...)]))]
                   [(es ...) (syntax->list (syntax-parse #'(cs ...) [([_ e] ...) #'(e ...)]))]
                   [(b-vars ...) (syntax->list (datum->syntax stx (eval #'(list ((const (gensym 'b-var)) 'cs) ...))))]
                   [(b-vals ...) (syntax->list (datum->syntax stx (eval #'(list ((const (gensym 'b-val)) 'cs) ...))))])
       #`(scimitar-switch
           (lambda (ty)
             (define-ir
               (select ((: c (intty)) (: b-vals ty) ...) (: r ty))
               (locals
                 (: b-vars (bitty)) ...)
               (= 1 (+ b-vars ...))
               (= 0 (* b-vars       (- c ds))) ...
               (= r (+ (* b-vars b-vals) ...)))
             select)
           (expr-translate ep)
           (list (expr-translate es) ...)))]
    [(_ (if ep et ef))
     #'(scimitar-if
         (expr-translate ep)
         (expr-translate et)
         (expr-translate ef))]
    [(_ (letrec ((bs:id es) ...) e))
     #'(scimitar-fix
         (list `(bs ,(expr-translate es)) ...)
         (expr-translate e))]
    [(_ (let ((bs:id es) ...) e))
     #'(scimitar-let
         (list `(bs ,(expr-translate es)) ...)
         (expr-translate e))]
    [(_ (let* ((bs:id es) ...) e))
     #'(scimitar-let*
         (list `(bs ,(expr-translate es)) ...)
         (expr-translate e))]
    [(_ (begin es ...))
     #'(scimitar-begin
         (list (expr-translate es) ...))]
    [(_ (for ([i:id (range (~optional lower #:defaults ([lower #'0])) upper)]) body))
     #'(scimitar-for 'i
                     (expr-translate lower)
                     (expr-translate upper)
                     (expr-translate body))]
    [(_ (for* ([i:id (range (~optional lower #:defaults ([lower #'0])) upper)] ...) body))
     #'(foldr scimitar-for
              (expr-translate body)
              (list 'i ...)
              (list (expr-translate lower) ...)
              (list (expr-translate upper) ...))]
    [(_ (sum ([i:id (range (~optional lower #:defaults ([lower #'0])) upper)]) body))
     #'(scimitar-sum 'i
                     (expr-translate lower)
                     (expr-translate upper)
                     (expr-translate body))]
    [(_ (sum* ([i:id (range (~optional lower #:defaults ([lower #'0])) upper)] ...) body))
     #'(foldr scimitar-sum
              (expr-translate body)
              (list 'i ...)
              (list (expr-translate lower) ...)
              (list (expr-translate upper) ...))]
    [(_ (optimum-ref (~and res (~or _:id (_:id ...))) solve))
     (syntax-parse #'solve
       [((~and dir (~or minimize maximize))
                         obj (locals:id ...) problems ...+)
        #'(scimitar-solve
            'dir
            'res
            (expr-translate obj)
            (list 'locals ...)
            (list (expr-translate problems) ...))])]
    [(_ (optimum-ref res solve))
     #'(syn-error res)]
    [(_ (assert ((~and ineq (~or <= = >=)) e1 e2)))
     #'(scimitar-constraint
         (expr-translate e1)
         'ineq
         (expr-translate e2))]
    [(_ (assert #f)) #'(scimitar-infeasible)]
    [(_ (symbolic)) #'(scimitar-symbolic)]
    [(_ (racket e))
     #'(match e
         [(? number? v) (scimitar-num v)]
         [(? vector? v) (scimitar-val (vec-dense v))]
         [(? val? v) (scimitar-val v)]
         [(? scimitar-expr? ex) ex]
         [_ (syn-error #'stx)])]
    [(_ var:id) #'(scimitar-var 'var)]
    [(_ num:number) #'(scimitar-num num)]
    [(_ b:boolean) #'(scimitar-num (if b 1 0))]
    ; vecty
    ; introduction #(1 2 3) #(#(100 300) #(200 10))  `#(,a)
    ;              #5(1 2 3 4) ==> #(1 2 3 4 4)
    ; elimination: (vec-ref v ix) where ix :: (unitty) + n + (tuplety n n ...)
    ;              future: projection?
    [(_ (~and v #(_ ...)))
     #`(scimitar-val (vec-dense v))]
    [(_ (vec-ref e1 e2))
     #'(scimitar-ref (expr-translate e1) (expr-translate e2))]
    ; unitty
    ; introduction: '()
    ; tuplety
    ; introduction: '(1 2 . 3) `(,a . ,b)  <-- tuples MUST have a dot, so no '(1)
    ; elimination;  (tuple-ref '(1 2 . 3)
    ; lookup how to force . --- this parses as a cons cell instead of as syntax
    [(_ ((~datum quote) v))
     #'(scimitar-val 'v)]
    [(_ ((~datum quasiquote) e))
     #'(go-quasiquote e)]
    [(_ (tuple-ref e j:nat n:nat))
     #'(let* ((vars (build-list n (lambda _ (scimitar-var (gensym 'j)))))
              (v-n (list-ref vars j)))
         (scimitar-let
           (zip vars (build-list n (lambda _ (expr-translate symbolic))))
           (scimitar-begin
             (scimitar-constraint
               (scimitar-expr-list->scimitar-expr vars)
               '=
               (expr-translate e))
             v-n)))]
    ; polyty
    ; introduction (lambda (...) 2 3)    (define-ir ...)
    ; elimination: (f ...)
    [(_ (lambda (~and p (~or _:id (_:id ...))) body))
     #'(scimitar-lambda 'p (expr-translate body))]
    [(_ (~and e (define-ir _ ...)))
     #'(syn-error e)] ; define-ir is top-level only
    [(_ (f vs ...))
     #'(scimitar-app
         (expr-translate f)
         (scimitar-expr-list->scimitar-expr
           (list (expr-translate vs) ...)))]
    [(_ v) #'(syn-error v)]))

(define-syntax (optimum-ref stx)
  (syntax-parse stx
    [(_ res solve)
     #'(expr-translate (optimum-ref res solve))]))
