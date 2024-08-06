#lang racket

(require (for-syntax syntax/parse))
(require "grammar.rkt")

(provide
  imp-block-translate)

(define-syntax (value-error stx)
  (syntax-case stx ()
    [(_ v)
     #`(error (format "during compilation: ~a:~a~n  Unexpected ~a"
                      #,(syntax-line stx) #,(syntax-column stx) (syntax->datum #'v)))]))

(begin-for-syntax
  (define-literal-set imp-lits
    #:datum-literals (+ - * = <= true false not and or skip := if while) ()))

(define-syntax (imp-expr-translate stx)
  (syntax-parse stx #:literal-sets (imp-lits)
    [(_ (+ e1 e2)) #'(imp-plus (imp-expr-translate e1) (imp-expr-translate e2))]
    [(_ (- e1 e2)) #'(imp-minus (imp-expr-translate e1) (imp-expr-translate e2))]
    [(_ (<= e1 e2)) #'(imp-le (imp-expr-translate e1) (imp-expr-translate e2))]
    [(_ (= e1 e2)) #'(imp-eq (imp-expr-translate e1) (imp-expr-translate e2))]
    [(_ true)  #'(imp-bool #t)]
    [(_ false) #'(imp-bool #f)]
    [(_ (not q)) #'(imp-not (imp-expr-translate q))]
    [(_ (and q1 q2)) #'(imp-and (imp-expr-translate q1) (imp-expr-translate q2))]
    [(_ (or q1 q2)) #'(imp-or (imp-expr-translate q1) (imp-expr-translate q2))]
    [(_ v) #'(match (syntax->datum #'v)
                     [(? natural? n) (imp-nat n)]
                     [(? symbol? x) (imp-var x)]
                     [_ (value-error v)])]))

(define-syntax (imp-stmt-translate stx)
  (syntax-parse stx #:literal-sets (imp-lits)
    [(_ skip) #'(imp-skip)]
    [(_ (:= x e)) #'(imp-assign (syntax->datum #'x) (imp-expr-translate e))]
    [(_ (if q st sf)) #'(imp-if (imp-expr-translate q) (imp-stmt-translate st) (imp-stmt-translate sf))]
    [(_ (while q ss ...)) #'(imp-while (imp-expr-translate q) (imp-block-translate ss ...))]
    [(_ v) #'(value-error v)]))

(define-syntax (imp-block-translate stx)
  (syntax-parse stx #:literal-sets (imp-lits)
    [(_ ss ...)
     #'(list (imp-stmt-translate ss) ...)]))
