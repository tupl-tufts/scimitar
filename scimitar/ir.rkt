#lang racket

(require (for-syntax syntax/stx))
(require (for-syntax syntax/parse))
(require "env.rkt")
(require "poly.rkt")
(require "ir/translate.rkt")

(provide
  (rename-out [module-ir #%module-begin])
  (except-out (all-from-out racket) #%module-begin))

(begin-for-syntax
  (define (ir-dec? stx)
    (syntax-parse stx #:datum-literals (define)
      [(define (_ (_ ...) _) _ ...) #t]
      [_ #f]))

  (define (ir-macro? stx)
    (syntax-parse stx #:datum-literals (forall)
      [(forall (_ ...) _) #t]
      [_ #f]))

  (define (ir-require? stx)
    (syntax-parse stx #:datum-literals (require)
      [(require _ ...) #t]
      [_ #f]))

  (define (ir-provide? stx)
    (syntax-parse stx #:datum-literals (provide)
      [(provide _ ...) #t]
      [_ #f]))

  (define (ir-other? stx)
    (syntax-parse stx #:datum-literals (define forall require provide)
      [(define (_ (_ ...) _) _ ...) #f]
      [(forall (_ ...) _) #f]
      [(require _ ...) #f]
      [(provide _ ...) #f]
      [_ #t]))

  (define ir-poly-env-sym (string->unreadable-symbol "phi")))

(define-syntax (module-ir stx)
  (syntax-parse stx
    [(_ ss ...)
     (with-syntax ([(rs ...) (filter ir-require? (syntax->list #'(ss ...)))]
                   [(ps ...) (filter ir-provide? (syntax->list #'(ss ...)))]
                   [(ms ...) (filter ir-macro? (syntax->list #'(ss ...)))]
                   [(ds ...) (filter ir-dec? (syntax->list #'(ss ...)))]
                   [(os ...) (filter ir-other? (syntax->list #'(ss ...)))]
                   [(fs ...) (stx-map ir-dec-name (filter ir-dec? (syntax->list #'(ss ...))))])
       #`(#%module-begin
          ps ...
          rs ...
          (when (not (null? (list #'os ...)))
            (error (format "Unknown syntax ~v" (list (syntax->datum #'os) ...))))
          (define #,ir-poly-env-sym (env-empty poly?))
          (ir-define-syntax #,ir-poly-env-sym ms) ...
          (begin
            (ir-define #,ir-poly-env-sym ds)
            (set! #,ir-poly-env-sym
              (env-add 'fs fs #,ir-poly-env-sym))) ...))]))
