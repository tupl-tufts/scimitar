#lang racket

(require (for-syntax syntax/stx))
(require (for-syntax syntax/parse))
(require "scimitar/compiler.rkt")
(require "scimitar/translate.rkt")
(require "cps.rkt")
(require "util.rkt")
(require "optomaton.rkt")

(provide
  (rename-out [module-scimitar #%module-begin])
  (except-out (all-from-out racket) #%module-begin) #%app
  (rename-out [scimitar-prog scimitar])
  (rename-out [optimum-ref-prog optimum-ref]))

(begin-for-syntax
  (define (require? stx)
    (syntax-parse stx #:datum-literals (require)
      [(require _ ...) #t]
      [_ #f]))
  (define not-require? (compose not require?))
  (define (syn-filter f stx)
    (filter f (syntax->list stx)))
  )

(begin-for-syntax
  (require racket)
  (require (for-syntax syntax/parse))
  (define-syntax (with-syntax* stx)
    (syntax-parse stx
      [(_ (b bs ...) e) #'(with-syntax (b) (with-syntax* (bs ...) e))]
      [(_ () e) #'e])))

(define-syntax-rule (scimitar-prog e)
  (optomaton-vm
    (cps-compile
      (scimitar-compile
        (scimitar e)))))

(define-syntax-rule (optimum-ref-prog es ...)
  (optomaton-vm
    (cps-compile
      (scimitar-compile
        (optimum-ref es ...)))))

(define-syntax (module-scimitar stx)
  (syntax-parse stx
    [(_ ss ...)
     (with-syntax* ([(rs ...) (syn-filter require? #'(ss ...))]
                    [(es ...) (syn-filter not-require? #'(ss ...))]
                    [(xs ...) (generate-temporaries #'(es ...))]
                    [e (syntax-parse #'(es ...)
                         [() #''()]
                         [(e) #'e]
                         [(_ ...) #'(let* ((xs es) ...) `(,xs ...))])])
       #`(#%module-begin
          rs ...
          (scimitar-prog e)))]))
