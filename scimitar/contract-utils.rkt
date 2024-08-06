#lang racket/base
(require
 (prefix-in racket: racket/contract)
 (for-syntax racket/base syntax/parse))
(provide
 define/contract
 struct/contract)

;; Enabled if the variable is set at all
(define-for-syntax enable-contracts?
  (and (getenv "SCIMITAR_CONTRACTS") #true))

;; https://github.com/racket/typed-racket/blob/9f192370124dcc3c7beceb352dee48ff65932b05/typed-racket-lib/typed-racket/utils/utils.rkt#L213
(define-syntax define/contract
  (if enable-contracts?
      (make-rename-transformer #'racket:define/contract)
      (lambda (stx)
        (syntax-parse stx
          [(_ head cnt . body)
           (syntax/loc stx (define head . body))]))))

(define-syntax struct/contract
  (if enable-contracts?
      (make-rename-transformer #'racket:struct/contract)
      (lambda (stx)
        (syntax-parse stx
          [(_ head ([field cnt] ...) . options)
           (syntax/loc stx (struct head (field ...) . options))]
          [(_ head super ([field cnt] ...) . options)
           (syntax/loc stx (struct head super (field ...) . options))]))))
