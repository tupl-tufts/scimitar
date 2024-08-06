#lang racket

(require (for-syntax syntax/parse))
(require "grammar.rkt")

(provide
  translate-module)

(define-syntax (translate-logistic stx)
  (syntax-parse stx #:datum-literals (product city road truck)
    [(_ (product name size profit))
     #'(product (syntax->datum #'name) size profit)]
    [(_ (city name cost capacity (prod demand) ...))
     #'(city (syntax->datum #'name) cost capacity
             `((,(syntax->datum #'prod) ,demand) ...))]
    [(_ (road from to length cost))
     #'(road (syntax->datum #'from) (syntax->datum #'to) length cost)]
    [(_ (truck name cost capacity))
     #'(truck (syntax->datum #'name) cost capacity)]))

(define-syntax (translate-module stx)
  (syntax-parse stx
    [(_ ss ...)
     #'`(,(translate-logistic ss) ...)]))
