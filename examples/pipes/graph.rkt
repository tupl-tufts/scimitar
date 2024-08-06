#lang racket

(require scimitar/contract-utils)
(require scimitar/util)
(require "grammar.rkt")

(provide
  junctions
  pipes
  in out)

(define/contract (group xs)
  (-> (listof (cons/c any/c any/c)) (listof (list/c any/c (listof any/c))))
  (map (lambda (xs) (list (caar xs) (map cdr xs)))
       (group-by car (sort xs any<? #:key car))))

(define graph? (listof (list/c symbol? plumbing?)))

(define/contract (junctions g)
  (-> graph? graph?)
  (define (source-sink js)
    (cond
      [(null? js)
       (error "bug: junction list cannot be empty")]
      [(null? (cdr js))
       js]
      [(> 1 (length js))
       (let ((js (filter (or/c sink? source?) js)))
         (cond
           [(null? (cdr js))
            js]
           [(andmap source? js)
            (error "multiple flow rates for source")]
           [else
            (error "node cannot be both source and sink")]))]))
  (map-cadr (compose only source-sink remove-duplicates concat)
            (group (filter (compose junction? cadr) g))))

(define/contract (pipes g)
  (-> graph? graph?)
  (filter (compose pipe? cadr) g))

(define/contract (connection how g j)
  (-> (-> pipe? symbol?) graph? symbol? graph?)
  (filter (compose (and/c pipe? (property/c how j)) cadr) g))

(define (in g j) (connection pipe-output g j))
(define (out g j) (connection pipe-input g j))
