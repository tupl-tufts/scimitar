#lang racket

(require math/flonum)
(require "../util.rkt")

(provide
  concat      ; reexport
  pad-to/null ; reexport
  range-class ; reexport
  calc-starts
  maxlen
  pad-to
  lower-bound
  upper-bound
  bound-constraint
  bound-rhs
  group-elements)

(define (calc-starts xs)
  (if (null? xs)
    '()
    (scanl + 0 (map length xs))))

(define (maxlen . xs)
  (apply max (map length xs)))

(define (pad-to n v xs)
  (if (null? xs)
    '()
    (pad-to/null n v xs)))

(define/match (lower-bound c rhs)
  [('<= _) -max.0] [('= b) b] [('>= b) b])

(define/match (upper-bound c rhs)
  [('<= b) b] [('= b) b] [('>= _) +max.0])

(define (bound-constraint rL rU)
  (cond
    [(=  rL rU)      '=]
    [(<= rL -max.0) '<=]
    [(>= rU +max.0) '>=]
    [else #f]))

(define (bound-rhs rL rU)
  (cond
    [(=  rL rU)     rU]
    [(<= rL -max.0) rU]
    [(>= rU +max.0) rL]
    [else #f]))

(define (group-elements starts inds vals)
  (let*-values ([(inds ixs vals elems last)
                 (apply values
                   (foldl
                     (match-lambda**
                       [(offset `(,inds ,ixs ,vals ,elems ,prev))
                        (let*-values ([(ind inds) (split-at inds (- offset prev))]
                                      [(val vals) (split-at vals (- offset prev))])
                          (list inds (cons ind ixs)
                                vals (cons val elems)
                                offset))])
                     `(,inds () ,vals () 0)
                     starts))]
                [(ixs) (cdr (reverse (cons inds ixs)))]
                [(elems) (cdr (reverse (cons vals elems)))])
    (values ixs elems)))
