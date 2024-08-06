#lang racket

(require "contract-utils.rkt")
(require "util.rkt")
(require "vec.rkt")
(require "poly.rkt")
(require "ty.rkt")

(provide
  ty-inf ty-inf-inf
  ty-sup ty-sup-sup
  ty-measure
  ty-sup-measure
  ty-1)

(define/contract (ty-1 ty)
  (-> ty? any/c)
  (match ty
    [(vecty _ shape) (vec-1 shape)]
    [(tuplety tys ...) (map ty-1 tys)]
    [(unitty) '()]
    [(polyty ty-i shape ty-o) (poly ty-i ty-o `(,(realty (cadr shape))) '() (constraint (vec-1 shape) '<= (vec-1 `(,(car shape)))))]))

(define/contract (ty-inf ty)
  (-> ty? any/c)
  (match ty
    [(vecty (interval inf _) shape) (vec.* inf (vec-1 shape))]
    [(tuplety tys ...) (map ty-inf tys)]
    [(unitty) '()]
    [(polyty ty-i shape ty-o) (poly ty-i ty-o `(,(realty (cadr shape))) '() (constraint (ty-inf (vecty real shape)) '<= (vec.* (interval-inf real) (vec-1 `(,(car shape))))))]))

(define/contract (ty-inf-inf ty)
  (-> ty? number?)
  (match ty
    [(vecty (interval inf _) _) inf]
    [(tuplety tys ...) (apply min (map ty-inf-inf tys))]
    [(unitty) 0]
    [(polyty ty-i shape ty-o) (interval-inf real)]))

(define/contract (ty-sup ty)
  (-> ty? any/c)
  (match ty
    [(vecty (interval _ sup) shape) (vec.* sup (vec-1 shape))]
    [(tuplety tys ...) (map ty-sup tys)]
    [(unitty) '()]
    [(polyty ty-i shape ty-o) (poly ty-i ty-o `(,(realty (cadr shape))) '() (constraint (ty-sup (vecty real shape)) '>= (vec.* (interval-sup real) (vec-1 `(,(car shape))))))]))

(define/contract (ty-sup-sup ty)
  (-> ty? number?)
  (match ty
    [(vecty (interval _ sup) _) sup]
    [(tuplety tys ...) (apply max (map ty-sup-sup tys))]
    [(unitty) 0]
    [(polyty ty-i shape ty-o) (interval-sup real)]))

(define/contract (ty-measure ty)
  (-> ty? any/c)
  (match ty
    [(vecty i shape) (vec.* (interval-measure i) (vec-1 shape))]
    [(tuplety tys ...) (map ty-measure tys)]
    [(unitty) '()]
    [(polyty ty-i shape ty-o) (poly (ty-i ty-o `(,(realty (cadr shape)))) '() (constraint (ty-measure (vecty real shape)) '>= (vec.* (interval-measure real) (vec-1 `(,(car shape))))))]))

(define/contract (ty-sup-measure ty)
  (-> ty? number?)
  (- (ty-sup-sup ty) (ty-inf-inf ty)))
