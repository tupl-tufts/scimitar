#lang racket

(require racket/struct)
(require (for-syntax syntax/transformer))
(require "contract-utils.rkt")
(require (only-in "params.rkt" omega))
(require "topo.rkt")
(require "util.rkt")

(provide
  (struct-out ty)
  ty-dim ty-shape-equiv?
  ty-<: ty-least-upper-bound ty-greatest-lower-bound
  ty-tree->ty ty->ty-tree ty-list->ty ty->ty-list ty->scalar-ty-list
  ty-scale ty-hull ty-msum
  (except-out (struct-out interval) interval)
  (except-out (struct-out intset) intset)
  (rename-out [interval-ctor interval])
  interval-measure interval-contains
  interval-hull interval-msum interval-prod interval.*
  real nneg int nat zero one bit
  (struct-out vecty)
  vecty-fmap vecty-hull vecty-intersection vecty-max vecty-msum vecty-prod vecty.*
  realty nnegty intty natty bitty
  (except-out (struct-out tuplety) tuplety)
  (rename-out [tuplety-ctor tuplety])
  (struct-out unitty)
  (struct-out polyty)
  scalarty? scalarty->vecty scalarty-interval
  constty? indicatorty?)

(struct
  ty ()
  #:transparent)

(struct/contract
  interval
  ([inf number?]
   [sup number?])
  #:property prop:custom-write
  (make-constructor-style-printer
    (lambda _ 'interval)
    (lambda (self)
      (list (interval-inf self) (interval-sup self))))
  #:property prop:custom-print-quotable 'never
  #:transparent)

(struct/contract
  intset interval
  ()
  #:transparent)

(struct/contract
  vecty ty
  ([interval interval?]
   [shape measurement?])
  #:transparent)

(struct/contract
  unitty ty
  ()
  #:transparent)

(struct/contract
  tuplety ty
  ([tys (*list/c ty? ty? ty?)])
  #:property prop:custom-write
  (make-constructor-style-printer
    (lambda _ 'tuplety)
    (lambda (self)
      (tuplety-tys self)))
  #:property prop:custom-print-quotable 'never
  #:transparent)

(struct/contract
  polyty ty
  ([dom ty?]
   [shape measurement?]
   [cod ty?])
  #:transparent)

;; scalars are modeled as 0 rank vectors
(define (scalarty? ty)
  (and (vecty? ty)
       (null? (vecty-shape ty))))

(define (scalarty->vecty ty)
  (match ty
    [(vecty _ '()) ty]
    [_ (error (format "Expected type castable to scalar type, but got ~v" ty))]))

(define (scalarty-interval ty)
  (vecty-interval (scalarty->vecty ty)))

(define (constty? ty)
  (and (scalarty? ty)
       (= (interval-inf (vecty-interval ty))
          (interval-sup (vecty-interval ty)))))

;; indicator vars are either binary or fixed at 0 or 1
(define (indicatorty? ty)
  (and (scalarty? ty)
       (let ((i (scalarty-interval ty)))
         (and (intset? i)
         (<= 0 (interval-inf i))
         (>= 1 (interval-sup i))))))

(define-match-expander interval-ctor
  (lambda (stx)
    (syntax-case stx ()
      [(_ inf sup) #`(interval inf sup)]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ inf sup) #`(case (range-class inf sup)
                       [(binary integer) (intset inf sup)]
                       [(continuous) (interval (exact->inexact inf)
                                               (exact->inexact sup))])])))

(define/contract (interval-measure i)
  (-> interval? number?)
  (- (interval-sup i) (interval-inf i)))

(define ((interval-contains i2) i1)
  (and (or (not (and (intset? i1) (not (intset? i2))))
           (= (interval-inf i1) (interval-sup i1))
           (= (interval-inf i2) (interval-sup i2)))
       (<= (interval-inf i1) (interval-inf i2))
       (>= (interval-sup i1) (interval-sup i2))))

(define/contract (interval-split f g i . is)
  (-> (-> number? number? ... number?)
      (-> number? number? ... number?)
      interval? interval? ... interval?)
  (interval-ctor
    (apply f (interval-inf i) (map interval-inf is))
    (apply g (interval-sup i) (map interval-sup is))))

(define/contract (interval-hull i . is)
  (-> interval? interval? ... interval?)
  (apply interval-split min max i is))

(define/contract (interval-intersection i . is)
  (-> interval? interval? ... interval?)
  (apply interval-split max min i is))

(define/contract (interval-max i . is)
  (-> interval? interval? ... interval?)
  (apply interval-split max max i is))

(define/contract (interval-msum i . is) ;; Minkowski sum
  (-> interval? interval? ... interval?)
  (apply interval-split + + i is))

(define/contract (interval-prod i1 i2)
  (-> interval? interval? interval?)
  (let ((m1 (* (interval-inf i1) (interval-inf i2)))
        (m2 (* (interval-inf i1) (interval-sup i2)))
        (m3 (* (interval-sup i1) (interval-inf i2)))
        (m4 (* (interval-sup i1) (interval-sup i2))))
    (interval-ctor
      (min m1 m2 m3 m4) (max m1 m2 m3 m4))))

(define/contract (interval.* m i)
  (-> number? interval? interval?)
  (let ((m1 (* m (interval-inf i)))
        (m2 (* m (interval-sup i))))
    (interval-ctor
      (min m1 m2) (max m1 m2))))

;; Predefined intervals
(define-syntax real (make-variable-like-transformer #'(interval (- (omega)) (omega))))
(define-syntax nneg (make-variable-like-transformer #'(interval 0.0 (omega))))
(define-syntax int  (make-variable-like-transformer #'(intset (- (inexact->exact (omega))) (inexact->exact (omega)))))
(define-syntax nat  (make-variable-like-transformer #'(intset 0 (inexact->exact (omega)))))
(define zero (intset 0 0))
(define one  (intset 1 1))
(define bit  (intset 0 1))

;; Type constructor helpers
(begin-for-syntax
  (define (vec-match ival)
    (lambda (stx)
      (syntax-case stx ()
        [(_ d ...) #`(vecty #,ival (list d ...))])))
  (define (vec-ctor ival)
    (lambda (stx)
      (syntax-case stx ()
        [(_ d ...) #`(vecty #,ival (list d ...))]
        [_ #`(lambda shape (vecty #,ival shape))]))))

(define-match-expander realty
  (vec-match #'real)
  (vec-ctor #'real))

(define-match-expander nnegty
  (vec-match #'nneg)
  (vec-ctor #'nneg))

(define-match-expander intty
  (vec-match #'int)
  (vec-ctor #'int))

(define-match-expander natty
  (vec-match #'nat)
  (vec-ctor #'nat))

(define-match-expander bitty
  (vec-match #'bit)
  (vec-ctor #'bit))

(define-match-expander tuplety-ctor
  (lambda (stx)
    (syntax-case stx ()
      [(_ ty ...) #'(tuplety (list ty ...))]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ ty ...) #'(tuplety (list ty ...))]
      [_ #'(lambda tys (tuplety tys))])))

;; Type properties
(define/contract (ty-dim ty)
  (-> ty? exact-integer?)
  (match ty
    [(vecty _ shape) (apply * shape)]
    [(unitty) 1]
    [(tuplety tys) (apply + (map ty-dim tys))]
    [(polyty _ shape _) (apply * shape)]))

(define/contract (ty-shape-equiv? ty1 ty2)
  (-> ty? ty? boolean?)
  (match* (ty1 ty2)
    [((vecty _ '()) (vecty _ s))
     (andmap (curry = 1) s)]
    [((vecty _ s) (vecty _ '()))
     (ty-shape-equiv? ty2 ty1)]
    [((vecty _ `(,d)) (vecty _ s2))
     (= d (measurement-volume s2))]
    [((vecty _ _) (vecty _ `(,d)))
     (ty-shape-equiv? ty2 ty1)]
    [((vecty _ s1) (vecty _ s2))
     (equal? s1 s2)]
    [((unitty) (unitty)) #t]
    [((tuplety t1s) (tuplety t2s))
     (and (= (length t1s) (length t2s))
          (andmap ty-shape-equiv? t1s t2s))]
    [((or (? vecty?) (? tuplety?))
      (or (? tuplety?) (? vecty?)))
     (= (ty-dim ty1) (ty-dim ty2))]
    [((unitty) (vecty _ s))
     (andmap (curry = 1) s)]
    [((? vecty?) (unitty))
     (ty-shape-equiv? ty2 ty1)]
    [((polyty _ s1 _) (polyty _ s2 _))
     (equal? s1 s2)]
    [(_ _) #f]))

(define ty-tree?
  (flat-rec-contract ty-tree
    null? ty?
    (*list/c ty-tree ty-tree ty-tree)))

(define/contract (ty-tree->ty ty)
  (-> ty-tree? ty?)
  (match ty
    ['() (unitty)]
    [(? list?) (tuplety (map ty-tree->ty ty))]
    [ty ty]))

(define/contract (ty->ty-tree ty)
  (-> ty? ty-tree?)
  (match ty
    [(? unitty?) '()]
    [(tuplety tys) (map ty->ty-tree tys)]
    [ty ty]))

(define/contract (ty-list->ty tys)
  (-> (listof ty?) ty?)
  (match tys
    ['() (unitty)]
    [`(,ty) ty]
    [tys (tuplety tys)]))

(define/contract (ty->ty-list ty)
  (-> ty? (listof ty?))
  (match ty
    [(? unitty?) '()]
    [(tuplety tys) tys]
    [ty `(,ty)]))

(define/contract (ty->scalar-ty-list ty)
  (-> ty? (listof vecty?))
  (define (ty->flat-ty-list ty)
    (match ty
      [(tuplety tys)
       (concat (map ty->flat-ty-list tys))]
      [ty `(,ty)]))
  (define (scalarize ty)
    (match ty
      [(unitty) `(,(vecty zero '()))]
      [(vecty i s) (replicate (ty-dim ty) (vecty i '()))]
      [(? polyty?)
       `(,(nnegty))]))
  (concat (map scalarize (ty->flat-ty-list ty))))

(define/contract (ty-<: ty1 ty2)
  (-> ty? ty? boolean?)
  (match* (ty1 ty2)
    [((vecty i1 '()) (vecty i2 _))
     ((interval-contains i1) i2)]
    [((vecty i1 s1) (vecty i2 s2))
     (and ((interval-contains i1) i2)
          ((=/on-dims/c s1) s2)
          ((measurement-exceeds s1) s2))]
    [((unitty) (unitty)) #t]
    [((unitty) _)
     (ty-<: (vecty zero '()) ty2)]
    [(_ (unitty))
     (ty-<: ty1 (vecty zero '()))]
    [((tuplety t1s) (tuplety t2s))
     (and (= (length t1s) (length t2s))
          (andmap ty-<: t1s t2s))]
    [((vecty _ '()) (tuplety t2s))
     (andmap (curry ty-<: ty1) t2s)]
    [((polyty tyi1 _ tyo1) (polyty tyi2 _ tyo2))
     (and (ty-<: tyi2 tyi1)
          (ty-<: tyo1 tyo2))]
    [(_ _) #f]))

(define-syntax-rule (possible? e)
  (with-handlers ([exn:fail? (lambda (_) #f)]) e))

(define/contract (ty-least-upper-bound ta tb)
  (-> ty? ty? ty?)
  (define/match (go t1 t2)
    [((vecty i1 '()) (vecty _ s))
     (vecty-hull (vecty i1 s) t2)]
    [((vecty _ s) (vecty i2 '()))
     (go t2 t1)]
    [((? vecty?) (? vecty?))
     (vecty-hull t1 t2)]
    [((unitty) (unitty))
     (unitty)]
    [((unitty) _)
     (go (vecty zero '()) t2)]
    [(_ (unitty))
     (go t1 (vecty zero '()))]
    [((tuplety t1s) (tuplety t2s))
     #:when (= (length t1s) (length t2s))
     (tuplety (map go t1s t2s))]
    [((vecty _ '()) (tuplety t2s))
     (tuplety (map (curry go t1) t2s))]
    [((tuplety t1s) (vecty _ '()))
     (go t2 t1)]
    [((polyty d1 _ c1) (polyty d2 _ c2))
     #:when (possible? (ty-greatest-lower-bound d1 d2))
     (polyty (ty-greatest-lower-bound d1 d2) '() (go c1 c2))]
    [(_ _) ;; TOP
     (error (format "There is no supertype common to ~v and ~v" ta tb))])
  (go ta tb))

(define/contract (ty-greatest-lower-bound ta tb)
  (-> ty? ty? ty?)
  (define/match (go t1 t2)
    [((vecty _ '()) (vecty i2 _))
     #:when (possible? (vecty-intersection t1 (vecty i2 '())))
     (vecty-intersection t1 (vecty i2 '()))]
    [((? vecty?) (vecty _ '()))
     (go t2 t1)]
    [((? vecty?) (? vecty?))
     #:when (possible? (vecty-intersection t1 t2))
     (vecty-intersection t1 t2)]
    [((unitty) (unitty))
     (unitty)]
    [((unitty) (vecty i2 _))
     #:when ((interval-contains zero) i2)
     (unitty)]
    [((? vecty?) (unitty))
     (go t2 t1)]
    [((tuplety t1s) (tuplety t2s))
     #:when (= (length t1s) (length t2s))
     (tuplety (map go t1s t2s))]
    [((or (unitty) (vecty _ '())) (tuplety t2s))
     (foldr ty-greatest-lower-bound t1 (map (curry go t1) t2s))]
    [((tuplety t1s) (or (unitty) (vecty _ '())))
     (go t2 t1)]
    [((polyty d1 _ c1) (polyty d2 _ c2))
     #:when (possible? (ty-least-upper-bound d1 d2))
     (polyty (ty-least-upper-bound d1 d2) '() (go c1 c2))]
    [(_ _) ;; BOTTOM
     (error (format "There is no subtype common to ~v and ~v" ta tb))])
  (go ta tb))

(define/contract (ty-scale i ty)
  (-> interval? ty? ty?)
  (match ty
    [(vecty iv s) (vecty (interval-prod i iv) s)]
    [(unitty) (unitty)]
    [(tuplety tys) (tuplety (map (curry ty-scale i) tys))]
    [(polyty tyi s tyo) (polyty (ty-scale tyi) s (ty-scale tyo))]))

(define (ty-combine vecty-f t1 t2)
  (define/match (go t1 t2)
    [((? vecty?) (? vecty?)) (vecty-f t1 t2)]
    [((unitty) (unitty)) (unitty)]
    [((tuplety t1s) (tuplety t2s))
     #:when (= (length t1s) (length t2s))
     (tuplety (map go t1s t2s))]
    [((polyty t1i s1 t1o) (polyty t2i s2 t2o))
     ; no idea if this even means anything
     (polyty (go t1i t2i) (map + s1 s2) (go t1o t2o))])
  (go t1 t2))

(define/contract (ty-hull t1 t2)
  (-> ty? ty? ty?)
  (ty-combine vecty-hull t1 t2))

(define/contract (ty-msum t1 t2)
  (-> ty? ty? ty?)
  (ty-combine vecty-msum t1 t2))

(define (vecty-split f-ival f-shape ty . tys)
  (vecty (apply f-ival  (vecty-interval ty) (map vecty-interval tys))
         (apply f-shape (vecty-shape ty)    (map vecty-shape tys))))

(define/contract (vecty-fmap f-ival f-shape ty)
  (-> (-> interval? interval?)
      (-> measurement? measurement?)
      vecty? vecty?)
  (vecty-split f-ival f-shape ty))

(define/contract (vecty-hull ty . tys)
  (-> vecty? vecty? ... vecty?)
  (apply vecty-split interval-hull measurement-max ty tys))

(define/contract (vecty-intersection ty . tys)
  (-> vecty? vecty? ... vecty?)
  (apply vecty-split interval-intersection measurement-min ty tys))

(define/contract (vecty-max ty . tys)
  (-> vecty? vecty? ... vecty?)
  (apply vecty-split interval-max measurement-max ty tys))

(define/contract (vecty-msum ty . tys) ;; Minkowski sum
  (-> vecty? vecty? ... vecty?)
  (apply vecty-split interval-msum measurement-max ty tys))

(define/contract (vecty-prod ty1 ty2)
  (-> vecty? vecty? vecty?)
  (vecty-split interval-prod measurement-max ty1 ty2))

(define/contract (vecty.* m ty)
  (-> number? vecty? vecty?)
  (vecty-split (curry interval.* m) identity ty))
