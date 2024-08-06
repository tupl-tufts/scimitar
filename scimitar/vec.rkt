#lang racket

(require "contract-utils.rkt")
(require "util.rkt")
(require "params.rkt")
(require "round.rkt")
(require "topo.rkt")
(require "ty.rkt")

(provide (rename-out [measurement-max shape-max ])
         (rename-out [vec-dense-ctor  vec-dense ])
         (rename-out [vec-sparse-ctor vec-sparse])
         vec-dims/c vec-shape/c vec-shape-equal/c
         vec-e vec-0 vec-n vec-1 I
         vec-scalar vec-scalar? vec-scalar-value
         vecof vec/c
         vec? vec-ty vec-nz vec-shape vec-ixs vec-vs vec! vec-unpack vec-null? vec-length vec-width
         round-vector ; move to round.rkt
         vec-exact-round vec-exact-floor vec-exact-gravitate-down
         vec-dynamic-cast vec-cast
         vec-flatten vec-pad vec-left-pad vec-reshape vec-transpose
         vec-hadamard vec-dot vec-augment vec-project
         vec< vec> vec<= vec>= vec=
         vec+ vec*. vec.* vec* vec-negate vec- vec-max
         vec->string string->vec vec->text text->vec)

(module vec-core racket
(provide (all-defined-out))
(require racket/struct)
(require "contract-utils.rkt")
(require "ty.rkt" "topo.rkt" "util.rkt")

;; A vec, or vector, is stored packed as a dictionary mapping its
;; dimensions to magnitudes, in increasing order of dimension.
;; For efficiency, it's a critical invariant that every vec is normalized.
;; Never construct with vec only vec-sparse and vec-dense.
(struct/contract
  vec ([entries (listof (list/c coordinate? number?))]
       [ty vecty?]
       [nz exact-nonnegative-integer?])
  #:property prop:custom-write
  (make-constructor-style-printer
    (lambda _ 'vec-sparse)
    (lambda (self)
      (list (map-car coordinate-xs (vec-entries self)) (vec-ty self))))
  #:property prop:custom-print-quotable 'never
  #:transparent)

(define/contract (vec-shape v)
  (-> vec? measurement?)
  (vecty-shape (vec-ty v)))

(define/contract (vec-interval v)
  (-> vec? interval?)
  (vecty-interval (vec-ty v)))

(define/contract (vec-dims v)
  (-> vec? natural?)
  (measurement-dims (vec-shape v)))

(define/contract (vec-dims/c c)
  (-> flat-contract? flat-contract?)
  (flat-named-contract
    `(vec-dims/c ,c)
    (and/c vec? (property/c vec-dims c))))

(define/contract (vec-shape/c c)
  (-> flat-contract? flat-contract?)
  (flat-named-contract
    `(vec-shape/c ,c)
    (and/c vec? (property/c vec-shape c))))

(define/contract (vec-shape-equal/c v)
  (-> vec? flat-contract?)
  (flat-named-contract
    `(vec-shape-equal/c ,v)
    (and/c vec? ((on/c vec-shape equal/c) v))))

(define (entries-nonzero? es s nz)
  (= (measurement-volume s) (or nz (length es))))

(define (vec-nonzero? v)
  (= (measurement-volume (vec-shape v)) (vec-nz v)))

(define (vecof p?)
  (and/c vec?
    (lambda (v)
      (and (andmap (lambda (xs) (p? (cadr xs))) (vec-entries v))
           (or (vec-nonzero? v) (p? 0))))))

(define/contract ((ty-exceeds t2) t1)
  (-> vecty? (-> vecty? boolean?))
  (and ((interval-contains (vecty-interval t2)) (vecty-interval t1))
       ((=/on-dims/c (vecty-shape t2)) (vecty-shape t1))
       ((measurement-exceeds (vecty-shape t2)) (vecty-shape t1))))
) (require 'vec-core)

(module vec-sparse-defs racket
(provide strip-zeros sparse-ty round-elements vec-sparse vec-sparse-ctor sparsify vec-transpose)
(require "contract-utils.rkt")
(require "round.rkt" "topo.rkt" "ty.rkt" "util.rkt")
(require (submod ".." vec-core))

(define (increasing-coordinates-inner xs ys)
  (cond
    [(null? ys) #t]
    [(coordinate< (caar xs) (caar ys))
     (increasing-coordinates-inner (cdr xs) (cdr ys))]
    [else #f]))

(define (increasing-coordinates? ys)
  (or (null? ys)
      (increasing-coordinates-inner ys (cdr ys))))

(define (strip-zeros xs)
  (filter (compose not zero? cadr) xs))

(define (flatten-groups xsss)
  (map (lambda (xss) (list (caar xss) (map cadr xss))) xsss))

(define (normalize xs)
  (let* ((m-xs (increasing-coordinates? xs))
         (xs (if m-xs xs (sort xs coordinate< #:key car)))
         (m-xs (or m-xs (increasing-coordinates? xs)))
         (xs (if m-xs xs (map-cadr sum (flatten-groups (group-by car xs))))))
    (strip-zeros xs)))

(define (padded-shape coords null-dims)
  (if (null? coords)
    (make-list null-dims 1)
    (measurement-pad (apply measurement-max (map (compose coordinate-xs car) coords)))))

(define (sparse-ty coords #:nz [nz #f] #:null-dims [null-dims 0] [shape (padded-shape coords null-dims)])
  (let* ((vs (map cadr coords))
         (vs (if (and (not (null? vs)) (entries-nonzero? vs shape nz)) vs (cons 0 vs)))
         (i (interval (apply min vs) (apply max vs))))
    (vecty i shape)))

(define (round-elements cs integral)
  (let ((rounded? (compose (if integral exact? inexact?) cadr))
        (round-val (if integral round-scalar exact->inexact)))
    (if (andmap rounded? cs) cs (strip-zeros (map-cadr round-val cs)))))

(define (sparsify v)
  (vec (vec-entries v) (sparse-ty (vec-entries v) #:nz (vec-nz v) #:null-dims (vec-dims v)) (vec-nz v)))

(define/contract (vec-sparse coords [ty (sparse-ty coords)] [nz #f])
  (->i ([coords (listof (list/c coordinate? number?))])
       ([ty (coords nz) (if (null? coords) vecty? (ty-exceeds (sparse-ty coords #:nz (and (not (unsupplied-arg? nz)) nz))))]
        [nz (or/c boolean? exact-nonnegative-integer?)])
       [result vec?])
  (let* ((cs (normalize coords))
         (cs (round-elements cs (intset? (vecty-interval ty))))
         (nz (or nz (length cs))))
    (vec cs ty nz)))

(define-match-expander vec-sparse-ctor
  (lambda (stx)
    (syntax-case stx ()
      [(_ es)       #'(? vec? (vec (app (curry map-car coordinate-xs) es) _  _))]
      [(_ es ty)    #'(? vec? (vec (app (curry map-car coordinate-xs) es) ty _))]
      [(_ es ty nz) #'(? vec? (vec (app (curry map-car coordinate-xs) es) ty nz))]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ es)    #'(vec-sparse (map-car coordinate es))]
      [(_ es ty) #'(vec-sparse (map-car coordinate es) ty)]
      [_ #'vec-sparse])))

(define/contract (vec-transpose v [order (range (sub1 (vec-dims v)) -1 -1)])
  (->i ([v vec?])
       ([order (v) (and/c (=/on-dims/c (vec-shape v))
                          (listof (<=/c (vec-dims v))))])
       [result vec?])
  (let ((reorder (curryr sort-using order <)))
    (vec-sparse
      (map-car (compose coordinate reorder coordinate-xs) (vec-entries v))
      (vecty-fmap identity reorder (vec-ty v))
      (vec-nz v))))
) (require 'vec-sparse-defs)

(module vec-dense-defs racket
(provide (all-defined-out))
(require "generic/foldable.rkt"
         "generic/ordered-container.rkt")
(require "contract-utils.rkt")
(require (except-in "util.rkt" snoc sum)
         "topo.rkt" "ty.rkt")
(require (submod ".." vec-core) (submod ".." vec-sparse-defs))

(define dense?
  (or/c
    (flat-rec-contract dense
      (vectorof dense #:flat? #t)
      number?)
    (flat-rec-contract dense
      (listof dense)
      number?)))

(define-syntax-rule (dense-case vs base rec)
  (cond
     [(ordered-container? vs) rec]
     [else base]))

(define (dense/c ps?)
  (dense-case ps? ps? (apply list/c (map dense/c ps?))))

;; coordinate order of dense vecs is (inner ... outer), e.g. (column row)
(define (dense-dims vs)
  (letrec ((go (lambda (vs)
                 (dense-case vs
                   '()
                   (snoc (if (null? vs) '() (apply map max (foldable->list (map go vs))))
                         (length vs))))))
    (go vs)))

(define/contract (dense-ty vs)
  (-> dense? vecty?)
  (let* ((s (dense-dims vs))
         (vs (dense-case vs `(,vs) (flatten vs)))
         (vs (if (and (not (null? vs)) (entries-nonzero? vs s (length vs))) vs (cons 0 vs)))
         (i (interval (argmin identity vs) (argmax identity vs))))
    (vecty i s)))

(define/contract (vec-dense xs [ty (dense-ty xs)] [order (range (measurement-dims (vecty-shape ty)))])
  (->i ([xs dense?])
       ([ty (xs order) (let ((ty-xs (dense-ty xs)))
                         (if (unsupplied-arg? order)
                           (ty-exceeds ty-xs)
                           (and/c
                             ((on/c vecty-interval interval-contains) ty-xs)
                             (property/c vecty-shape (and/c (=/on-dims/c order)
                                                            (=/on-dims/c (vecty-shape ty-xs))
                                                            (measurement-exceeds (sort-using (vecty-shape ty-xs) order <))
                                                            (property/c measurement-dims (apply and/c (map >/c order))))))))]
        [order measurement?])
       [result vec?])
  (letrec ((reorder (curryr sort-using order <))
           (go (lambda (xs n ds)
                 (dense-case xs
                   (if (zero? xs)
                     '()
                     `((,(coordinate ds) ,xs)))
                   (if (null? xs)
                     '()
                     (append (go (first xs) 0 (cons n ds))
                             (go (rest xs) (+ 1 n) ds))))))
           (es (go xs 0 '())))
    (vec-transpose (vec es (vecty-fmap identity reorder ty) (length es)) order)))

(define/contract (vec-unpack v [order (range (vec-dims v))])
  (->i ([v vec?])
       ([order (v) (and/c (=/on-dims/c (vec-shape v))
                          (listof (<=/c (vec-dims v))))])
       [result dense?])
  (letrec ((go (lambda (c rs es cont)
                 (match rs
                   ['()         (entry c es cont)]
                   [(cons n rs) (row 0 c n rs es cont)])))
           (row (lambda (i c n rs es cont)
                  (if (= i n)
                    (cont '() es)
                    (go (cons i c) rs es
                      (lambda (x es)
                        (row (add1 i) c n rs es
                             (lambda (xs es)
                               (cont (cons x xs) es))))))))
           (entry (lambda (c es cont)
                    (if (or (null? es) (coordinate< (coordinate c) (caar es)))
                      (cont (if (intset? (vecty-interval (vec-ty v))) 0 0.0) es)
                      (cont (cadar es) (cdr es)))))
           (v^T (vec-transpose v order)))
    (go '() (reverse (vec-shape v^T))
        (vec-entries v^T)
        (lambda (xs es)
          (unless (null? es)
            (error "bug in vec-unpack"))
          xs))))

(define-match-expander vec-dense-ctor
  (lambda (stx)
    (syntax-case stx ()
      [(_ xs)      #'(? vec? (app vec-unpack xs))]
      [(_ xs ty)   #'(? vec? (app (lambda (v) (values (vec-unpack v)   (vec-shape v))) xs ty))]
      [(_ xs ty o) #'(? vec? (app (lambda (v) (values (vec-unpack v o) (vec-shape v))) xs ty))]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ xs ...) #'(vec-dense xs ...)]
      [_ #'vec-dense])))
) (require 'vec-dense-defs)

(define (vec-exact-snap f v)
  (match v
    [(vec cs (vecty (interval inf sup) s) nz)
     (if (andmap (compose exact? cadr) cs)
       v
       (let* ((es (strip-zeros (map-cadr f cs)))
              (nz (length es))
              (inf (f inf))
              (sup (f sup)))
         (vec es (vecty (interval inf sup) s) nz)))]))

(define/contract (vec-exact-round v)
  (-> vec? (vecof exact-integer?))
  (vec-exact-snap exact-round v))

(define/contract (vec-exact-floor v)
  (-> vec? (vecof exact-integer?))
  (vec-exact-snap exact-floor v))

(define/contract (vec-exact-gravitate-down v [epsilon 1e-6])
  (->* (vec?) (real?) (vecof exact-integer?))
  ;; The goal is to shrink v except when it's very close to the integer above
  ;; Behaves like vec-exact-round for values within epsilon of an integer
  ;; Behaves like vec-exact-floor for values outside that range
  ;; Note that here, epsilon is NOT the parameter (epsilon), because epsilon
  ;; could be greater than 1 which causes low numbers to jump up instead of down
  (vec-exact-snap (lambda (e) (exact-floor (+ e epsilon))) v))

(define (trying-to-stuff-range-into-intset? maybe-intset-ty maybe-not-intset-ty)
  (and (intset? (vecty-interval maybe-intset-ty))
       (not (intset? (vecty-interval maybe-not-intset-ty)))))

; needed by vec-dynamic-cast and exported
(define/contract (round-vector v)
  (-> vec? (vecof exact-integer?))
  (match v
    [(vec cs (vecty (interval inf sup) s) _)
     (let* ((cs (round-elements cs #t))
            (nz (length cs))
            (inf (round-scalar inf))
            (sup (round-scalar sup)))
       (vec cs (vecty (interval inf sup) s) nz))]))

(define (approximate v)
  (match v
    [(vec cs (vecty (and i (interval inf sup)) s) _)
     (let* ((do-one (curryr (if (intset? i) round-scalar rationalize) (epsilon)))
            (cs (strip-zeros (map-cadr do-one cs)))
            (nz (length cs))
            (inf (do-one inf))
            (sup (do-one sup))
            (i (vecty-interval (sparse-ty cs s #:nz nz))))
       (when (or (> inf (interval-inf i))
                 (< sup (interval-sup i)))
         (error "BUG in approximate"))
       (vec cs (vecty (interval inf sup) s) nz))]))

(define/contract (vec-dynamic-cast ty v)
  (-> vecty? vec? (or/c vec? #f))
  (or (vec-cast ty v)
      (vec-cast ty (sparsify v))
    (let* ((v (approximate (sparsify v))))
      (or (vec-cast ty v)
        (and (trying-to-stuff-range-into-intset? ty (vec-ty v))
             (andmap near-integer? (vec-vs v))
             (vec-cast ty (round-vector v)))))))

(define/contract (vec-cast ty v)
  (-> vecty? vec? (or/c vec? #f))
  (and ((ty-exceeds (vec-ty v)) ty)
       (not (trying-to-stuff-range-into-intset? ty (vec-ty v)))
       (vec (vec-entries v) ty (vec-nz v))))

(define (vec/c ps?)
  (and/c
    vec?
    (property/c vec-shape (apply list/c (dense-dims ps?)))
    (property/c vec-unpack (dense/c ps?))))

(define/contract (vec-e i [s (measurement-pad i)] [ival bit])
  (->i ([i measurement?])
       ([s (i) (and/c measurement? (measurement-strictly-exceeds i))]
        [ival (and/c interval? (interval-contains bit))])
       [result vec?])
  (vec-sparse
    `((,(coordinate i) 1))
    (vecty ival s)
    1))

(define/contract (vec-0 [s '(0)] [ival zero])
  (->* () (measurement? (and/c interval? (interval-contains zero))) vec?)
  (vec '() (vecty ival s) 0))

(define/contract (vec-n s n [ival (interval n n)])
  (->i ([s measurement?] [n number?])
       ([ival (n) (and/c interval? (interval-contains (interval n n)))])
       [result vec?])
  (if (zero? n)
    (vec-0 s ival)
    (vec
      (let ((m (if (intset? ival) n (exact->inexact n))))
        (map (lambda (c) (list (coordinate c) m))
             (measurement-gen-coords s)))
      (vecty ival s)
      (measurement-volume s))))

(define/contract (vec-1 s [ival one])
  (->* (measurement?) ((and/c interval? (interval-contains one))) vec?)
  (vec-n s 1 ival))

(define/contract (vec-scalar n [d 0])
  (->* (number?) (number?) vec?)
  (if (= n 0)
    (vec-0 (make-list d 1) (interval n n))
    (vec `((,(coordinate (make-list d 0)) ,n))
         (vecty (interval (min 0 n) (max 0 n)) (make-list d 1))
         1)))

(define (vec-scalar? v)
  (and (vec? v) (andmap (curry = 1) (vec-shape v))))

(define/contract (vec-scalar-value v)
  (-> vec-scalar? number?)
  (match v
    [(vec '() _ _) 0]
    [(vec `((,(coordinate (list (? zero?) ...)) ,n)) _ _) n]))

(define/contract (I k [ival bit])
  (->* (natural?) ((and/c interval? (interval-contains bit))) vec?)
  (vec
    (let ((m (if (intset? ival) 1 1.0)))
      (build-list k (lambda (i) `(,(coordinate (list i i)) ,m))))
    (vecty ival `(,k ,k))
    k))

(define/contract (vec-ixs v)
  (-> vec? (listof measurement?))
  (map (compose coordinate-xs car) (vec-entries v)))

(define/contract (vec-vs v)
  (-> vec? (listof number?))
  (map cadr (vec-entries v)))

(define/contract (vec! v c)
  (->i ([v (c) (and/c vec? (property/c vec-shape (measurement-strictly-exceeds c)))]
        [c measurement?])
       [result number?])
  (let ((c (coordinate c)))
    (or (ormap (match-lambda
                 [`(,x ,e) (case (coordinate-compare x c)
                             [(LT) #f] [(EQ) e] [(GT) 0])])
               (vec-entries v))
        0)))

(define/contract (vec-flatten v)
  (-> vec? vec?)
  (match v
    [(vec es ty nz)
     (let-values ([(bases volume) (split-at-right (scanl * 1 (vecty-shape ty)) 1)])
       (vec-sparse
         (map-car
           (lambda (c)
             (coordinate
               (list
                 (measurement-scaled-L1-norm
                   (coordinate-xs c)
                   bases))))
           es)
         (vecty (vecty-interval ty) volume)
         nz))]))

(define/contract (vec-pad v s)
  (->i ([v vec?]
        [s (v) (and/c (=/on-dims/c (vec-shape v))
                      (measurement-exceeds (vec-shape v)))])
       [result vec?])
  (match v
    [(vec cs (vecty i shape) nz)
     (if (equal? s shape)
       v
       (vec cs (vecty (interval-hull zero i) s) nz))]))

(define/contract (vec-left-pad n v)
  (-> natural? (vec-dims/c (=/c 2)) vec?)
  (let-values ([(es ty) (vec-shift 1 n v)])
    (vec es ty (vec-nz v))))

(define/contract (vec-reshape v s)
  (->i ([v vec?]
        [s (v) (and/c measurement? ((on/c measurement-volume equal/c) (vec-shape v)))])
       [result vec?])
  (match v
    [(vec es (vecty i shape) nz)
    (letrec ((v-coords (measurement-gen-coords shape))
             (s-coords (measurement-gen-coords s))
             (coord-assoc (zip v-coords s-coords))
             (go (lambda (es ca)
                   (if (null? es)
                     '()
                     (if (null? ca)
                       (error "bug in vec-reshape or malformed vector")
                       (if (equal? (coordinate-xs (caar es)) (caar ca))
                         (cons (list (coordinate (cadar ca)) (cadar es))
                               (go (cdr es) (cdr ca)))
                         (go es (cdr ca))))))))
      (vec (go es coord-assoc) (vecty i s) nz))]))

(define/contract (vec-null? v)
  (-> vec? boolean?)
  (null? (vec-entries v)))

(define/contract (vec-length v)
  (-> (vec-dims/c (<=/c 2)) natural?)
  (car (vec-shape v)))

(define/contract (vec-width v)
  (-> (vec-dims/c (=/c 2)) natural?)
  (cadr (vec-shape v)))

(define/contract (vec-hadamard v1 v2)
  (->i ([v1 vec?]
        [v2 (v1) (vec-shape-equal/c v1)])
       [result vec?])
  ((curry mergeWith * vecty-prod) v1 v2))

(define/contract (vec-dot v1 v2)
  (-> (vec-dims/c (=/c 1)) (vec-dims/c (=/c 1)) number?)
  (apply + (vec-vs (vec-hadamard v1 v2))))

(define (vec-shift axis offset v)
  (define (make v p l)
    (if (= l 0)
      '()
      (cons (if (= p 0) v 0)
            (make v (- p 1) (- l 1)))))
  (if (= offset 0)
    (values (vec-entries v) (vec-ty v))
    (let* ((es (vec-entries v))
           (ty (vec-ty v))
           (ds (vecty-shape ty))
           (offset (make offset axis (length ds)))
           (ival (vecty-interval ty))
           (interval (if ((interval-contains zero) ival) ival (interval-hull zero ival))))
      (values (map-car (lambda (c) (coordinate (measurement+ offset (coordinate-xs c)))) es)
              (vecty interval (measurement+ offset ds))))))

(define/contract (vec-augment axis v1 . vs)
  (->i ([axis natural?]
        [v1 (axis) (vec-dims/c (>=/c (+ axis 1)))])
       #:rest [vs (axis v1) (listof (and/c (vec-dims/c (>=/c (+ axis 1)))
                                           ((on/c (compose (curryr list-set axis 0) vec-shape) equal/c) v1)))]
       [result vec?])
  (let*-values ([(vs) (cons v1 vs)]
                [(offsets) (init (scanl (lambda (v a) (+ a (list-ref (vec-shape v) axis))) 0 vs))]
                [(ess tys) (map-values (curry vec-shift axis) offsets vs)]
                [(vn) (last vs)]
                [(vs) (init vs)]
                [(nz) (foldr (lambda (v n) (+ n (vec-nz v))) (vec-nz vn) vs)])
    (vec-sparse
      (concat ess)
      (vecty (apply interval-hull (map vecty-interval tys))
                  (vecty-shape (last tys)))
      nz)))

(define/contract (vec-project v from [to (vec-shape v)])
  (->i ([v vec?] [from measurement?])
       ([to (from v) (and/c (=/on-dims/c from)
                            (measurement-exceeds from)
                            (=/on-dims/c (vec-shape v)))])
       [result vec?])
  (vec-sparse
    (map-car
      (curryr coordinate- (coordinate from))
      (filter
        (lambda (e)
          (let ((d (coordinate-xs (car e))))
            (and ((measurement-exceeds from) d)
                 ((measurement-strictly-exceeds d) to))))
        (vec-entries v)))
    (vecty (vec-interval v) (measurement- to from))))

(define/contract (mergeWith g f v1 v2)
  ; it is a precondition that (g 0 0) = 0; if not, this breaks
  (->i ([g (-> number? number? number?)]
        [f (-> vecty? vecty? vecty?)]
        [v1 vec?]
        [v2 (v1) (vec-shape-equal/c v1)])
       [result vec?])
  (letrec
    ((merge
       (lambda (c1 c2)
         (cond
           [(null? c2) (map-cadr (curryr g 0) c1)]
           [(null? c1) (map-cadr (curry  g 0) c2)]
           [else
             (let ((d1 (caar c1)) (m1 (cadar c1))
                   (d2 (caar c2)) (m2 (cadar c2))
                   (c3  (cdr c1)) (c4  (cdr c2)))
              (case (coordinate-compare d1 d2)
                [(LT) (cons (list d1 (g m1  0)) (merge c3 c2))]
                [(EQ) (cons (list d1 (g m1 m2)) (merge c3 c4))]
                [(GT) (cons (list d2 (g  0 m2)) (merge c1 c4))]))]))))
    (let ((cs (merge (vec-entries v1) (vec-entries v2)))
          (ty (f (vec-ty v1) (vec-ty v2))))
      ;This check is too expensive; it takes half the time of all of mergeWith, so it's
      ;disabled.  But if new vec functions are added, this should be turned back on.
      ;(when (not ((ty-exceeds (sparse-ty cs (vec-shape v1))) ty))
      ;  (error "mergeWith was given a bad type merger ~v < ~v" ty (sparse-ty cs (vec-shape v1))))
      (vec-sparse cs ty))))

(define/contract (vec-compare f? v1 v2 check-zero)
  (->i ([f? (-> number? number? boolean?)]
        [v1 vec?]
        [v2 (v1) (vec-shape-equal/c v1)]
        [check-zero boolean?])
       [result boolean?])
  (let* ((inf1 (interval-inf (vec-interval v1)))
         (sup1 (interval-sup (vec-interval v1)))
         (inf2 (interval-inf (vec-interval v2)))
         (sup2 (interval-sup (vec-interval v2)))
         (c1 (f? inf1 inf2))
         (c2 (f? sup1 inf2))
         (c3 (f? inf1 sup2))
         (c4 (f? sup1 sup2))
         (intervals-overlap
           (or (and (> sup2 inf1) (< sup2 sup1))
               (and (> sup1 inf2) (< sup1 sup2))
               (and (= inf1 inf2) (= sup1 sup2) (not (= inf1 sup1))))))
    (if (and (not intervals-overlap) c1 c2 c3 c4)
      #t
      (if (and (not intervals-overlap) (not c1) (not c2) (not c3) (not c4))
        #f
        (vec-null?
          (let ((cmp (lambda (n m) (if (f? n m) 0 1)))
                (ty (apply bitty (vec-shape v1))))
            (if check-zero
              (vec-dense (tree-map cmp (vec-unpack v1) (vec-unpack v2)) ty)
              (mergeWith cmp (lambda _ ty) v1 v2))))))))

(define/contract (vec< v1 v2)
  (-> vec? vec? boolean?)
  (vec-compare < v1 v2 #t))

(define/contract (vec> v1 v2)
  (-> vec? vec? boolean?)
  (vec-compare > v1 v2 #t))

(define/contract (vec<= v1 v2)
  (-> vec? vec? boolean?)
  (vec-compare <= v1 v2 #f))

(define/contract (vec>= v1 v2)
  (-> vec? vec? boolean?)
  (vec-compare >= v1 v2 #f))

(define/contract (vec= v1 v2)
  (-> vec? vec? boolean?)
  (vec-compare = v1 v2 #f))

(define/contract (vec+ v1 . vs)
  (-> vec? vec? ... vec?)
  (foldl (curry mergeWith + vecty-msum) v1 vs))

(define/contract (vec*. v m)
  (-> vec? number? vec?)
  (match v
    [(vec es ty nz)
     (let ((ty (vecty.* m ty)))
       (if (= 0 m)
         (vec-0 (vecty-shape ty) (vecty-interval ty))
         (vec (map-cadr (curryr * m) es) ty nz)))]))

(define/contract (vec.* m v)
  (-> number? vec? vec?)
  (match v
    [(vec es ty nz)
     (let ((ty (vecty.* m ty)))
       (if (= 0 m)
         (vec-0 (vecty-shape ty) (vecty-interval ty))
         (vec (map-cadr (curry  * m) es) ty nz)))]))

(define/contract (vec-negate v)
  (-> vec? vec?)
  (vec (map-cadr (curry  - 0) (vec-entries v))
       (vecty.* -1 (vec-ty v))
       (vec-nz v)))

(define/contract (vec- v1 v2)
  (->i ([v1 vec?]
        [v2 (v1) (vec-shape-equal/c v1)])
       [result vec?])
  (vec+ v1 (vec-negate v2)))

(define/contract (vec-max v1 . vs)
  (-> vec? vec? ... vec?)
  (foldl (curry mergeWith max vecty-max) v1 vs))

(define/contract (vec* v1 v2)
  (->i ([v1 (vec-dims/c (=/c 2))]
        [v2 (v1) (and/c (vec-dims/c (=/c 2))
                        (vec-shape/c (lambda (s)
                                       (= (vec-width v1)
                                          (car s)))))])
       [result (v1 v2) (vec-shape/c (equal/c `(,(vec-length v1) ,(vec-width v2))))])
  (let ((A (vec-entries (vec-transpose v1)))
        (B (vec-entries v2)))
    (define (group-after what which)
      (dropf what (compose (curry = which) second coordinate-xs car)))
    (define/match (outer B)
      [('()) '()]
      [(`((,(coordinate `(,_ ,i)) ,_) . ,_))
       (define/match (inner A)
         [('()) (outer (group-after B i))]
         [(`((,(coordinate `(,_ ,j)) ,_) . ,_))
          (define/match (go val A Btemp)
            [(_ '() _) `((,(coordinate `(,j ,i)) ,val) . ,(inner A))]
            [(_ _ '()) `((,(coordinate `(,j ,i)) ,val) . ,(inner (group-after A j)))]
            [(_ `((,(coordinate `(,ca ,ra)) ,va) . ,Ap)
                `((,(coordinate `(,rb ,cb)) ,vb) . ,Bp))
             (cond
               [(> ra j) `((,(coordinate `(,j ,i)) ,val) . ,(inner A))]
               [(> cb i) `((,(coordinate `(,j ,i)) ,val) . ,(inner (group-after A j)))]
               [(= ca rb) (go (+ val (* va vb)) Ap Bp)]
               [(> ca rb) (go val A  Bp)]
               [(< ca rb) (go val Ap B)])])
          (go 0 A B)])
       (inner A)])
    (vec-sparse (outer B))))

(define/contract (vec->string v)
  (-> vec? string?)
  (define (pad-to s n)
    (string-append
      (apply string-append
        (replicate (- n (string-length s)) " "))
      s))
  (match (vec-shape v)
    ['()
     (~v (vec-unpack v))]
    [`(,n)
     (apply string-append
       (add-between
         (map ~v (vec-unpack v))
         "\n"))]
    [`(,n ,m)
     (let* ((ss (map (curry map ~v) (vec-unpack v)))
            (widths (map (lambda (s) (apply max (map string-length s))) ss))
            (ss-aligned (map (lambda (s w) (map (curryr pad-to w) s)) ss widths)))
        (apply string-append
          (add-between
            (map
              (lambda (s)
                (apply string-append
                  (add-between s " ")))
              (unzip ss-aligned))
            "\n")))]
    [shape
     (let* ((shape-init (init shape))
            (shape-last (last shape))
            (shape-zero (replicate (length shape-init) 0))
            (slices (build-list shape-last
                      (lambda (i)
                        (let ((from (snoc shape-zero i))
                              (to (snoc shape-init (+ 1 i))))
                          (vec-reshape
                            (vec-project v from to)
                            shape-init))))))
       (apply string-append
         (add-between
           (map vec->string slices)
           (apply string-append
             (replicate (length shape-init) "\n")))))]))

(define/contract (string->vec s)
  (-> string? vec?)
  ;; The length of the leftmost branch of the tree
  (define (depth xs)
    (if (or (not (list? xs)) (null? xs))
      0
      (+ 1 (depth (car xs)))))
  ;; gathers the stack entries of the same depth into one list
  (define (collect stack)
    (let*-values ([(d) (depth stack)]
                  [(top stack)
                   (splitf-at stack
                     (lambda (entry)
                       (= (- d 1) (depth entry))))])
      (cons (reverse top) stack)))
  ;; gather the entire stack into a single entry
  (define (finalize stack)
    (if (= (length stack) 1)
      (car stack)
      (finalize (collect stack))))
  (let* ((rows (string-split s "\n"))
         (rows (map (curryr string-split " ") rows))
         (rows (map (curry map string->number) rows))
         (rows (map (curry filter identity) rows)))
    ;; We default to the lowest possible number of dimensions
    ;; So, if all dimensions past the first ones are 1, we exclude them.
    ;; That is, this process can't be a perfect bijection
    (match rows
      ['() (vec-dense 0)] ;; Special case sinse we can't have a -1 rank vector
      [`((,n)) (vec-dense n)] ;; Special case for rank zero
      [_ #:when (andmap (list/c number?) rows) ;; Special case for rank one
       (vec-dense (map only rows))]
      [_ #:when (null? (cdr rows)) ;; Special case for rank two with one row
       (let* ((ty (dense-ty rows))
              (shape (vecty-shape ty))
              (shape (list (cadr shape) (car shape)))
              (ty (vecty (vecty-interval ty) shape)))
         (vec-dense rows ty '(1 0)))]
      [_ (let* ((tensor
                  (finalize
                    (foldl
                      (lambda (row stack)
                        (if (null? row)
                          (collect stack)
                          (cons row stack)))
                      `(,(car rows))
                      (cdr rows))))
                (ty (dense-ty tensor))
                (shape (vecty-shape ty))
                (shape (cons (cadr shape) (cons (car shape) (cddr shape))))
                (ty (vecty (vecty-interval ty) shape))
                (order (range (measurement-dims shape)))
                (order (cons 1 (cons 0 (cddr order)))))
            (vec-dense tensor ty order))])))

(define/contract (vec->text v [eol 0])
  (->* (vec?) (integer?) string?)
  (let* ((bs (vec-unpack v))
         (bs (takef bs (lambda (b) (not (equal? b eol))))))
    (bytes->string/utf-8 (list->bytes bs))))

(define/contract (text->vec str [eol 0] [strlen 50])
  (->* (string?) (integer? integer?) vec?)
  (let* ((bs (bytes->list (string->bytes/utf-8 str)))
         (bs (take (pad-to/null strlen eol bs) strlen)))
    (vec-dense bs (vecty (interval 0 255) `(,strlen)))))

(module+ test
  (require rackunit)
  (define vs (vec-dense-ctor 10.0))
  (define v123 (vec-dense-ctor '(1.0 2.0 3.0)))
  (define v000100 (vec-dense-ctor '(0 0 0 1 0 0)))
  (define v000000 (vec-dense-ctor '(0 0 0 0 0 0)))
  (define v111111 (vec-dense-ctor '(1 1 1 1 1 1)))
  (define v1-1x6  (vec-dense-ctor '((1) (1) (1) (1) (1) (1))))
  (define v1-2x3  (vec-dense-ctor '((1.0 1.0) (1.0) (1.0 1.0))))
  (define v1-3x2  (vec-dense-ctor '((1.0 1.0) (1.0) (1.0 1.0))
                                  (vecty (interval 0.0 1.0) '(3 2)) '(1 0)))
  (define vetc-2x3x4 (vec-dense-ctor '((((1 2) (3 1) (2 3)) ((3 2) (2 1) (1 3)))
                                       (((4 5) (6 4) (5 6)) ((6 5) (5 4) (4 6)))
                                       (((7 8) (9 7) (8 9)) ((9 8) (8 7) (7 9)))
                                       (((1 4) (4 7) (7 1)) ((7 4) (4 1) (1 7))))))
  (check-match v123 (vec-dense-ctor '(1.0 2.0 3.0)))
  (check-match v123 (vec-sparse-ctor '(((0) 1.0) ((1) 2.0) ((2) 3.0))))
  (check-equal? (vec-sparse-ctor '(((0) 1.0) ((1) 2.0) ((2) 3.0)))
                v123)
  (check-equal? (vec-e '(3) '(6))
                v000100)
  (check-equal? (vec-0 '(6))
                v000000)
  (check-equal? (vec-1 '(6))
                v111111)
  (check-equal? (I 3)
                (vec-dense-ctor '((1 0 0)
                                  (0 1 0)
                                  (0 0 1))))
  (check-true (vec? v123))
  (check-equal? (vec-ty v000100)
                (vecty (interval 0 1) '(6)))
  (check-equal? (vec-shape v000100) '(6))
  (check-equal? (vec-ixs v000100)
                '((3)))
  (check-equal? (vec-vs v000100)
                '(1))
  (check-= (vec! v000100 '(3)) 1 0.00000001)
  (check-= (vec! v1-3x2 '(1 1)) 0.0 0.00000001)
  (check-= (vec! v000000 '(5)) 0 0.00000001)
  (check-equal? (vec-unpack vs)
                10.0)
  (check-equal? (vec-unpack v000100)
                '(0 0 0 1 0 0))
  (check-equal? (vec-unpack v1-2x3)
                '((1.0 1.0) (1.0 0.0) (1.0 1.0)))
  (check-true (and (vec-null? v000000)
                   (not (vec-null? v000100))))
  (check-equal? (vec-cast (vecty (interval -100.0 100.0) '()) vs)
                (vec-dense-ctor 10.0 (vecty (interval -100.0 100.0) '())))
  (check-equal? (vec-exact-round (vec-dense-ctor '(11.4 4.6 5.999999)))
                (vec-dense-ctor `(11 5 6)))
  (check-equal? (vec-exact-floor (vec-dense-ctor '(11.4 4.6 5.999999)))
                (vec-dense-ctor `(11 4 5)))
  (check-equal? (vec-exact-gravitate-down (vec-dense-ctor '(11.4 4.6 5.999999)))
                (vec-dense-ctor `(11 4 6)))
  (check-equal? (vec-dynamic-cast (vecty (interval -1000000.0 1000000.0) '(1))
                                  (vec-dense '(-1000000.0000000002) (vecty (interval -1000000.0000000002 -1000000.0000000002) '(1))))
                (vec-dense '(-1000000.0) (vecty (interval -1000000.0 1000000.0) '(1))))
  (check-equal? (vec-dynamic-cast (vecty (interval 1.0 3.0) '(3))
                                  (vec-dense '(1.0 2.0 3.0) (vecty (interval 0.0 4.0) '(3))))
                (vec-dense-ctor '(1.0 2.0 3.0) (vecty (interval 1.0 3.0) '(3))))
  (check-equal? (vec-dynamic-cast (vecty (interval 0 4) '(3)) v123)
                (vec-dense-ctor '(1 2 3) (vecty (interval 0 4) '(3))))
  (check-false (vec-dynamic-cast (vecty (interval 0 3) '(3))
                                 (vec-dense '(1.0 2.5 2.9))))
  (check-false (vec-dynamic-cast (vecty (interval 0.0 2.5) '(3)) v123))
  (check-equal? (vec-cast (vecty (interval -100.0 100.0) '()) vs)
                (vec-dense-ctor 10.0 (vecty (interval -100.0 100.0) '())))
  (check-false (vec-cast (vecty (interval 0.0 1.0) '()) vs))
  (check-equal? (vec-unpack (vec-flatten v1-2x3))
                '(1.0 1.0 1.0 0.0 1.0 1.0))
  (check-equal? (vec-unpack (vec-pad v1-2x3 '(3 4)))
                '((1.0 1.0 0.0)
                  (1.0 0.0 0.0)
                  (1.0 1.0 0.0)
                  (0.0 0.0 0.0)))
  (check-equal? (vec-unpack (vec-left-pad 1 v1-2x3))
                '((0.0 0.0)
                  (1.0 1.0)
                  (1.0 0.0)
                  (1.0 1.0)))
  (check-equal? (vec-unpack (vec-reshape v1-2x3 '(3 2)))
                '((1.0 1.0 1.0) (0.0 1.0 1.0)))
  (check-equal? (vec-transpose v1-2x3)
                v1-3x2)
  (check-equal? (vec-hadamard v123 v123)
                (vec-dense-ctor '(1.0 4.0 9.0)))
  (check-= (vec-dot v123 v123) 14.0 0.00000001)
  (check-equal? (vec-augment 0 v000100 v123)
                (vec-dense-ctor '(0.0 0.0 0.0 1.0 0.0 0.0 1.0 2.0 3.0)))
  (check-equal? (vec-project v123 '(1) '(2))
                (vec-dense-ctor '(2.0) (vecty (interval 1.0 3.0) '(1))))
  (check-equal? (vec-project v1-2x3 '(1 1) '(2 3))
                (vec-dense-ctor '((0.0) (1.0)) (vecty (interval 0.0 1.0) '(1 2))))
  (check-equal? (vec-project v1-2x3 '(1 1))
                (vec-dense-ctor '((0.0) (1.0)) (vecty (interval 0.0 1.0) '(1 2))))
  (check-true   (vec< v000000 v111111))
  (check-false  (vec< v111111 v111111))
  (check-false  (vec< v000100 v111111))
  (check-true   (vec> v111111 v000000))
  (check-false  (vec> v111111 v111111))
  (check-false  (vec> v111111 v000100))
  (check-true   (vec<= v000000 v111111))
  (check-false  (vec<= v111111 v000000))
  (check-true   (vec<= v111111 v111111))
  (check-false  (vec<= v111111 v000100))
  (check-true   (vec<= v000100 v111111))
  (check-false  (vec>= v000000 v111111))
  (check-true   (vec>= v111111 v000000))
  (check-true   (vec>= v111111 v111111))
  (check-true   (vec>= v111111 v000100))
  (check-false  (vec>= v000100 v111111))
  (check-true   (vec= v111111 v111111))
  (check-true   (vec= v000000 v000000))
  (check-false  (vec= v111111 v000000))
  (check-equal? (vec+ v000100 v111111)
                (vec-dense-ctor '(1 1 1 2 1 1)))
  (check-equal? (vec*. v123 6)
                (vec-dense-ctor '(6.0 12.0 18.0)))
  (check-equal? (vec*. v123 0)
                (vec '() (vecty zero '(3)) 0))
  (check-equal? (vec.* 4 v111111)
                (vec-dense-ctor '(4 4 4 4 4 4)))
  (check-equal? (vec.* 0 v111111)
                (vec '() (vecty zero '(6)) 0))
  (check-equal? (vec-negate v123)
                (vec-dense-ctor '(-1.0 -2.0 -3.0)))
  (check-equal? (vec- v111111 v000100)
                (vec-dense-ctor '(1 1 1 0 1 1)))
  (check-equal? (vec-max v111111 v000100)
                (vec-dense-ctor '(1 1 1 1 1 1)))
  (check-equal? (vec-max v000000 v000100)
                (vec-dense-ctor '(0 0 0 1 0 0)))
  (check-equal? (vec* v1-2x3 v1-3x2)
                (vec-dense-ctor '((3.0 2.0) (2.0 2.0))))
  (check-equal? (vec* v1-3x2 v1-2x3)
                (vec-dense-ctor '((2.0 1.0 2.0) (1.0 1.0 1.0) (2.0 1.0 2.0))))
  (check-equal? (string->vec (vec->string vs))
                vs)
  (check-equal? (string->vec (vec->string v123))
                v123)
  (check-equal? (string->vec (vec->string v000100))
                v000100)
  (check-equal? (string->vec (vec->string v1-1x6))
                v1-1x6)
  (check-equal? (string->vec (vec->string v1-2x3))
                v1-2x3)
  (check-equal? (string->vec (vec->string v1-3x2))
                v1-3x2)
  (check-equal? (string->vec (vec->string vetc-2x3x4))
                vetc-2x3x4)
  (check-equal? (vec->text (text->vec "test"))
                "test")
  )
