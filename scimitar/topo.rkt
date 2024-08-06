#lang racket

(require "contract-utils.rkt")
(require "util.rkt")

(provide
  measurement?
  measurement-dims
  =/on-dims/c
  measurement-exceeds measurement-strictly-exceeds
  measurement-pad
  measurement-volume
  measurement+ measurement- measurement-max measurement-min
  measurement-gen-coords measurement-flatten
  measurement-scaled-L1-norm
  (struct-out coordinate)
  coordinate-compare
  coordinate<
  coordinate+ coordinate- coordinate-max
  coordinate-exceeds
  )

(define measurement?
  (listof natural?))

(define/contract (measurement-dims m)
  (-> measurement? natural?)
  (length m))

(define/contract (=/on-dims/c m)
  (-> measurement? flat-contract?)
  (and/c measurement? ((on/c measurement-dims =/c) m)))

(define/contract ((measurement-exceeds m2) m1)
  (-> measurement? (-> measurement? boolean?))
  (andmap >= m1 m2))

(define/contract ((measurement-strictly-exceeds m2) m1)
  (-> measurement? (-> measurement? boolean?))
  (andmap > m1 m2))

(define/contract (measurement-pad m)
  (-> measurement? measurement?)
  (map add1 m))

(define/contract (measurement-volume m)
  (-> measurement? natural?)
  (apply * m))

(define/contract (measurement+ m1 m2)
  (->i ([m1 measurement?]
        [m2 (m1) (=/on-dims/c m1)])
       [result measurement?])
  (map + m1 m2))

(define/contract (measurement- m1 m2)
  (->i ([m1 measurement?]
        [m2 (m1) (=/on-dims/c m1)])
       [result measurement?])
  (map - m1 m2))

(define/contract (measurement-max m . ms)
  (-> measurement? measurement? ... measurement?)
  (apply map max m ms))

(define/contract (measurement-min m . ms)
  (-> measurement? measurement? ... measurement?)
  (apply map min m ms))

(define/contract (measurement-scaled-L1-norm c1 c2)
  (-> measurement? measurement? natural?)
  (apply + (map * c1 c2)))

(define/contract (measurement-gen-coords m)
  (-> measurement? (listof measurement?))
  ((foldl (lambda (k build)
            (lambda (x)
              (concat
                (build-list k
                  (lambda (i)
                    (build (cons i x)))))))
          list m) '()))

(define/contract (measurement-flatten c s)
  (->i ([c measurement?]
        [s (c) (and/c measurement? (measurement-strictly-exceeds c))])
       [result natural?])
  (measurement-scaled-L1-norm c (init (scanl * 1 s))))

(struct/contract
  coordinate ([xs measurement?])
  #:transparent)

;; Coordinates are in a total order, ascending by dimension
(define/contract (coordinate-compare c1 c2)
  (->i ([c1 coordinate?]
        [c2 (c1) ((on/c coordinate-xs =/on-dims/c) c1)])
       [result (or/c 'LT 'EQ 'GT)])
  (foldr (lambda (x1 x2 acc)
           (case acc
             [(LT GT) acc]
             [(EQ) (compare x1 x2)]))
         'EQ (coordinate-xs c1) (coordinate-xs c2)))

(define (coordinate< c1 c2)
  (define/match (go m1 m2 a)
    [('() '() _) a]
    [(`(,d1 . ,ds1) `(,d2 . ,ds2) _)
     (go ds1 ds2 (or (and a (= d1 d2)) (< d1 d2)))])
  (go (coordinate-xs c1) (coordinate-xs c2) #f))

(define/contract (coordinate+ c1 c2)
  (-> coordinate? coordinate? coordinate?)
  (coordinate (measurement+ (coordinate-xs c1) (coordinate-xs c2))))

(define/contract (coordinate- c1 c2)
  (-> coordinate? coordinate? coordinate?)
  (coordinate (measurement- (coordinate-xs c1) (coordinate-xs c2))))

(define/contract (coordinate-max c . cs)
  (-> coordinate? coordinate? ... coordinate?)
  (coordinate (apply measurement-max (coordinate-xs c) (map coordinate-xs cs))))

(define (coordinate-exceeds c2)
  (and/c coordinate?
         ((on/c coordinate-xs measurement-exceeds) c2)))

(module+ test
  (require rackunit)

  (define m000 '(0 0 0))
  (define m001 '(0 0 1))
  (define m002 '(0 0 2))
  (define m010 '(0 1 0))
  (define m011 '(0 1 1))
  (define m012 '(0 1 2))
  (define m123 '(1 2 3))
  (define m223 '(2 2 3))
  (define m133 '(1 3 3))
  (define m124 '(1 2 4))
  (define m233 '(2 3 3))
  (define m234 '(2 3 4))
  (define m246 '(2 4 6))

  (check-=      (measurement-dims m123) 3 0)
  (check-true   ((measurement-exceeds m123) m123))
  (check-true   ((measurement-exceeds m123) m223))
  (check-true   ((measurement-exceeds m123) m133))
  (check-true   ((measurement-exceeds m123) m124))
  (check-true   ((measurement-exceeds m223) m233))
  (check-true   ((measurement-exceeds m233) m234))
  (check-false  ((measurement-exceeds m223) m123))
  (check-false  ((measurement-exceeds m233) m223))
  (check-false  ((measurement-exceeds m234) m233))
  (check-false  ((measurement-strictly-exceeds m123) m123))
  (check-false  ((measurement-strictly-exceeds m123) m223))
  (check-false  ((measurement-strictly-exceeds m123) m133))
  (check-false  ((measurement-strictly-exceeds m123) m124))
  (check-true   ((measurement-strictly-exceeds m123) m234))
  (check-equal? (measurement-pad m123) m234)
  (check-=      (measurement-volume m234) 24 0)
  (check-equal? (measurement+ m123 m123) m246)
  (check-equal? (measurement- m246 m123) m123)
  (check-equal? (measurement- m123 m123) m000)
  (check-equal? (measurement-max m000 m123 m223 m133 m124) m234)
  (check-equal? (measurement-gen-coords m123) (list m000 m010 m001 m011 m002 m012))
  (check-equal? (measurement-flatten m123 m246) 29)
  )
