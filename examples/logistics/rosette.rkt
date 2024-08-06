#lang rosette

(require "grammar.rkt")
(require (except-in scimitar/profile #%app))

;; simple:
(define name-sm "simple")
(define cs-sm `(,(city 'downtown 100 10000 '((tacos 11000)))
                ,(city 'uptown 200 2000 '((tacos 1000)))))
(define ts-sm `(,(truck 'foodtruck 0.1 1000)))
(define ps-sm `(,(product 'tacos 1 1.00)))
(define rs-sm `(,(road 'downtown 'uptown 10 9)))

;; example:
(define name-ex "example")
(define cs-ex `(,(city 'smallville    1000 14000 '((apples 400) (banana 0) (orange 600) (grapes 50)))
                ,(city 'mediumville   1200 22000 '((apples 1750) (banana 500) (orange 3000) (grapes 2000)))
                ,(city 'bigville      900 40000 '((apples 4000) (banana 5500) (orange 5500) (grapes 4000)))
                ,(city 'nowheresville 100 15000 '((apples 50) (banana 0) (orange 40) (grapes 0)))))
(define ts-ex `(,(truck 'mega-rig     0.33 2000)
                ,(truck 'the-monster  0.32 2000)
                ,(truck 'big-bertha   0.30 1800)
                ,(truck 'keep-truckin 0.25 1400)
                ,(truck 'lonely-road  0.20 600)
                ,(truck 'dusty-trail  0.19 600)
                ,(truck 'little-guy   0.16 300)
                ,(truck 'putt-putt    0.15 300)))
(define ps-ex `(,(product 'apples 3 0.50)
                ,(product 'banana 2 0.25)
                ,(product 'orange 4 0.75)
                ,(product 'grapes 2 1.25)))
(define rs-ex `(,(road 'smallville 'mediumville 40 20)
                ,(road 'smallville 'bigville 60 10)
                ,(road 'mediumville 'bigville 30 40)
                ,(road 'nowheresville 'smallville 100 0)))

(for ([name `(,name-sm ,name-ex)]
      [cities `(,cs-sm ,cs-ex)]
      [trucks `(,ts-sm ,ts-ex)]
      [products `(,ps-sm ,ps-ex)]
      [roads `(,rs-sm ,rs-ex)])

(println name)

(define-symbolic* profit real?)

(time-expr problem-definition
  (let ()

(define-syntax-rule (define-symbolic-tensor* name type? dims)
  (define name
    (let ()
      (define (make-symbolic-tensor ds)
        (if (null? (cdr ds))
          (begin (define-symbolic* name type? #:length (car ds)) name)
          (build-list (car ds) (lambda _ (make-symbolic-tensor (cdr ds))))))
      (make-symbolic-tensor dims))))

(define (lists-ref l ds)
  (if (null? (cdr ds))
    (list-ref l (car ds))
    (lists-ref (list-ref l (car ds)) (cdr ds))))

(define-symbolic-tensor* has-warehouse boolean? `(,(length cities)))
(define-symbolic-tensor* takes-road boolean? `(2 ,(length roads) ,(length trucks)))
(define-symbolic-tensor* truck-product-stock real? `(,(length products) ,(length trucks)))
(define-symbolic-tensor* city-product-stock real? `(,(length products) ,(length cities)))
(define-symbolic-tensor* city-product-supply real? `(,(length products) ,(length cities)))
(define-symbolic-tensor* truck-cost real? `(,(length trucks)))
(define-symbolic-tensor* city-profit real? `(,(length cities)))
(define-symbolic-tensor* city-costs real? `(,(length cities)))

(define-syntax-rule (sum i n m b)
  (foldl (lambda (i acc) (+ b acc)) 0 (range n m)))

(for ([k (range (length trucks))])
  (for ([i (range (length products))])
    (assert (>= (lists-ref truck-product-stock `(,i ,k)) 0))))

(for ([k (range (length cities))])
  (for ([i (range (length products))])
    (assert (>= (lists-ref city-product-stock `(,i ,k)) 0))
    (assert (>= (lists-ref city-product-supply `(,i ,k)) 0))))

(for ([k (range (length trucks))])
  (assert (>= (list-ref truck-cost k) 0)))

(for ([k (range (length cities))])
  (assert (>= (list-ref city-profit k) 0))
  (assert (>= (list-ref city-costs k) 0)))

(assert (>= profit 0))

;; truck stock
(for ([k (range (length trucks))])
  (define-symbolic* takes-any-road boolean?)
  (define-symbolic* loading real?)
  (define roads-taken (count identity (flatten (map (curry map (curryr list-ref k)) takes-road))))
  (assert (>= loading 0))
  (assert (<= roads-taken 1))
  (assert (equal? takes-any-road (= 1 roads-taken)))
  (assert (<= loading (truck-capacity (list-ref trucks k))))
  (assert (= loading
             (sum i 0 (length products)
               (* (product-size (list-ref products i)) (lists-ref truck-product-stock `(,i ,k))))))
  (for ([i (range (length products))])
    (define-symbolic* product-would-stock real?)
    (assert (>= product-would-stock 0))
    (assert (= (lists-ref truck-product-stock `(,i ,k)) (if takes-any-road product-would-stock 0)))))

;; city stock
(for ([k (range (length cities))])
  (define-symbolic* stock real?)
  (assert (>= stock 0))
  (assert (= stock
             (sum i 0 (length products)
               (* (product-size (list-ref products i)) (lists-ref city-product-stock `(,i ,k))))))
  (assert (<= stock (city-capacity (list-ref cities k))))
  (for ([i (range (length products))])
    (define-symbolic* product-would-stock real?)
    (assert (>= product-would-stock 0))
    (assert (= (lists-ref city-product-stock `(,i ,k))
               (if (list-ref has-warehouse k) product-would-stock 0)))))

;; city supply
(for ([k (range (length cities))])
  (define-symbolic-tensor* city-imports real? `(,(length products)))
  (for ([i (range (length products))])
    (assert (= (lists-ref city-product-supply `(,i ,k))
               (+ (lists-ref city-product-stock `(,i ,k)) (list-ref city-imports i))))
    (assert (<= (lists-ref city-product-supply `(,i ,k))
                (second (list-ref (city-demand (list-ref cities k)) i))))
    (assert (= (list-ref city-imports i)
       (sum j 0 (length trucks)
         (sum h 0 (length roads)
           (+ (if (equal? (city-name (list-ref cities k)) (road-to (list-ref roads h)))
                (- (if (lists-ref takes-road `(0 ,h ,j)) (lists-ref truck-product-stock `(,i ,j)) 0)
                   (if (lists-ref takes-road `(1 ,h ,j)) (lists-ref truck-product-stock `(,i ,j)) 0))
                0)
              (if (equal? (city-name (list-ref cities k)) (road-from (list-ref roads h)))
                (- (if (lists-ref takes-road `(1 ,h ,j)) (lists-ref truck-product-stock `(,i ,j)) 0)
                   (if (lists-ref takes-road `(0 ,h ,j)) (lists-ref truck-product-stock `(,i ,j)) 0))
                0))))))))

;; truck cost
(for ([k (range (length trucks))])
  (define-symbolic-tensor* cost-road real? `(2 ,(length roads)))
  (for ([i (range 2)])
    (for ([j (range (length roads))])
      (assert (>= (lists-ref cost-road `(,i ,j)) 0))
      (assert (= (lists-ref cost-road `(,i ,j))
        (if (lists-ref takes-road `(,i ,j ,k))
          (+ (* (truck-cost/mile (list-ref trucks k))
                (road-length (list-ref roads j)))
             (road-cost (list-ref roads j)))
          0)))))
  (assert (= (list-ref truck-cost k)
             (sum i 0 2 (sum j 0 (length roads) (lists-ref cost-road `(,i ,j)))))))

;; city profit
(for ([k (range (length cities))])
  (assert (= (list-ref city-profit k)
             (sum i 0 (length products)
               (* (product-profit (list-ref products i))
                  (lists-ref city-product-supply `(,i ,k)))))))

;; city cost
(for ([k (range (length cities))])
  (if (list-ref has-warehouse k)
    (assert (= (list-ref city-costs k) (city-cost (list-ref cities k))))
    (assert (= (list-ref city-costs k) 0))))

;; overall profit
(assert (= profit (- (sum i 0 (length cities) (list-ref city-profit i))
                     (sum i 0 (length cities) (list-ref city-costs i))
                     (sum i 0 (length trucks) (list-ref truck-cost i)))))
))

(let ((s (time-expr
           (optimize
             #:maximize `(,profit)
             #:guarantee
             #t
             ))))
  (println (evaluate profit s)))
)
