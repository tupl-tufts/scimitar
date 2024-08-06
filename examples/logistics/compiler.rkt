#lang racket

(require "grammar.rkt")
(require "semantics.ir.rkt")
(require scimitar)
(require scimitar/ty)
(require scimitar/util)
(require scimitar/vec)

(provide
  compile-module)

(inline-amount 8)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                        ;;
 ;; Unfortunately this problem lends poorly to composition ;;
 ;; because of the city-supply constraint which requires   ;;
 ;; both products and the takes-road indicators.  Without  ;;
 ;; it, it would decompose nicely into primative `ap`s     ;;
 ;; over primitive `par`.                                  ;;
 ;;                                                        ;;
 ;;             ,------------> truck-profit -------.       ;;
 ;;             |                                  |       ;;
 ;; takes-road -+-> truck-products ---.            |       ;;
 ;;             |                     V            |       ;;
 ;;             `-------------------> city-supply  |       ;;
 ;;                                   ^  |         |       ;;
 ;;                                   |  |         |       ;;
 ;; has-warehouse --> city-products --'  |         |       ;;
 ;;               |                      V         V       ;;
 ;;               `-------------> city-profit ---> profit  ;;
 ;;                                                        ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compile-module p)
  (let*-values ([(products rest) (partition product? p)]
                [(roads rest)    (partition road? rest)]
                [(trucks cities) (partition truck? rest)]
                [(cities) (map (match-lambda
                                 [(city name cost capacity demand)
                                  (city name cost capacity
                                        (map
                                          (lambda (product)
                                            (let ((name (product-name product)))
                                              (match (assoc name demand)
                                                [#f `(,name 0)]
                                                [d d])))
                                          products))])
                               cities)]
                [(names) (nub (append (map city-name cities)
                                      (map road-from roads)
                                      (map road-to roads)))]
                [(name-ix) (curry index-of names)])
    (for ([road roads])
      (let ((found-city
              (lambda (road-endpoint city)
                (equal? (city-name city)
                        (road-endpoint road)))))
        (when (not (ormap (curry found-city road-to) cities))
          (error (format "Road end point ~a not found" (road-to road))))
        (when (not (ormap (curry found-city road-from) cities))
          (error (format "Road start point ~a not found" (road-from road))))))
    (optimum-ref profit
      (maximize profit
                (has-warehouse
                 takes-road
                 truck-product-stock
                 city-product-stock
                 city-product-supply
                 truck-cost
                 city-profit
                 city-cost
                 profit)
        ;; truck stock
        (for ([k (range (racket (length trucks)))])
          (let ((takes-any-road (: (symbolic) (bitty)))
                (loading (: (symbolic) (nnegty))))
            (begin
              (assert (=
                takes-any-road
                (sum ([j (range 2)])
                  (sum ([h (range (racket (length roads)))])
                    (vec-ref (: takes-road (bitty 2 (length roads) (length trucks))) `(,j ,h ,k))))))
              (assert
                (<= loading
                    (vec-ref (racket (vec-dense (map truck-capacity trucks))) k)))
              (assert
                (= loading
                   (sum ([i (range (racket (length products)))])
                     (* (vec-ref (racket (vec-dense (map product-size products))) i)
                        (vec-ref (: truck-product-stock (nnegty (length products) (length trucks))) `(,i ,k))))))
              (for ([i (range (racket (length products)))])
                (let ((product-would-stock (: (symbolic) (nnegty))))
                  (assert (= (vec-ref (: truck-product-stock (nnegty (length products) (length trucks))) `(,i ,k))
                             (if takes-any-road product-would-stock 0))))))))

        ;; city stock
        (for ([k (range (racket (length cities)))])
          (let ((stock (: (symbolic) (nnegty))))
            (begin
              (assert
                (= stock
                   (sum ([i (range (racket (length products)))])
                        (* (vec-ref (racket (vec-dense (map product-size products))) i)
                                     (vec-ref (: city-product-stock (nnegty (length products) (length cities))) `(,i ,k))))))
              (assert
                (<= stock
                    (vec-ref (racket (vec-dense (map city-capacity cities))) k)))
              (for ([i (range (racket (length products)))])
                (let ((product-would-stock (: (symbolic) (nnegty))))
                  (assert (= (vec-ref (: city-product-stock (nnegty (length products) (length cities))) `(,i ,k))
                             (if (vec-ref (: has-warehouse (bitty (length cities))) k)
                               product-would-stock 0))))))))

        ;; city supply
        #;(for ([k (range (racket (length cities)))])
          (let ((city-imports (: (symbolic) (realty (length products)))))
            (for ([i (range (racket (length products)))])
              (begin
                (assert
                  (= (vec-ref (: city-product-supply (nnegty (length products) (length cities))) `(,i ,k))
                     (+ (vec-ref (: city-product-stock (nnegty (length products) (length cities))) `(,i ,k))
                        (vec-ref (: city-imports (realty (length products))) i))))
                (assert
                  (<= (vec-ref (: city-product-supply (nnegty (length products) (length cities))) `(,i ,k))
                      (vec-ref (racket (vec-dense (map (compose (curry map second) city-demand) cities))) `(,i ,k))))
                (assert
                  (= (vec-ref (: city-imports (realty (length products))) i)
                     (sum ([j (range (racket (length trucks)))])
                       (sum ([h (range (racket (length roads)))])
                         (+ (if (= (vec-ref (racket (vec-dense (map (compose name-ix city-name) cities))) k)
                                                      (vec-ref (racket (vec-dense (map (compose name-ix road-to) roads))) h))
                              (- (* (vec-ref (: takes-road (bitty 2 (length roads) (length trucks))) `(,0 ,h ,j))
                                    (vec-ref (: truck-product-stock (nnegty (length products) (length trucks))) `(,i ,j)))
                                 (* (vec-ref (: takes-road (bitty 2 (length roads) (length trucks))) `(,1 ,h ,j))
                                    (vec-ref (: truck-product-stock (nnegty (length products) (length trucks))) `(,i ,j))))
                              0)
                            (if (= (vec-ref (racket (vec-dense (map (compose name-ix city-name) cities))) k)
                                                      (vec-ref (racket (vec-dense (map (compose name-ix road-from) roads))) h))
                              (- (* (vec-ref (: takes-road (bitty 2 (length roads) (length trucks))) `(,1 ,h ,j))
                                    (vec-ref (: truck-product-stock (nnegty (length products) (length trucks))) `(,i ,j)))
                                 (* (vec-ref (: takes-road (bitty 2 (length roads) (length trucks))) `(,0 ,h ,j))
                                    (vec-ref (: truck-product-stock (nnegty (length products) (length trucks))) `(,i ,j))))
                              0))))))))))
         ((racket (logistics-city-supply products roads trucks cities))
          takes-road city-product-stock truck-product-stock city-product-supply)

         ;; truck cost
         (for ([k (range (racket (length trucks)))])
           (let ((cost-road (: (symbolic) (nnegty 2 (length roads)))))
             (begin
               (for ([i (range 2)])
                 (for ([j (range (racket (length roads)))])
                   (assert
                     (= (vec-ref (: cost-road (nnegty 2 (length roads))) `(,i ,j))
                        (if (vec-ref (: takes-road (bitty 2 (length roads) (length trucks))) `(,i ,j ,k))
                          (+ (* (vec-ref (racket (vec-dense (map truck-cost/mile trucks))) k)
                                (vec-ref (racket (vec-dense (map road-length roads))) j))
                             (vec-ref (racket (vec-dense (map road-cost roads))) j))
                          0)))))
               (assert
                 (= (vec-ref (: truck-cost (nnegty (length trucks))) k)
                    (sum ([i (range 2)])
                      (sum ([j (range (racket (length roads)))])
                        (vec-ref (: cost-road (nnegty 2 (length roads))) `(,i ,j)))))))))

         ;; city profit
         (for ([k (range (racket (length cities)))])
           (assert
             (= (vec-ref (: city-profit (nnegty (length cities))) k)
                (sum ([i (range (racket (length products)))])
                  (* (vec-ref (racket (vec-dense (map product-profit products))) i)
                     (vec-ref (: city-product-supply (nnegty (length products) (length cities))) `(,i ,k)))))))

         ;; city cost
         (for ([k (range (racket (length cities)))])
           (assert
             (= (vec-ref (: city-cost (nnegty (length cities))) k)
                (if (vec-ref (: has-warehouse (bitty (length cities))) k)
                    (vec-ref (racket (vec-dense (map city-cost cities))) k)
                    0))))

         ;; overall profit
         (assert
           (= profit
              (- (sum ([i (range (racket (length cities)))])
                   (vec-ref (: city-profit (nnegty (length cities))) i))
                 (sum ([i (range (racket (length cities)))])
                   (vec-ref (: city-cost (nnegty (length cities))) i))
                 (sum ([i (range (racket (length trucks)))])
                   (vec-ref (: truck-cost (nnegty (length trucks))) i)))))))))
