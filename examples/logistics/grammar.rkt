#lang racket

(require scimitar/contract-utils)

(provide
  (struct-out product)
  (struct-out city)
  (struct-out road)
  (struct-out truck))

;; Given some products, cities, roads, and trucks, where should warehouses be
;; placed to minimize costs, what each should stock, which trucks should take
;; which routes, and what each should take from a warehouse to a city.
;;
;; Simplifying assumptions are 1) that truck trips are made once a day, and in
;; only one direction; 2) roads are however bidirectional and cost the same in
;; each direction; 3) warehouses are resupplied magically every day, and
;; magically deliver any demanded goods to the city they are in, and 4) cost is
;; per day and is not rolled into the product profit which is the difference
;; between revenue and production costs, and thus fixed.

(define inventory?
  (listof (list/c symbol? natural?)))

(struct/contract
  product
  ([name symbol?]    ;; The product's name
   [size natural?]   ;; How much space one unit of the product takes up
   [profit number?]) ;; The profit made on the product
  #:transparent)

(struct/contract
  city
  ([name symbol?]       ;; The city's name
   [cost number?]       ;; How much it costs to put a warehouse here
   [capacity natural?]  ;; How large that warehouse could be
   [demand inventory?]) ;; How much of what product does this city need
  #:transparent)

(struct/contract
  road
  ([from symbol?]     ;; City name at one end of the road
   [to symbol?]       ;; City name at the other end
   [length positive?] ;; The number of miles to traverse the road
   [cost number?])    ;; Fixed costs (like tolls)
  #:transparent)

(struct/contract
  truck
  ([name symbol?]       ;; The unique designation of this truck
   [cost/mile number?]  ;; The operating costs of this truck
   [capacity natural?]) ;; How much of which products this truck can hold
  #:transparent)
