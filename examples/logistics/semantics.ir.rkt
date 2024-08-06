#lang s-exp scimitar/ir

(require scimitar/ty)
(require "grammar.rkt")

(provide
  (prefix-out logistics-
    city-supply))

(forall (products ; (listof product?)
         roads    ; (listof road?)
         trucks   ; (listof truck?)
         cities)  ; (listof city?)
  (define
    (city-supply ((: takes-road (bitty 2 (length roads) (length trucks)))
                  (: city-product-stock (nnegty (length products) (length cities)))
                  (: truck-product-stock (nnegty (length products) (length trucks)))
                  (: city-product-supply (nnegty (length products) (length cities))))
                 (: unused (unitty)))
    ; city supply
    (forall k 0 (eval (length cities))
      (subject-to
        (locals
          (: city-imports (realty (length products))))
        (forall i 0 (eval (length products))
          (subject-to
            (= (! city-product-supply `(,i ,k))
               (+ (! city-product-stock `(,i ,k)) (! city-imports i)))
            (<= (! city-product-supply `(,i ,k))
                (eval (second (list-ref (city-demand (list-ref cities k)) i))))
            (= (! city-imports i)
               (sum j 0 (eval (length trucks))
                 (sum h 0 (eval (length roads))
                   (+ (.* (eval (if (equal? (city-name (list-ref cities k)) (road-to (list-ref roads h))) 1 0))
                        (- (* (! takes-road `(0 ,h ,j)) (! truck-product-stock `(,i ,j)))
                           (* (! takes-road `(1 ,h ,j)) (! truck-product-stock `(,i ,j)))))
                      (.* (eval (if (equal? (city-name (list-ref cities k)) (road-from (list-ref roads h))) 1 0))
                        (- (* (! takes-road `(1 ,h ,j)) (! truck-product-stock `(,i ,j)))
                           (* (! takes-road `(0 ,h ,j)) (! truck-product-stock `(,i ,j)))))))))))))))
