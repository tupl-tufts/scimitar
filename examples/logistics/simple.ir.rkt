#lang s-exp scimitar/ir

(require scimitar/ty)
(provide example)

(forall (
  truck-foodtruck-cost     ; 0.1
  truck-foodtruck-cap      ; 1000

  road-downtown-uptown-len    ; 10
  road-downtown-uptown-cost   ; 9
  road-uptown-downtown-len    ; 10
  road-uptown-downtown-cost   ; 9

  product-tacos-size         ; 1
  product-tacos-profit       ; 1.00

  city-downtown-cost        ; 100
  city-downtown-capacity    ; 10000
  city-downtown-tacos       ; 11000

  city-uptown-cost       ; 200
  city-uptown-capacity   ; 2000
  city-uptown-tacos      ; 1000
  )
  (define
    (example () (: result (realty)))
    (locals
      (: foodtruck-takes-any-road (bitty))

      (: foodtruck-takes-dwn-upw (bitty))
      (: foodtruck-takes-upw-dwn (bitty))

      (: foodtruck-dwn-upw-cost (nnegty))
      (: foodtruck-upw-dwn-cost (nnegty))

      (: foodtruck-cost (nnegty))

      (: dwn-tacos-would-stock (nnegty))
      (: upw-tacos-would-stock (nnegty))

      (: dwn-tacos-stock (nnegty))
      (: upw-tacos-stock (nnegty))

      (: dwn-stock (nnegty))
      (: upw-stock (nnegty))

      (: foodtruck-tacos-would-stock (nnegty))

      (: foodtruck-tacos-stock (nnegty))

      (: foodtruck-loading (nnegty))

      (: dwn-tacos-imports (realty))
      (: upw-tacos-imports (realty))

      (: dwn-tacos-supply (realty))
      (: upw-tacos-supply (realty))

      (: dwn-cost (nnegty))
      (: upw-cost (nnegty))

      (: dwn-has-warehouse (bitty))
      (: upw-has-warehouse (bitty))

      (: dwn-profit (nnegty))
      (: upw-profit (nnegty))

      (: profit (realty))
      )
    (= result profit)

    (>= 1 foodtruck-takes-any-road)

    (= foodtruck-takes-any-road     (+ foodtruck-takes-dwn-upw foodtruck-takes-upw-dwn))

    (= foodtruck-dwn-upw-cost (* foodtruck-takes-dwn-upw (+ (.* truck-foodtruck-cost road-downtown-uptown-len) road-downtown-uptown-cost)))
    (= foodtruck-upw-dwn-cost (* foodtruck-takes-upw-dwn (+ (.* truck-foodtruck-cost road-uptown-downtown-len) road-uptown-downtown-cost)))

    (= foodtruck-cost     (+ foodtruck-dwn-upw-cost foodtruck-upw-dwn-cost))

    (= dwn-tacos-stock (* dwn-has-warehouse dwn-tacos-would-stock))
    (= upw-tacos-stock (* upw-has-warehouse upw-tacos-would-stock))

    (<= dwn-stock city-downtown-capacity)
    (= dwn-stock
       (+ (.* product-tacos-size dwn-tacos-stock)))
    (<= upw-stock city-uptown-capacity)
    (= upw-stock
       (+ (.* product-tacos-size upw-tacos-stock)))

    (= foodtruck-tacos-stock (* foodtruck-takes-any-road foodtruck-tacos-would-stock))

    (<= foodtruck-loading truck-foodtruck-cap)
    (= foodtruck-loading
       (+ (.* product-tacos-size foodtruck-tacos-stock)))

    (= dwn-tacos-imports
      (+ (- (* foodtruck-takes-upw-dwn foodtruck-tacos-stock) (* foodtruck-takes-dwn-upw foodtruck-tacos-stock))))
    (= upw-tacos-imports
      (+ (- (* foodtruck-takes-dwn-upw foodtruck-tacos-stock) (* foodtruck-takes-upw-dwn foodtruck-tacos-stock))))

    (= dwn-tacos-supply (+ dwn-tacos-stock dwn-tacos-imports))
    (= upw-tacos-supply (+ upw-tacos-stock upw-tacos-imports))

    (= dwn-cost (* dwn-has-warehouse city-downtown-cost))
    (= upw-cost (* upw-has-warehouse city-uptown-cost))

    (<= dwn-tacos-supply city-downtown-tacos)
    (<= upw-tacos-supply city-uptown-tacos)

    (= dwn-profit
       (+ (.* product-tacos-profit dwn-tacos-supply)))
    (= upw-profit
       (+ (.* product-tacos-profit upw-tacos-supply)))

    (= profit
       (- (+ dwn-profit
             upw-profit)
          (+ dwn-cost
             upw-cost)
          (+ foodtruck-cost)))))
