#lang s-exp "logistics.rkt"

(product tacos 1 1.00)

(truck foodtruck 0.1 1000)

(city downtown
      100 10000
      (tacos 11000))

(city uptown
      200 2000
      (tacos 1000))

(road downtown uptown 10 9)
