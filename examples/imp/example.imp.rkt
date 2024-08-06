#lang s-exp "imp.rkt"

(:= y 0)

(:= x 1)

(if false
    (if true
        skip
        skip)
    skip)

(while (not (= y 5))
  (:= x (+ x y))
  (:= y (+ y 1)))

(:= c (and false true))

(if c
  (:= q 3)
  (:= y 4))

(:= b (= 0 0))
(:= w (+ y y))
(:= y 4)
(:= q 5)
