#lang s-exp "imp.rkt"
(:= y 0)
(:= x 0)
(while (<= y 5)
  (:= x (+ x y))
  (:= y (+ y 1)))
