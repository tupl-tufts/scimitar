#lang s-exp "pipes.rkt"

((source a 12) >== 10 ==> b)
(b >== 5 ==> c)
(b >== 5 ==> d)
(c >== 4 ==> (sink e))
(d >== 6 ==> (sink e))
