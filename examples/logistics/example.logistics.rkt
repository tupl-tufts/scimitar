#lang s-exp "logistics.rkt"

(product apples 3 0.50)
(product banana 2 0.25)
(product orange 4 0.75)
(product grapes 2 1.25)

(city smallville
      1000 14000
      (apples 400)
      (orange 600)
      (grapes 50))

(city mediumville
      1200 22000
      (apples 1750)
      (banana 500)
      (orange 3000)
      (grapes 2000))

(city bigville
      900 40000
      (apples 4000)
      (banana 5500)
      (orange 5500)
      (grapes 4000))

(city nowheresville
      100 15000
      (apples 50)
      (orange 40))

(road smallville mediumville 40 20)
(road smallville bigville 60 10)
(road mediumville bigville 30 40)
(road nowheresville smallville 100 0)

(truck mega-rig     0.33 2000)
(truck the-monster  0.32 2000)
(truck big-bertha   0.30 1800)
(truck keep-truckin 0.25 1400)
(truck lonely-road  0.20 600)
(truck dusty-trail  0.19 600)
(truck little-guy   0.16 300)
(truck putt-putt    0.15 300)
