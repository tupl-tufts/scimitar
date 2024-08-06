#lang racket

(require (rename-in "simple.ir.rkt" [example simple-example]))
(require (rename-in "example.ir.rkt" [example example-example]))
(require scimitar/vm)
(require scimitar/vec)
(require scimitar/poly)
(require scimitar/params)
(require (only-in scimitar/profile time-fun))

(preprocess #f)

(define (make-lp poly)
  (let* ((obj (vec-augment 0
                (vec-1 `(,(poly-output-dim poly)))
                (vec-0 `(,(max 0 (poly-body-width poly)))))))
    (lp 'maximize obj poly)))

;; A runner for simple.ir.rkt
((time-fun solve)
   ((time-fun make-lp)
     (simple-example
       0.1 1000 ; foodtruck
       10 9     ; downtown -> uptown
       10 9     ; uptown -> downtown
       1 1.00   ; tacos
       100 10000 11000   ; downtown
       200 2000 1000))) ; uptown

;; A runner for example.ir.rkt
((time-fun solve)
   ((time-fun make-lp)
     (example-example
       0.33 2000 ; mega-rig    
       0.32 2000 ; the-monster 
       0.30 1800 ; big-bertha  
       0.25 1400 ; keep-truckin
       0.20 600  ; lonely-road 
       0.19 600  ; dusty-trail 
       0.16 300  ; little-guy  
       0.15 300  ; putt-putt
       40 20     ; smallville -> mediumville
       40 20     ; mediumville -> smallville
       60 10     ; smallville -> bigville
       60 10     ; bigville -> smallville
       30 40     ; mediumville -> bigville
       30 40     ; bigville -> mediumville
       100 0     ; nowheresville -> smallville
       100 0     ; smallville -> nowheresville
       3 0.50    ; apples
       2 0.25    ; banana
       4 0.75    ; orange
       2 1.25    ; grapes
       1000 14000 400 0 600 50       ; smallville
       1200 22000 1750 500 3000 2000 ; mediumville
       900 40000 4000 5500 5500 4000 ; bigville
       100 15000 50 0 40 0 )))      ; nowheresville
