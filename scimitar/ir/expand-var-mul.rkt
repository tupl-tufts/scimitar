#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "error.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  ir-dec-expand-var-mul)

(define/contract (ir-stmt-expand-var-mul s Gamma)
  (-> ir-stmt? env? (list/c env? (listof ir-stmt?)))
  (match s
    [(ir-constraint (? ir-var? x) '= (ir-var-mul y z))
     (let* ((ty-y (cadr (env-assoc (ir-var-name y) Gamma)))
            (ty-z (cadr (env-assoc (ir-var-name z) Gamma)))
            (i-y? (indicatorty? ty-y))
            (y (if i-y? y z))
            (z (if i-y? z y))
            (ty-y (if i-y? ty-y ty-z))
            (ty-z (if i-y? ty-z ty-y)))
       (cond
         [(unitty? ty-z)
          (let ((w (gensym 'w)))
            `(,(env `((,w ,(vecty zero '()))))
              (,(ir-constraint (ir-sub (ir-var w) (ir-cast z (vecty zero '()))) '= (ir-num 0))
               ,(ir-constraint x '= (ir-var-mul y (ir-var w))))))]
         [(scalarty? ty-z)
          `(,(env-empty) (,s))]
         [(vecty? ty-z)
          (let* ((cs (measurement-gen-coords (vecty-shape ty-z)))
                 (ws (map (lambda (_) (gensym 'vec-ix-)) cs))
                 (vws (map ir-var ws))
                 (wty (vecty (vecty-interval ty-z) '()))
                 (us (map (lambda (_) (gensym 'var-mul-)) cs))
                 (vus (map ir-var us))
                 (uty (ty-scale (scalarty-interval ty-y) wty)))
            `(,(env-union-key
                 (env (map (lambda (w) `(,w ,wty)) ws))
                 (env (map (lambda (u) `(,u ,uty)) us)))
              (,@(map (lambda (c vw) (ir-constraint (ir-sub vw (ir-vec-ix z (ir-val-list->ir-val (map ir-num c)))) '= (ir-num 0))) cs vws)
               ,@(map (lambda (vw vu) (ir-constraint vu '= (ir-var-mul y vw))) vws vus)
               ,(ir-constraint (if (null? vus) x (ir-sub x (ir-val-list->ir-val vus))) '= (ir-num 0)))))]
         [(tuplety? ty-z)
          (let* ((wtys (tuplety-tys ty-z))
                 (ws (build-list (length wtys)
                       (lambda (_) (gensym 'tuple-ix-))))
                 (vws (map ir-var ws))
                 (us (map (lambda (_) (gensym 'var-mul-)) ws))
                 (vus (map ir-var us))
                 (utys (map (curry ty-scale (scalarty-interval ty-y)) wtys))
                 (Gamma-w (env (zip ws wtys)))
                 (Gamma-u (env (zip us utys)))
                 (Gamma (env-union-key Gamma-w Gamma-u Gamma))
                 (Gammas-css (unzip (map (lambda (vw vu)
                                           (ir-stmt-expand-var-mul (ir-constraint vu '= (ir-var-mul y vw)) Gamma))
                                         vws vus)))
                 (Gammas (car Gammas-css))
                 (css (cadr Gammas-css))
                 )
            `(,(apply env-union-key Gamma-w Gamma-u Gammas)
              (,(ir-constraint (ir-sub z (ir-tuple vws)) '= (ir-num 0))
               ,@(concat css)
               ,(ir-constraint (ir-sub x (ir-tuple vus)) '= (ir-num 0))))
            )]
         [else (error (format "Expected scalar, vector, or tuple in variable multiplication, but got ~v" ty-z))]))]
    [(? ir-constraint?) `(,(env-empty) (,s))]
    [else
      (ir-unexpected "expand var mul" s)]))

(define/contract (ir-dec-expand-var-mul d)
  (-> ir-dec? ir-dec?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (let* ((Gamma (env-union-key Gamma_r Gamma_f Gamma_l))
            (Gammas-sss (if (null? ss) `((,(env)) ())
                          (unzip (map (curryr ir-stmt-expand-var-mul Gamma) ss))))
            (Gammas (car Gammas-sss))
            (sss (cadr Gammas-sss)))
       (ir-poly-dec f Gamma_f Gamma_r (apply env-union-key Gamma_l Gammas) (concat sss)))]
    [else
      (ir-unexpected "expand var mul" d)]))
