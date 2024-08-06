#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../util.rkt")
(require "error.rkt")
(require "grammar.rkt")
(require "var-rename.rkt")

(provide
  ir-dec-elim-st)

(define/contract (ir-stmt-elim-st s)
  (-> ir-stmt? (list/c env? (listof ir-constraint?)))
  (match s
    [(ir-subject-to Gamma_l ss)
     (let ([Gammas-sss (unzip (map ir-stmt-elim-st ss))])
       (if (null? Gammas-sss)
         (list Gamma_l '())
         (list (apply env-union-key Gamma_l (car Gammas-sss)) (concat (cadr Gammas-sss)))))]
    [(? ir-constraint?)
     (list (env-empty) `(,s))]
    [else
      (ir-unexpected "elim st" s)]))

(define/contract (ir-dec-elim-st d)
  (-> ir-dec? ir-dec?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (let* ((Gamma-ss (if (null? ss) `((,(env)) ())
                        (unzip (map ir-stmt-elim-st ss))))
            (Gamma (apply env-union-key (car Gamma-ss)))
            (ss (concat (cadr Gamma-ss))))
       (ir-poly-dec f Gamma_f Gamma_r (env-union-key Gamma_l Gamma) ss))]
    [else
      (ir-unexpected "elim st" d)]))
