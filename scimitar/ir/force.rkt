#lang racket

(require "../contract-utils.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide ir-dec-force-thunk)

(define/contract (ir-dec-force-thunk d)
  (-> ir-dec? ir-dec?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (ir-poly-dec f Gamma_f Gamma_r Gamma_l
       (map (curry ir-fmap ir-thunk?
                   (match-lambda [(ir-thunk _ '() force) (force)] [e e])) ss))]))
