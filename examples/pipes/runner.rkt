#lang racket

;; A runner for example.ir.rkt
(require "example.ir.rkt")
(require scimitar/vm)
(require scimitar/vec)
(require scimitar/poly)
(require (only-in scimitar/profile time-fun))

(define (make-lp poly)
  (let* ((obj (vec-augment 0
                (vec-1 `(,(poly-output-dim poly)))
                (vec-0 `(,(max 0 (poly-body-width poly)))))))
    (lp 'maximize obj poly)))

((time-fun solve) ((time-fun make-lp) net))
