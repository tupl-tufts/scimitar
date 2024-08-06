#lang s-exp scimitar/ir

(require scimitar/ty)
(require "ba-helpers.rkt")

(provide
  bit? boolean->bit bit->boolean
  ba-not ba-and ba-nand ba-or ba-xor)

(define
  (ba-not ((: b (bitty))) (: r (bitty)))
  (= r (- 1 b)))

(define
  (ba-and ((: b1 (bitty)) (: b2 (bitty))) (: r (bitty)))
  (= r (* b1 b2)))

(define
  (ba-nand ((: b1 (bitty)) (: b2 (bitty))) (: r (bitty)))
  (= r (ba-not (ba-and b1 b2))))

(define
  (ba-or ((: b1 (bitty)) (: b2 (bitty))) (: r (bitty)))
  (= r (- (+ b1 b2) (* b1 b2))))

(define
  (ba-xor ((: b1 (bitty)) (: b2 (bitty))) (: r (bitty)))
  (= r (- (+ b1 b2) (.* 2 (* b1 b2)))))
