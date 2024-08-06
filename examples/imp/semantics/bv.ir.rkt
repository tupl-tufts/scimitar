#lang s-exp scimitar/ir

(require scimitar/ty)
(require (lib "scimitar/prim/op.ir.rkt"))
(require "ba.ir.rkt")
(require "bv-helpers.rkt")

(provide
  bitwidth
  bv? int->bv bv->int gen-all-bvs
  bv-to-real
  bv-halfadd
  bv-fulladd
  bv-add
  bv-complement
  bv-sub
  bv-le)

(forall ([k (bitwidth)])
  (define
    (bv-to-real ((: v (bitty k))) (: r (realty)))
    (locals
      (: sum (realty (+ k 1))))
    (= r (! sum k)
       )
    (= (! sum 0) 0)
    (forall i 0 k
      (= (! sum (+ i 1)) (+ (! sum i) (.* (eval (expt 2 i)) (! v i)))))))

(define
  (bv-halfadd ((: b1 (bitty)) (: b2 (bitty))) (: r (tuplety (bitty) (bitty))))
  (= r `(,((eval ba-xor) b1 b2) ,((eval ba-and) b1 b2))))

(define
  (bv-fulladd ((: b1 (bitty)) (: b2 (bitty)) (: c (bitty))) (: r (tuplety (bitty) (bitty))))
  (locals
    (: b3 (bitty))
    (: c1 (bitty))
    (: bp (bitty))
    (: c2 (bitty))
    (: cp (bitty)))
  (= r `(,bp ,cp))
  (= `(,b3 ,c1)
     (bv-halfadd b1 b2))
  (= `(,bp ,c2)
     (bv-halfadd c  b3))
  (= cp ((eval ba-or) c1 c2)))

(forall ([k (bitwidth)])
  (define
    (bv-add ((: n (bitty k)) (: m (bitty k))) (: r (bitty k)))
    (locals
      (: b (bitty k))
      (: c (bitty (+ 1 k))))
    (= r b)
    (= (! c 0) 0)
    (forall i 0 k
      (= `(,(! b i) ,(! c (+ 1 i)))
         (bv-fulladd (! n i) (! m i) (! c i))))))

(forall ([k (bitwidth)])
  (define
    (bv-complement ((: n (bitty k))) (: r (bitty k)))
    (forall i 0 k
      (= (! r i) (- 1 (! n i))))))

(forall ([k (bitwidth)])
  (define
    (bv-sub ((: n (bitty k)) (: m (bitty k))) (: r (bitty k)))
    (locals
      (: one (bitty k)))
    (= (! one 0) 1)
    (forall i 1 k
      (= (! one i) 0))
    (= r ((eval (bv-add k)) n ((eval (bv-add k)) one ((eval (bv-complement k)) m))))))

(forall ([k (bitwidth)])
  (define
    (bv-le ((: n (bitty k)) (: m (bitty k))) (: le (bitty)))
    (locals
      (: n<ms (bitty k))
      (: n=ms (bitty k))
      (: lt (bitty (+ k 1)))
      (: eq (bitty (+ k 1)))
      (: leq (bitty (+ k 2))))
    (forall i 0 k
      (subject-to
        (locals
          (: n>m (bitty))
          (: n/m (bitty)))
        (= '() ((eval (op-cmp (bitty))) (! n i) (! m i) (! n<ms i) (! n=ms i) n>m n/m))))
    (= 0 (! lt k))
    (forall i 0 k
      (= (! lt i) ((eval ba-or)  (! lt (+ i 1)) (! n<ms i))))
    (= 1 (! eq k))
    (forall i 0 k
      (= (! eq i) ((eval ba-and) (! eq (+ i 1)) (! n=ms i))))
    (= 1 (! leq (+ k 1)))
    (forall i 0 (+ k 1)
      (= (! leq i) ((eval ba-and) (! leq (+ i 1)) ((eval ba-or) (! lt i) (! eq i)))))
    (= le (! leq 0))))
