#lang s-exp scimitar/ir

(require scimitar/ty)

(provide
  real-id real-negate
  real-index real-shift
  real-abs real-max real-min real-bound)

(forall (k)
  (define
    (real-id ((: v (realty k))) (: r (realty k)))
    (= r v)))

(forall (k)
  (define
    (real-negate ((: v (realty k))) (: r (realty k)))
    (= r (- v))))

(forall (i k)
  (define
    (real-index ((: v (realty k))) (: r (realty)))
    (= r (! v i))))

(forall (i k)
  (define
    (real-shift ((: v (realty k))) (: r (realty k)))
    (forall j 0 i
      (= r 0))
    (forall j i k
      (= r (! v (- j i))))))

(forall (k)
  (define
    (real-abs ((: v (realty k))) (: r (nnegty k)))
    (>= r v) (>= r (- v))))

(forall (k)
  (define
    (real-max ((: v (realty k)) (: alpha (realty))) (: r (realty k)))
    (>= r v) (forall i 0 k (>= r alpha))))

(forall (k)
  (define
    (real-min ((: v (realty k)) (: alpha (realty))) (: r (realty k)))
    (locals
      (: u (realty k))
      (: beta (realty k)))
    (= r (- ((eval (real-max k)) v alpha)
            ((eval (real-abs k)) u)))
    (= u (- v beta))
    (forall i 0 k
      (= (! beta i) alpha))))

(forall (k)
  (define
    (real-bound ((: v (realty k))) (: r (nnegty k)))
    (>= r 0) (>= r v)))
