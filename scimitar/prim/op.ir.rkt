#lang s-exp "../ir.rkt"

(require "../params.rkt")
(require "../ty.rkt")
(require "../ty-def.rkt")

(provide
  op-cmp op-eq)

(forall (ty)
  (define
    (op-cmp ((: n ty) (: m ty) (: <0 (bitty)) (: =0 (bitty)) (: >0 (bitty)) (: /0 (bitty))) (: unused (unitty)))
    (locals
      (: nr (realty (ty-dim ty)))
      (: mr (realty (ty-dim ty)))
      (: <0prod (bitty (+ 1 (ty-dim ty))))
      (: =0prod (bitty (+ 1 (ty-dim ty))))
      (: >0prod (bitty (+ 1 (ty-dim ty)))))
    (= nr n)
    (= mr m)
    (= (+ <0 =0 >0 /0) 1)
    ;; Hack for zero dimensions, which is always equal,
    ;; otherwise the above criterion can't be satisfied.
    (= (eval (if (= 0 (ty-dim ty)) 0 1)) (! <0prod 0))
    (= 1 (! =0prod 0))
    (= (eval (if (= 0 (ty-dim ty)) 0 1)) (! >0prod 0))
    (= <0 (! <0prod (eval (ty-dim ty))))
    (= =0 (! =0prod (eval (ty-dim ty))))
    (= >0 (! >0prod (eval (ty-dim ty))))
    (forall i 0 (eval (ty-dim ty))
      (subject-to
        (locals
          (: <0_i (bitty))
          (: =0_i (bitty))
          (: >0_i (bitty)))
        (= (+ <0_i =0_i >0_i) 1)
        (>= (* <0_i (- (! mr i) (! nr i) (eval (epsilon)))) 0)
        (=  (* =0_i (- (! nr i) (! mr i))) 0)
        (>= (* >0_i (- (! nr i) (! mr i) (eval (epsilon)))) 0)
        (=  (* <0_i (! <0prod i)) (! <0prod (+ i 1)))
        (=  (* =0_i (! =0prod i)) (! =0prod (+ i 1)))
        (=  (* >0_i (! >0prod i)) (! >0prod (+ i 1)))))))

; a special case of op-cmp, but with fewer variables for efficiency
(forall (ty)
  (define
    (op-eq ((: n ty) (: m ty)) (: =0 (bitty)))
    (locals
      (: kr (realty (ty-dim ty)))
      (: =0prod (bitty (+ 1 (ty-dim ty)))))
    (= kr (- n m))
    (= 1 (! =0prod 0))
    (= =0 (! =0prod (eval (ty-dim ty))))
    (forall i 0 (eval (ty-dim ty))
      (subject-to
        (locals
          (: <0_i (bitty))
          (: =0_i (bitty))
          (: >0_i (bitty)))
        (= (+ <0_i =0_i >0_i) 1)
        (<= (* <0_i (+ (! kr i) (eval (epsilon)))) 0)
        (=  (* =0_i (! kr i)) 0)
        (>= (* >0_i (- (! kr i) (eval (epsilon)))) 0)
        (=  (* =0_i (! =0prod i)) (! =0prod (+ i 1)))))))
