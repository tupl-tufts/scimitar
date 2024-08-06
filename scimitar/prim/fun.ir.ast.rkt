#lang racket

(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../val.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/util.rkt")

(provide
  fun-app
  fun-rapp
  fun-par
  fun-o
  )

;(forall (f v)
;  (define
;   (fun-app () (: r (polyty-cod (poly-ty f))))
;   (= r (f v))))

(define-syntax-rule (fun-app f v)
  (ir-compile
    (let ((r (gensym 'r-app)))
      (ir-poly-dec
        'fun-app
        (env-empty)
        (env `((,r ,(polyty-cod (poly-ty f)))))
        (env-empty)
        (list
          (ir-constraint
            (ir-var r)
            '=
            (ir-poly-app (ir-poly f) (sym-val->ir-val v))))))
    (env-empty)
    (env-empty number?)
    (env-empty poly?)))

;(forall (f v)
;  (define
;   (fun-rapp ((: x (polyty-dom (poly-ty f)))) (r : (unitty)))
;   (= v (f x))))

(define-syntax-rule (fun-rapp f v)
  (ir-compile
    (let ((r (gensym 'r-rapp)))
      (ir-poly-dec
        'fun-rapp
        (env (list (list 'x (polyty-dom (poly-ty f)))))
        (env `((,r ,(unitty))))
        (env-empty)
        (list
          (ir-constraint
            (sym-val->ir-val v)
            '=
            (ir-poly-app
              (ir-poly f)
              (ir-var 'x))))))
    (env-empty)
    (env-empty number?)
    (env-empty poly?)))

(define (genargs x f)
  (map
    (lambda (ty) (list (gensym x) ty))
    (ty->ty-list (polyty-dom (poly-ty f)))))

;(forall (f g)
;  (define
;     (fun-par ((: x (let ((fdom (polyty-dom (poly-ty f)))
;                          (gdom (polyty-dom (poly-ty g))))
;                      (if (equal? fdom gdom)
;                        fdom
;                        (error (format "in fun-par, expected same domain, but got ~v and ~v" fdom gdom))))))
;              (: r (tuplety (polyty-cod (poly-ty f)) (polyty-cod (poly-ty g)))))
;   (= r '((f x) (g x)))))

(define-syntax-rule (fun-par f g)
  (ir-compile
    (let ((args (genargs 'x f))
          (r (gensym 'r-fan)))
      (ir-poly-dec
        'fun-fan
        (let ((fdom (polyty-dom (poly-ty f)))
              (gdom (polyty-dom (poly-ty g))))
          (if (equal? fdom gdom)
            (env args)
            (error (format "in fun-par, expected same domain, but got ~v and ~v" fdom gdom))))
        (env `((,r ,(tuplety (polyty-cod (poly-ty f))
                             (polyty-cod (poly-ty g))))))
        (env-empty)
        (list
          (ir-constraint
            (ir-var r)
            '=
            (ir-tuple
              (list
                (ir-poly-app (ir-poly f) (map (compose ir-var car) args))
                (ir-poly-app (ir-poly g) (map (compose ir-var car) args))))))))
    (env-empty)
    (env-empty number?)
    (env-empty poly?)))

;(forall (f g)
;  (define
;   (fun-o ((: x (polyty-dom (poly-ty g))))
;      (: r (polyty-cod (poly-ty f))))
;   (= r (f (g x)))))

(define-syntax-rule (fun-o f g)
  (ir-compile
    (let ((gargs (genargs 'x g))
          (r (gensym 'r-o)))
      (ir-poly-dec
        'fun-o
        (env gargs)
        (env `((,r ,(polyty-cod (poly-ty f)))))
        (env-empty)
        (list
         (ir-constraint
          (ir-var r)
          '=
          (ir-poly-app (ir-poly f)
            (ir-poly-app (ir-poly g)
              (ir-val-list->ir-val (map (compose (curryr ir-var) car) gargs))))))))
    (env-empty)
    (env-empty number?)
    (env-empty poly?)))
