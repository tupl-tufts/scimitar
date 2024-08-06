#lang racket

(require "contract-utils.rkt")
(require (except-in "elab.rkt" constraint?))
(require "env.rkt")
(require "poly.rkt")
(require "ty.rkt")
(require "util.rkt")
(require (only-in "vm.rkt" lp?))
(require "cps/alpha.rkt")
(require "cps/closure.rkt")
(require "cps/lower.rkt")
(require "cps/grammar.rkt")
(require "cps/elab.rkt")
(require "cps/elim-escaping.rkt")
(require "cps/elim-redirects.rkt")
(require "cps/elim-unused.rkt")
(require "cps/flatten.rkt")
(require "cps/vm.rkt")

(provide
  cps-compile)

(define/contract (cps-compile e)
  (-> cps-cexp? (list/c (listof (list/c number? symbol? lp?)) lp?))
  (define (addresses xs)
    (enumerate-with
      (lambda (j x)
        (list x j))
      xs))
  (match (let* ((e (cps-alpha e))
                (_ (cps-elab e))
                ;; the next five passes must run in order,
                ;; and e is invalid until after flatten.
                (e (cps-elim-escaping e))
                (e (cps-elim-unused e))
                (e (cps-closure e))
                (e (cps-flatten e))
                (e (cps-elim-redirects e))
                (e (cps-alpha e)))
           e)
    [(and (cps-fix fs cont) e)
     (let* ((Gamma (second (cps-elab e)))
            (addrs (addresses (map car fs))))
       (list
         (map (match-lambda
                [`(,f ,xs ,b)
                 `(,@(swap (assoc f addrs))
                   ,(cps-lower
                        addrs
                        (let* ((G-x (env (tree-preorder-zip xs (tree-map (lambda (_) (fresh-tyvar)) xs))))
                               (ty-x (env->ty G-x))
                               (ty-i (polyty-dom (second (env-assoc f Gamma)))))
                          (env-subst G-x (solve 'CPS `(,(~ ty-x ty-i)))))
                        b))])
              fs)
         (cps-lower addrs (env-empty) cont)))]
    [(? cps-primop?)
     (list '() (cps-lower '() (env-empty) e))]
    ;; The below should be true because e shouldn't be an app or a switch
    [e (error (format "BUG: ~a cannot be in flattened primary expression" e))]))
