#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../util.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- elim-redirects))

(define/contract (elim-redirects e)
  (-> cps-cexp? cps-cexp?)
  (let ((subs (match e
                [(cps-fix fs cont)
                 (foldr (match-lambda*
                          [`((,f ,xs ,(cps-app g ys)) ,subs)
                           #:when (equal? xs ys)
                           (let ((g (second (or (env-assoc g subs) `(#f ,g)))))
                             (if (equal? f g)
                               subs
                               (env-add f g
                                 (env-map-vals
                                   (lambda (v)
                                     (if (equal? f v) g v))
                                   subs))))]
                          [`(,_ ,subs) subs])
                        (env-empty symbol?)
                        fs)]
                [_ (error "During CPS redirect elimination, got unexpected syntax")])))
    ;; also remove dead code
    (foldl subst
           (foldl remove e (env-dom subs))
           (env-dom subs)
           (env-range subs))))
